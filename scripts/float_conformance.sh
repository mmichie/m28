#!/usr/bin/env bash
# Float/int conformance scoreboard: compares M28 to live CPython (repr) across
# the numeric spec. Used to drive the int/float type refactor. Run from repo root.
#   bash scripts/float_conformance.sh          # summary + only failures
#   bash scripts/float_conformance.sh -v       # show every case
set -u
M28=./bin/m28
PY=python3
verbose=0
[ "${1:-}" = "-v" ] && verbose=1

exprs=(
  # --- repr / str ---
  'repr(1.0)' 'str(1.0)' 'repr(100.0)' 'repr(0.5)' 'repr(-0.0)' 'repr(1e300)'
  'repr(0.1)' 'str(3.14)' 'repr(1e16)' 'repr(1e17)' 'repr(1.5e-8)'
  # --- type identity ---
  'type(1.0).__name__' 'type(0.5).__name__' 'type(1).__name__' 'type(2**70).__name__'
  'type(4/2).__name__' 'type(3*1.0).__name__' 'type(float(5)).__name__'
  'type(1//1).__name__' 'type(7.0//2).__name__' 'type(2**10).__name__' 'type(10**-1).__name__'
  # --- isinstance ---
  'isinstance(0.5, float)' 'isinstance(0.5, int)' 'isinstance(2, float)' 'isinstance(2, int)'
  'isinstance(True, int)' 'isinstance(1.0, int)'
  # --- true division always float ---
  '4/2' '1/2' '6/3' '7/2'
  # --- floor div / mod keep float when a float is involved ---
  '7.0//2' '7//2' '7.0%2' '7%2' '-7//2' '-7.0//2'
  # --- mixed arithmetic promotes to float ---
  '3*1.0' '3+1.0' '3-1.0' '2+2.0' 'float(3)+2'
  # --- pow ---
  '2**10' '10**-1' '2**0.5' '4**0.5'
  # --- overflow => inf, not bigint ---
  '1e300*1e300' '1e308*10' '2.0**2000'
  # --- int stays exact (bigint), not float ---
  '2**70' '10**30' 'type(10**30).__name__'
  # --- conversions ---
  'int(3.7)' 'float(5)' 'float("1.5")' 'int(3.0)' 'type(int(3.0)).__name__'
  # --- equality / hashing across int and float ---
  '1 == 1.0' '1.0 == 1' 'hash(1) == hash(1.0)' 'hash(2.0) == hash(2)'
  '{1: "a"}[1.0]' 'len({1, 1.0})' '1.0 in {1}'
  # --- round / divmod / abs ---
  'round(2.5)' 'round(3.5)' 'round(2.675, 2)' 'type(round(1.5)).__name__'
  'divmod(7, 2)' 'divmod(7.5, 2)' 'abs(-3.5)' 'type(abs(-5)).__name__'
  # --- bool arithmetic ---
  'True + 1' 'type(True + 1).__name__' 'True + 1.0' 'type(True + 1.0).__name__'
  # --- special floats ---
  "float('inf')" "float('-inf')" "float('nan') != float('nan')"
)

pass=0; fail=0; failures=()
for e in "${exprs[@]}"; do
  want=$($PY -c "print(repr($e))" 2>&1)
  got=$($M28 -e "print(repr($e))" 2>&1 | head -1)
  if [ "$want" = "$got" ]; then
    pass=$((pass+1))
    [ $verbose -eq 1 ] && printf 'OK   %-28s %s\n' "$e" "$got"
  else
    fail=$((fail+1))
    failures+=("$(printf '%-28s want=%-22s got=%s' "$e" "$want" "$got")")
  fi
done

if [ ${#failures[@]} -gt 0 ]; then
  echo "--- FAILURES ---"
  for f in "${failures[@]}"; do echo "FAIL $f"; done
fi
echo "================================"
echo "float conformance: $pass/$((pass+fail)) pass"
