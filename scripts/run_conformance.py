#!/usr/bin/env python3
"""Run the tracked CPython conformance tests against ./bin/m28, parse the
unittest summary, and update tests/cpython/conformance.json in place.

Passing = Ran - failures - errors - skipped - expected_failures - unexpected.
A test with no "Ran N tests" line is blocked (crash / parse error / import).
"""
import json
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
JSON = os.path.join(ROOT, "tests/cpython/conformance.json")
TESTDIR = os.path.join(ROOT, "tests/cpython-source/Lib/test")
LIB = os.path.join(ROOT, "tests/cpython-source/Lib")
M28 = os.path.join(ROOT, "bin/m28")
TIMEOUT = int(os.environ.get("CONF_TIMEOUT", "100"))
DATE = os.environ.get("CONF_DATE", "2026-07-01")

RAN = re.compile(r"^Ran (\d+) tests?", re.M)


def num(text, key):
    m = re.search(key + r"=(\d+)", text)
    return int(m.group(1)) if m else 0


def run_one(name):
    path = os.path.join(TESTDIR, name + ".py")
    env = dict(os.environ, PYTHONPATH=LIB)
    try:
        p = subprocess.run([M28, path], env=env, capture_output=True,
                           text=True, timeout=TIMEOUT)
        out = p.stdout + "\n" + p.stderr
    except subprocess.TimeoutExpired:
        return {"status": "timeout", "note": f"timed out after {TIMEOUT}s"}

    rans = RAN.findall(out)
    if not rans:
        # No unittest summary => crashed / failed to import / parse error.
        low = out.lower()
        if "stack overflow" in low or "goroutine 1 " in low or "runtime.gopanic" in low:
            return {"status": "blocked", "note": "crash: Go stack overflow (deep recursion)"}
        if "mgc.go" in low or "out of memory" in low or "runtime:" in low:
            return {"status": "blocked", "note": "crash: Go runtime error"}
        # Otherwise take the last human-readable line (import/parse/attr error).
        tail = [l for l in out.strip().splitlines()
                if l.strip() and not l.startswith("time=")
                and "/" not in l.split(".go:")[0][:2] and ".go:" not in l]
        note = tail[-1].strip()[:150] if tail else "no output"
        return {"status": "blocked", "note": note}

    run = int(rans[-1])
    if run == 0:
        # File loaded but exposed no unittest cases (usually doctest-based).
        return {"status": "blocked", "note": "0 tests ran (doctest-based or no unittest cases)"}
    fail = num(out, "failures")
    err = num(out, "errors")
    skip = num(out, "skipped")
    xfail = num(out, r"expected failures")
    usucc = num(out, r"unexpected successes")
    passing = run - fail - err - skip - xfail - usucc
    ok = ("\nOK" in out) or re.search(r"^OK\b", out, re.M)
    entry = {"tests_run": run, "passing": passing, "failures": fail,
             "errors": err, "skipped": skip}
    if xfail:
        entry["expected_failures"] = xfail
    if usucc:
        entry["unexpected_successes"] = usucc
    if ok and passing == run - skip and fail == 0 and err == 0:
        entry["status"] = "passing"
    else:
        entry["status"] = "partial"
    return entry


def main():
    d = json.load(open(JSON))
    results = {}
    for tier, tv in d["tiers"].items():
        for name in tv.get("tests", {}):
            r = run_one(name)
            results[name] = r
            summary = (f"{r.get('passing','-')}/{r.get('tests_run','-')}"
                       if "tests_run" in r else r["status"])
            print(f"{name:22} {r['status']:8} {summary}"
                  + (f"  ({r['note']})" if r.get("note") else ""))
            sys.stdout.flush()

    # Merge results into the JSON, preserving notes/source/blocker.
    total_run = total_pass = 0
    fully = []
    highpass = []
    for tier, tv in d["tiers"].items():
        for name, entry in tv["tests"].items():
            r = results[name]
            src = entry.get("source")
            notes = entry.get("notes")
            blocker = entry.get("blocker")
            if r["status"] in ("timeout", "blocked"):
                new = {"status": r["status"]}
                if r["status"] == "blocked":
                    new["blocker"] = r.get("note") or blocker or "unknown"
                else:
                    new["note"] = r.get("note")
            else:
                new = {k: r[k] for k in ("status", "tests_run", "passing",
                                         "failures", "errors", "skipped")}
                for k in ("expected_failures", "unexpected_successes"):
                    if k in r:
                        new[k] = r[k]
                run, pas = r["tests_run"], r["passing"]
                total_run += run
                total_pass += pas
                pct = round(100 * pas / run) if run else 0
                label = f"{name} ({pas}/{run}, {pct}%)"
                if r["status"] == "passing":
                    fully.append(f"{name} ({pas}/{pas})")
                elif pct >= 50:
                    highpass.append(label)
            if notes:
                new["notes"] = notes
            if src:
                new["source"] = src
            tv["tests"][name] = new

    d["last_updated"] = DATE
    d["summary"]["tests_running"] = str(total_run)
    d["summary"]["tests_passing"] = str(total_pass)
    d["summary"]["fully_passing_files"] = sorted(fully)
    d["summary"]["high_pass_files"] = sorted(
        highpass, key=lambda s: -int(re.search(r"(\d+)%", s).group(1)))

    json.dump(d, open(JSON, "w"), indent=2)
    open(JSON, "a").write("\n")
    print(f"\nTOTAL: {total_pass}/{total_run} passing across running files")


if __name__ == "__main__":
    main()
