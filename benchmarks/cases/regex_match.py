# Subsystem: regex matching. M28 runs CPython's re.py wrapper per call, which
# is a known hot spot (~1000x CPython). N is intentionally small so the case
# still finishes in a few seconds; the startup-adjusted ratio is what matters.
import re

pat = re.compile(r"(\d+)-(\w+)")
N = 2500
total = 0
for i in range(N):
    m = pat.match(str(i) + "-abc")
    if m:
        total = total + len(m.group(2))
print(total)
