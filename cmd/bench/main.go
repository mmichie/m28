// Command bench is the macro tier of the M28 performance harness.
//
// It runs each program in benchmarks/cases/ under both CPython and the m28
// binary, records wall-clock time (best-of-N), and reports the slowdown ratio
// m28/CPython per case plus the geometric mean across cases. The "_startup"
// case measures per-process startup so the harness can also report a
// startup-adjusted ratio that isolates steady-state interpreter work.
//
// Results are written to benchmarks/bench.json and diffed against
// benchmarks/baseline.json so a /loop iteration can see regressions and
// improvements at a glance.
//
// Usage:
//
//	go run ./cmd/bench                 # score against ./bin/m28 and python3
//	go run ./cmd/bench -n 5            # 5 runs per case (best-of-5)
//	go run ./cmd/bench -update-baseline
//	go run ./cmd/bench -filter arith   # only cases matching the regex
package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"
)

// errTimeout marks a run that exceeded the per-run timeout (distinct from a
// program error, so the table can show [TIMEOUT] vs [ERROR]).
var errTimeout = errors.New("timeout")

// startupCase is the file name (without extension) whose timing is treated as
// per-interpreter startup overhead and subtracted from every other case.
const startupCase = "_startup"

// Sample is the per-case result of one scoring run.
type Sample struct {
	Case      string  `json:"case"`
	Status    string  `json:"status"` // ok | mismatch | error | timeout
	PyMs      float64 `json:"py_ms"`  // best-of-N wall time
	M28Ms     float64 `json:"m28_ms"`
	PyMedian  float64 `json:"py_median_ms"`
	M28Median float64 `json:"m28_median_ms"`
	Ratio     float64 `json:"ratio"`     // m28_ms / py_ms (best-of-N)
	AdjRatio  float64 `json:"adj_ratio"` // startup-adjusted ratio
	Note      string  `json:"note,omitempty"`
}

// Report is the full output of a scoring run, persisted to bench.json.
type Report struct {
	Timestamp       string   `json:"timestamp"`
	M28Bin          string   `json:"m28_bin"`
	Python          string   `json:"python"`
	Runs            int      `json:"runs"`
	PyStartupMs     float64  `json:"py_startup_ms"`
	M28StartupMs    float64  `json:"m28_startup_ms"`
	GeomeanRatio    float64  `json:"geomean_ratio"`
	GeomeanAdjRatio float64  `json:"geomean_adj_ratio"`
	Cases           []Sample `json:"cases"`
}

func main() {
	var (
		m28Bin   = flag.String("m28", "./bin/m28", "path to the m28 binary")
		python   = flag.String("python", "python3", "path to the CPython interpreter (the baseline)")
		runs     = flag.Int("n", 3, "runs per case; the best (minimum) time is used")
		dir      = flag.String("dir", "benchmarks/cases", "directory of .py benchmark cases")
		out      = flag.String("out", "benchmarks/bench.json", "where to write the JSON report")
		baseline = flag.String("baseline", "benchmarks/baseline.json", "baseline JSON to diff against")
		update   = flag.Bool("update-baseline", false, "after scoring, overwrite the baseline with this run")
		filter   = flag.String("filter", "", "only run cases whose name matches this regexp")
		timeout  = flag.Duration("timeout", 120*time.Second, "per-run timeout")
		jsonOnly = flag.Bool("json", false, "print only the JSON report to stdout")
	)
	flag.Parse()

	var filterRe *regexp.Regexp
	if *filter != "" {
		re, err := regexp.Compile(*filter)
		if err != nil {
			fmt.Fprintf(os.Stderr, "invalid -filter regexp: %v\n", err)
			os.Exit(2)
		}
		filterRe = re
	}

	cases, err := filepath.Glob(filepath.Join(*dir, "*.py"))
	if err != nil || len(cases) == 0 {
		fmt.Fprintf(os.Stderr, "no .py cases found in %s\n", *dir)
		os.Exit(2)
	}
	sort.Strings(cases)

	// Run the startup case first so its timing is available for adjustment.
	report := Report{
		Timestamp: time.Now().UTC().Format(time.RFC3339),
		M28Bin:    *m28Bin,
		Python:    *python,
		Runs:      *runs,
	}

	type rawResult struct {
		sample        Sample
		pyMin, m28Min float64
		isStartup     bool
	}
	var results []rawResult

	for _, file := range cases {
		name := strings.TrimSuffix(filepath.Base(file), ".py")
		if filterRe != nil && name != startupCase && !filterRe.MatchString(name) {
			continue
		}

		pyTimes, pyOut, pyErr := timeRuns(*python, file, *runs, *timeout)
		m28Times, m28Out, m28Err := timeRuns(*m28Bin, file, *runs, *timeout)

		s := Sample{Case: name, Status: "ok"}
		switch {
		case pyErr != nil:
			s.Status = statusFor(pyErr)
			s.Note = "python: " + pyErr.Error()
		case m28Err != nil:
			s.Status = statusFor(m28Err)
			s.Note = "m28: " + m28Err.Error()
		case strings.TrimSpace(pyOut) != strings.TrimSpace(m28Out):
			s.Status = "mismatch"
			s.Note = fmt.Sprintf("py=%q m28=%q", truncate(pyOut), truncate(m28Out))
		}

		pyMin, pyMed := minMedian(pyTimes)
		m28Min, m28Med := minMedian(m28Times)
		s.PyMs, s.PyMedian = pyMin, pyMed
		s.M28Ms, s.M28Median = m28Min, m28Med
		if pyMin > 0 {
			s.Ratio = m28Min / pyMin
		}

		results = append(results, rawResult{sample: s, pyMin: pyMin, m28Min: m28Min, isStartup: name == startupCase})
	}

	// Establish startup baselines from the _startup case (0 if absent).
	for _, r := range results {
		if r.isStartup {
			report.PyStartupMs = r.pyMin
			report.M28StartupMs = r.m28Min
		}
	}

	// Compute startup-adjusted ratios and the geometric means.
	var logSum, logAdjSum float64
	var counted int
	for i := range results {
		r := &results[i]
		if r.isStartup {
			continue
		}
		pyAdj := r.pyMin - report.PyStartupMs
		m28Adj := r.m28Min - report.M28StartupMs
		// Only adjust when there is meaningful steady-state work left after
		// subtracting startup; otherwise fall back to the raw ratio.
		if pyAdj > 1 && m28Adj > 0 {
			r.sample.AdjRatio = m28Adj / pyAdj
		} else {
			r.sample.AdjRatio = r.sample.Ratio
		}
		if r.sample.Status == "ok" && r.sample.Ratio > 0 {
			logSum += math.Log(r.sample.Ratio)
			logAdjSum += math.Log(r.sample.AdjRatio)
			counted++
		}
		report.Cases = append(report.Cases, r.sample)
	}
	if counted > 0 {
		report.GeomeanRatio = math.Exp(logSum / float64(counted))
		report.GeomeanAdjRatio = math.Exp(logAdjSum / float64(counted))
	}

	data, _ := json.MarshalIndent(report, "", "  ")
	if err := os.WriteFile(*out, append(data, '\n'), 0o644); err != nil {
		fmt.Fprintf(os.Stderr, "could not write %s: %v\n", *out, err)
		os.Exit(1)
	}

	if *jsonOnly {
		fmt.Println(string(data))
		return
	}

	printTable(report, loadBaseline(*baseline))

	if *update {
		if err := os.WriteFile(*baseline, append(data, '\n'), 0o644); err != nil {
			fmt.Fprintf(os.Stderr, "could not write baseline %s: %v\n", *baseline, err)
			os.Exit(1)
		}
		fmt.Printf("\nbaseline updated -> %s\n", *baseline)
	}
}

// timeRuns runs `bin file` n times and returns the wall-clock times in
// milliseconds, the stdout of the last run (for correctness comparison), and
// the first error encountered (timeout or non-zero exit).
func timeRuns(bin, file string, n int, timeout time.Duration) ([]float64, string, error) {
	times := make([]float64, 0, n)
	var lastOut string
	for i := 0; i < n; i++ {
		ms, out, err := timeOnce(bin, file, timeout)
		if err != nil {
			return times, out, err
		}
		times = append(times, ms)
		lastOut = out
	}
	return times, lastOut, nil
}

func timeOnce(bin, file string, timeout time.Duration) (float64, string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, bin, file)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	start := time.Now()
	err := cmd.Run()
	elapsed := time.Since(start)

	if ctx.Err() == context.DeadlineExceeded {
		return 0, "", fmt.Errorf("%w after %s", errTimeout, timeout)
	}
	if err != nil {
		msg := strings.TrimSpace(stderr.String())
		if len(msg) > 200 {
			msg = msg[:200] + "..."
		}
		return 0, stdout.String(), fmt.Errorf("%v: %s", err, msg)
	}
	return float64(elapsed.Nanoseconds()) / 1e6, stdout.String(), nil
}

// minMedian returns the minimum and median of a slice of timings.
func minMedian(xs []float64) (float64, float64) {
	if len(xs) == 0 {
		return 0, 0
	}
	s := append([]float64(nil), xs...)
	sort.Float64s(s)
	min := s[0]
	var med float64
	n := len(s)
	if n%2 == 1 {
		med = s[n/2]
	} else {
		med = (s[n/2-1] + s[n/2]) / 2
	}
	return min, med
}

func statusFor(err error) string {
	if errors.Is(err, errTimeout) {
		return "timeout"
	}
	return "error"
}

func truncate(s string) string {
	s = strings.TrimSpace(s)
	if len(s) > 40 {
		return s[:40] + "..."
	}
	return s
}

func loadBaseline(path string) map[string]Sample {
	m := map[string]Sample{}
	data, err := os.ReadFile(path)
	if err != nil {
		return m
	}
	var r Report
	if json.Unmarshal(data, &r) != nil {
		return m
	}
	for _, s := range r.Cases {
		m[s.Case] = s
	}
	return m
}

// printTable renders a human-readable comparison, flagging regressions
// (current ratio worse than baseline by >5%) and improvements (>5% better).
func printTable(r Report, base map[string]Sample) {
	fmt.Printf("M28 performance vs %s   (best-of-%d, %s)\n", r.Python, r.Runs, r.Timestamp)
	fmt.Printf("startup: python=%.1fms  m28=%.1fms  (%.1fx)\n\n",
		r.PyStartupMs, r.M28StartupMs, safeRatio(r.M28StartupMs, r.PyStartupMs))
	fmt.Printf("%-16s %10s %10s %8s %8s %10s\n", "case", "py(ms)", "m28(ms)", "ratio", "adj", "vs base")
	fmt.Printf("%s\n", strings.Repeat("-", 66))
	for _, s := range r.Cases {
		// Regression is measured on M28's absolute time, not the CPython
		// ratio: M28 time is dominated by steady-state work and has low
		// variance, whereas the ratio also moves with CPython-side startup
		// noise (CPython steady-state is near its noise floor at these sizes).
		delta := ""
		if b, ok := base[s.Case]; ok && b.M28Ms > 0 && s.M28Ms > 0 {
			d := (s.M28Ms/b.M28Ms - 1) * 100
			switch {
			case d > 5:
				delta = fmt.Sprintf("+%.0f%% WORSE", d)
			case d < -5:
				delta = fmt.Sprintf("%.0f%% better", d)
			default:
				delta = fmt.Sprintf("%+.0f%%", d)
			}
		}
		status := ""
		if s.Status != "ok" {
			status = "  [" + strings.ToUpper(s.Status) + "]"
		}
		fmt.Printf("%-16s %10.1f %10.1f %7.1fx %7.1fx %10s%s\n",
			s.Case, s.PyMs, s.M28Ms, s.Ratio, s.AdjRatio, delta, status)
	}
	fmt.Printf("%s\n", strings.Repeat("-", 66))
	fmt.Printf("%-16s %10s %10s %7.1fx %7.1fx\n", "GEOMEAN", "", "", r.GeomeanRatio, r.GeomeanAdjRatio)
	if base != nil {
		if b, ok := base["__geomean__"]; ok && b.Ratio > 0 {
			fmt.Printf("baseline geomean: %.1fx\n", b.Ratio)
		}
	}
	fmt.Println("\nLower is better. 'adj' subtracts startup. Attack the worst-ratio case first.")
}

func safeRatio(a, b float64) float64 {
	if b == 0 {
		return 0
	}
	return a / b
}
