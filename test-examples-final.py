#!/usr/bin/env python3
"""Test all M28 examples and report results."""

import subprocess
import os
from pathlib import Path

def test_example(filepath):
    """Test a single example file."""
    try:
        result = subprocess.run(
            ['./bin/m28', str(filepath)],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.returncode == 0, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return False, "", "Timeout"
    except Exception as e:
        return False, "", str(e)

def main():
    # Find all example files
    examples_dir = Path("examples")
    example_files = sorted(examples_dir.rglob("*.m28"))
    
    results = []
    for example_file in example_files:
        success, stdout, stderr = test_example(example_file)
        results.append((example_file, success, stderr))
        
        status = "✓" if success else "✗"
        print(f"{status} {example_file.relative_to('examples')}")
        if not success and stderr:
            print(f"  Error: {stderr.strip()[:80]}")
    
    # Summary
    passed = sum(1 for _, success, _ in results if success)
    total = len(results)
    print(f"\nSummary: {passed}/{total} examples passed ({passed/total*100:.1f}%)")
    
    # List failures
    failures = [(f, e) for f, s, e in results if not s]
    if failures:
        print("\nFailed examples:")
        for f, e in failures:
            print(f"  - {f.relative_to('examples')}")

if __name__ == "__main__":
    main()