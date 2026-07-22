#!/usr/bin/env python3
"""Adversarial Nix input tests for nix-compile.

Generates adversarial Nix source files across multiple categories and tests them
against all nix-compile subcommands (nix, fmt, scope, typecheck).

Usage:
  python tools/adversarial_test.py [--binary PATH] [--outdir DIR] [--report REPORT.json]
"""

import json
import os
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


def find_binary() -> str:
    """Locate the nix-compile binary."""
    result = subprocess.run(
        ["cabal", "list-bin", "exe:nix-compile"],
        capture_output=True, text=True, cwd=str(WORKDIR),
    )
    if result.returncode == 0 and result.stdout.strip():
        return result.stdout.strip()
    # fallback path
    fallback = WORKDIR / "dist-newstyle/build/x86_64-linux/ghc-9.10.3/nix-compile-0.1.0.0/x/nix-compile/build/nix-compile/nix-compile"
    if fallback.exists():
        return str(fallback)
    raise RuntimeError("Cannot find nix-compile binary")


WORKDIR = Path(__file__).resolve().parent.parent
TIMEOUT = 5  # seconds


# ============================================================================
# Data types
# ============================================================================

@dataclass
class TestResult:
    name: str
    command: str
    exit_code: Optional[int]
    stdout: str = ""
    stderr: str = ""
    timed_out: bool = False
    exception: Optional[str] = None


@dataclass
class TestCase:
    name: str
    content: str
    results: list[TestResult] = field(default_factory=list)


# ============================================================================
# Generator functions
# ============================================================================

def gen_deeply_nested_let(depth: int = 200) -> str:
    lines = [f"let a{i} = {i}; in" for i in range(1, depth + 1)]
    src = "\n".join(lines)
    src += f" a{depth}"
    return src


def gen_deeply_nested_attrset(depth: int = 500) -> str:
    inner = "42"
    for i in range(depth, 0, -1):
        inner = f"{{ a = {inner}; }}"
    return inner


def gen_deeply_nested_func_calls(depth: int = 200) -> str:
    left = "f " * depth
    return left + "1"


def gen_mutual_recursion() -> str:
    return "rec {\n  a = b;\n  b = a;\n}"


def gen_self_ref_func() -> str:
    return "rec {\n  f = x: f x;\n}"


def gen_indirect_cycle() -> str:
    return "rec {\n  a = b.c;\n  b = { c = a; };\n}"


def gen_large_string(size: int = 100_000) -> str:
    return '"' + ("a" * size) + '"\n'


def gen_large_list(count: int = 10_000) -> str:
    elems = " ".join(str(i) for i in range(1, count + 1))
    return f"[ {elems} ]"


def gen_unicode_varname() -> str:
    return "let 変数 = 1;\n in 変数"


def gen_emoji_in_string() -> str:
    return '"\U0001f525\U0001f525\U0001f525 ${\"\U0001f480\"} \U0001f525\U0001f525\U0001f525"'


def gen_zero_width_chars() -> str:
    return '"hello\u200bworld"  # zero-width space between hello and world'


def gen_long_pattern(n_params: int = 100) -> str:
    params = ", ".join(chr(ord("a") + (i % 26)) for i in range(n_params))
    return f"{{ {params} }}: 1"


def gen_variadic_pattern() -> str:
    return "{ ... }@args: args"


def gen_default_complex_expr() -> str:
    return "{ x ? (let y = 1; in y + 1) }: x"


def gen_nested_with() -> str:
    return "let lib = {}; pkgs = {}; stdenv = {}; in with lib; with pkgs; stdenv"


def gen_with_complex_expr() -> str:
    return "let lib = {}; pkgs = {}; in with (if true then lib else pkgs); 1"


def gen_with_lambda() -> str:
    return "with (x: x); 1"


def gen_long_attr_path() -> str:
    labels = "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z"
    return f"let x = {{ z = 1; }}; in x.{labels}"


def gen_dynamic_attr_access() -> str:
    return 'let x = { b = 1; }; in a.${"b"}'


def gen_or_null_chain(depth: int = 100) -> str:
    parts = ["null"] * depth
    return " or ".join(parts)


# ============================================================================
# All test cases
# ============================================================================

ALL_TESTS: list[TestCase] = []


def register(name: str, content: str) -> None:
    content = content.strip() + "\n"
    ALL_TESTS.append(TestCase(name=name, content=content))


register("deep_let_200", gen_deeply_nested_let(200))
register("deep_attrset_500", gen_deeply_nested_attrset(500))
register("deep_func_calls_200", gen_deeply_nested_func_calls(200))
register("mutual_recursion", gen_mutual_recursion())
register("self_ref_func", gen_self_ref_func())
register("indirect_cycle", gen_indirect_cycle())
register("large_string_100kb", gen_large_string(100_000))
register("large_list_10k", gen_large_list(10_000))
register("unicode_varname", gen_unicode_varname())
register("emoji_in_string", gen_emoji_in_string())
register("zero_width_chars", gen_zero_width_chars())
register("long_pattern_100", gen_long_pattern(100))
register("variadic_pattern", gen_variadic_pattern())
register("default_complex_expr", gen_default_complex_expr())
register("nested_with", gen_nested_with())
register("with_complex_expr", gen_with_complex_expr())
register("with_lambda", gen_with_lambda())
register("long_attr_path_26", gen_long_attr_path())
register("dynamic_attr_access", gen_dynamic_attr_access())
register("or_null_chain_100", gen_or_null_chain(100))


# ============================================================================
# Test runner
# ============================================================================

def run_command(
    binary: str, subcommand: str, filepath: str, timeout: int = TIMEOUT
) -> TestResult:
    """Run a nix-compile subcommand against a file and return the result."""
    cmd = [binary, subcommand, filepath]
    result = TestResult(
        name=Path(filepath).name,
        command=" ".join(cmd),
        exit_code=None,
    )
    try:
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        result.exit_code = proc.returncode
        result.stdout = proc.stdout[:10000]
        result.stderr = proc.stderr[:10000]
    except subprocess.TimeoutExpired:
        result.timed_out = True
    except Exception as e:
        result.exception = str(e)
    return result


def run_all_tests(
    binary: str, outdir: Path
) -> list[TestCase]:
    """Write all test files and run all subcommands against each."""
    outdir.mkdir(parents=True, exist_ok=True)

    test_cases: list[TestCase] = []

    for tc in ALL_TESTS:
        filepath = outdir / f"{tc.name}.nix"
        filepath.write_text(tc.content)

        results: list[TestResult] = []

        # Run nix
        r = run_command(binary, "nix", str(filepath))
        r.name = tc.name
        results.append(r)

        # Run fmt
        r = run_command(binary, "fmt", str(filepath))
        r.name = tc.name
        results.append(r)

        # Run scope
        r = run_command(binary, "scope", str(filepath))
        r.name = tc.name
        results.append(r)

        # Run typecheck on the directory
        r = run_command(binary, "typecheck", str(outdir))
        r.name = tc.name
        results.append(r)

        tc.results = results
        test_cases.append(tc)

    return test_cases


# ============================================================================
# Report generation
# ============================================================================

def classify_result(r: TestResult) -> str:
    """Classify a test result."""
    if r.timed_out:
        return "TIMEOUT"
    if r.exception:
        return "CRASH"
    if r.exit_code is None:
        return "UNKNOWN"
    if r.exit_code == 0:
        return "OK"
    if r.exit_code < 0:
        return f"CRASH (signal {-r.exit_code})"
    return f"FAIL (exit {r.exit_code})"


def is_concerning(r: TestResult) -> bool:
    """Check if a result is concerning (crash or hang)."""
    if r.timed_out:
        return True
    if r.exception:
        return True
    if r.exit_code is not None and r.exit_code < 0:
        return True
    # Check stderr for segfault/panic indicators
    stderr_lower = r.stderr.lower()
    crash_keywords = [
        "segfault", "segmentation fault", "core dumped",
        "stack overflow", "out of memory", "panic",
        "assertion failed", "internal error", "exception",
        "non-exhaustive", "irrefutable",
    ]
    return any(kw in stderr_lower for kw in crash_keywords)


def generate_report(test_cases: list[TestCase]) -> dict:
    """Generate a structured report."""
    summary = {
        "total_tests": len(test_cases),
        "total_commands": 0,
        "crashes": [],
        "hangs": [],
        "unexpected_successes": [],
        "failures": [],
        "ok": [],
        "performance_issues": [],
        "per_command": {
            "nix": {"ok": 0, "fail": 0, "crash": 0, "hang": 0, "skip": 0},
            "fmt": {"ok": 0, "fail": 0, "crash": 0, "hang": 0, "skip": 0},
            "scope": {"ok": 0, "fail": 0, "crash": 0, "hang": 0, "skip": 0},
            "typecheck": {"ok": 0, "fail": 0, "crash": 0, "hang": 0, "skip": 0},
        },
    }

    for tc in test_cases:
        is_rec = "rec {" in tc.content
        is_with = "with " in tc.content

        for r in tc.results:
            subcmd = (
                "nix" if " nix " in r.command
                else "fmt" if " fmt " in r.command
                else "scope" if " scope " in r.command
                else "typecheck" if " typecheck " in r.command
                else "unknown"
            )
            cls = classify_result(r)
            entry = {
                "test": tc.name,
                "command": r.command,
                "classification": cls,
                "exit_code": r.exit_code,
                "timed_out": r.timed_out,
                "exception": r.exception,
                "stderr_preview": r.stderr[:500] if r.stderr else "",
                "stdout_preview": r.stdout[:500] if r.stdout else "",
            }

            summary["total_commands"] += 1

            if r.timed_out:
                summary["hangs"].append(entry)
                summary["per_command"][subcmd]["hang"] += 1
            elif is_concerning(r):
                summary["crashes"].append(entry)
                summary["per_command"][subcmd]["crash"] += 1
            elif r.exit_code == 0:
                # Check if success is unexpected
                if is_rec:
                    entry["note"] = "rec is a forbidden Nix construct (NARSIL-N002) — success unexpected"
                    summary["unexpected_successes"].append(entry)
                if is_with:
                    entry["note"] = "with is a forbidden Nix construct (NARSIL-N001) — success unexpected"
                    if entry not in summary["unexpected_successes"]:
                        summary["unexpected_successes"].append(entry)
                summary["ok"].append(entry)
                summary["per_command"][subcmd]["ok"] += 1
            else:
                summary["failures"].append(entry)
                summary["per_command"][subcmd]["fail"] += 1

        # Check for performance issues on large inputs
        if "large_string" in tc.name or "large_list" in tc.name:
            for r in tc.results:
                if not r.timed_out:
                    summary["performance_issues"].append({
                        "test": tc.name,
                        "command": r.command,
                        "note": "Large input processed without timeout (check memory usage)",
                    })

    return summary


def print_report(summary: dict) -> None:
    """Print a human-readable report."""
    print("=" * 70)
    print("  ADVERSARIAL NIX TEST REPORT")
    print("=" * 70)
    print()
    print(f"  Total test files:  {summary['total_tests']}")
    print(f"  Total commands run: {summary['total_commands']}")
    print()

    # Per-command breakdown
    print("  ─── Per-Command Breakdown ───")
    print(f"  {'Command':<12} {'OK':>5} {'FAIL':>5} {'CRASH':>5} {'HANG':>5}")
    print(f"  {'─' * 12} {'─' * 5} {'─' * 5} {'─' * 5} {'─' * 5}")
    for cmd, stats in summary["per_command"].items():
        print(f"  {cmd:<12} {stats['ok']:>5} {stats['fail']:>5} {stats['crash']:>5} {stats['hang']:>5}")
    print()

    # Crashes
    if summary["crashes"]:
        print(f"  {'!' * 20} CRASHES ({len(summary['crashes'])}) {'!' * 20}")
        for c in summary["crashes"]:
            print(f"    [{c['classification']}] {c['test']}")
            print(f"      cmd: {c['command']}")
            if c["exception"]:
                print(f"      exception: {c['exception']}")
            if c["stderr_preview"]:
                preview = c["stderr_preview"].replace("\n", "\n      ")
                print(f"      stderr: {preview}")
        print()

    # Hangs
    if summary["hangs"]:
        print(f"  {'!' * 20} HANGS/TIMEOUTS ({len(summary['hangs'])}) {'!' * 20}")
        for h in summary["hangs"]:
            print(f"    {h['test']}: {h['command']}")
        print()

    # Unexpected successes
    if summary["unexpected_successes"]:
        print(f"  {'?' * 20} UNEXPECTED SUCCESSES ({len(summary['unexpected_successes'])}) {'?' * 20}")
        seen = set()
        for u in summary["unexpected_successes"]:
            key = (u["test"], u["command"])
            if key not in seen:
                seen.add(key)
                print(f"    {u['test']}: {u['command']}")
                if u.get("note"):
                    print(f"      note: {u['note']}")
        print()

    # Performance
    if summary["performance_issues"]:
        print(f"  {'⚡' * 10} PERFORMANCE NOTES ({len(summary['performance_issues'])}) {'⚡' * 10}")
        for p in summary["performance_issues"]:
            print(f"    {p['test']}: {p['command']} - {p['note']}")
        print()

    # OK count
    ok_count = len(summary["ok"])
    crash_count = len(summary["crashes"])
    hang_count = len(summary["hangs"])
    unexpected_count = len(summary["unexpected_successes"])
    fail_count = len(summary["failures"])

    print(f"  Summary: {ok_count} OK, {fail_count} FAIL, {crash_count} CRASH, {hang_count} HANG, {unexpected_count} UNEXPECTED")
    print("=" * 70)

    if crash_count > 0 or hang_count > 0:
        print("\n  *** WARNING: Crashes or hangs detected! ***")
        sys.exit(1)
    elif unexpected_count > 0:
        print("\n  *** NOTE: Unexpected successes detected (lint violations missed) ***")
        sys.exit(1)
    else:
        print("\n  All tests completed without crashes or unexpected results.")
        sys.exit(0)


# ============================================================================
# Main
# ============================================================================

def main() -> None:
    import argparse

    parser = argparse.ArgumentParser(
        description="Adversarial Nix input tests for nix-compile"
    )
    parser.add_argument(
        "--binary",
        default=None,
        help="Path to nix-compile binary (auto-detected if not given)",
    )
    parser.add_argument(
        "--outdir",
        default=None,
        help="Directory for generated Nix test files (temp dir if not given)",
    )
    parser.add_argument(
        "--report",
        default=None,
        help="Write JSON report to this file",
    )
    parser.add_argument(
        "--keep",
        action="store_true",
        help="Keep generated test files (don't clean up)",
    )
    args = parser.parse_args()

    binary = args.binary or find_binary()
    print(f"Using binary: {binary}")
    print()

    if args.outdir:
        outdir = Path(args.outdir)
        outdir.mkdir(parents=True, exist_ok=True)
        print(f"Test directory: {outdir}")
        print()
        test_cases = run_all_tests(binary, outdir)
        summary = generate_report(test_cases)
        print_report(summary)
        if args.report:
            Path(args.report).write_text(json.dumps(summary, indent=2))
    else:
        with tempfile.TemporaryDirectory(prefix="nix-adversarial-") as tmpdir:
            outdir = Path(tmpdir)
            print(f"Test directory: {outdir}")
            print()
            test_cases = run_all_tests(binary, outdir)
            summary = generate_report(test_cases)
            print_report(summary)
            if args.report:
                Path(args.report).write_text(json.dumps(summary, indent=2))
            if args.keep:
                print(f"\nTest files kept at: {outdir}")
                import shutil
                keepdir = WORKDIR / "tools" / "adversarial_output"
                keepdir.mkdir(parents=True, exist_ok=True)
                for f in outdir.iterdir():
                    shutil.copy2(str(f), str(keepdir / f.name))
                print(f"Files copied to: {keepdir}")


if __name__ == "__main__":
    main()
