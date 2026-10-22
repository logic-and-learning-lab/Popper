#!/usr/bin/env python3
import argparse
import concurrent.futures
import json
import os
import random
import subprocess
import sys


RESULT_PREFIX = "@@POPPER_BENCHMARK_RESULT@@"
TIMEOUT_MARKER = "TIMEOUT OF "
TIMEOUT_SUFFIX = "SECONDS EXCEEDED"

DETERMINISTIC_ENV = {
    "PYTHONHASHSEED": "0",
}

DETERMINISTIC_CLINGO_OPTS = [
    "--seed=1",
]

CLINGO_OPT_PREFIXES = (
    "--seed",
)

# Baselines from the 60s timeout-aware run. A later run must find a program of
# the same size while generating no more programs than this baseline.
BASELINES = [
    {"path": "examples/andersen", "size": 11, "max_programs": 5223},
    {"path": "examples/1d_flip", "size": 21, "max_programs": 75121, "extra_args": ["--max-body=5"]},
    {"path": "examples/iggp-attrition-next-score", "size": 14, "max_programs": 3327},
    {"path": "examples/iggp-buttons-goal", "size": 13, "max_programs": 192},
    {"path": "examples/iggp-buttons-next", "size": 52, "max_programs": 1544},
    {"path": "examples/iggp-coins-next-cell", "size": 11, "max_programs": 533},
    {"path": "examples/iggp-dont-touch-next-control", "size": 8, "max_programs": 3567},
    {"path": "examples/iggp-eight-puzzle-legal-move", "size": 20, "max_programs": 38956},
    {"path": "examples/iggp-farming-next-has-arson", "size": 10, "max_programs": 9611},
    {"path": "examples/iggp-gt-chicken-goal", "size": 6, "max_programs": 286},
    {"path": "examples/iggp-connect4team-next-control", "size": 16, "max_programs": 62627, "extra_args": ["--max-body=5"]},
    {"path": "examples/iggp-gt_centipede-goal", "size": 40, "max_programs": 2791},
    {"path": "examples/iggp-hextforthree-next-control", "size": 12, "max_programs": 13019},
    {"path": "examples/iggp-minimal-decay-next-value", "size": 9, "max_programs": 213},
    {"path": "examples/iggp-polgrimage-goal", "size": 18, "max_programs": 4954},
    {"path": "examples/iggp-rps-next-score", "size": 16, "max_programs": 393},
    {"path": "examples/iggp-sokoban-goal", "size": 8, "max_programs": 1622},
    {"path": "examples/iggp-ultimatium-goal", "size": 6, "max_programs": 388},
    {"path": "examples/imdb3", "size": 10, "max_programs": 277},
    {"path": "examples/kinship-ancestor", "size": 6, "max_programs": 88},
    {"path": "examples/kinship-pi", "size": 7, "max_programs": 28789},
    {"path": "examples/noisy-alzheimer_amine", "size": 34, "max_programs": 3382, "noisy": True},
    {"path": "examples/noisy-alzheimer_mem", "size": 17, "max_programs": 4955, "noisy": True},
    {"path": "examples/noisy-alzheimer_toxic", "size": 26, "max_programs": 3841, "noisy": True},
    {"path": "examples/robots-linear", "size": 6, "max_programs": 142},
    {"path": "examples/robots-recursion", "size": 6, "max_programs": 1542},
    {"path": "examples/synthesis-alleven", "size": 7, "max_programs": 1835},
    {"path": "examples/synthesis-contains", "size": 9, "max_programs": 1026},
    {"path": "examples/synthesis-dropk", "size": 7, "max_programs": 61},
    {"path": "examples/synthesis-droplast", "size": 8, "max_programs": 504},
    {"path": "examples/synthesis-finddupl", "size": 7, "max_programs": 12065},
    {"path": "examples/synthesis-length", "size": 7, "max_programs": 99},
    {"path": "examples/synthesis-next", "size": 8, "max_programs": 510},
    {"path": "examples/synthesis-reverse", "size": 8, "max_programs": 245},
    {"path": "examples/synthesis-sorted", "size": 10, "max_programs": 5321},
    {"path": "examples/trains1", "size": 6, "max_programs": 223},
    {"path": "examples/trains2", "size": 11, "max_programs": 340},
    {"path": "examples/zendo1", "size": 6, "max_programs": 682},
]


def child_env():
    env = os.environ.copy()
    env.update(DETERMINISTIC_ENV)
    return env


def deterministic_clingo_args(arguments):
    arguments = list(arguments or [])
    filtered = [
        arg for arg in arguments
        if not any(arg == opt or arg.startswith(f"{opt}=") for opt in CLINGO_OPT_PREFIXES)
    ]
    return filtered + DETERMINISTIC_CLINGO_OPTS


def run_child(kbpath, timeout, noisy, extra_args=None):
    if os.environ.get("PYTHONHASHSEED") != "0":
        raise RuntimeError("child must be started with PYTHONHASHSEED=0")

    random.seed(0)

    import numpy as np
    np.random.seed(0)

    import clingo

    counters = {
        "clingo_control_calls": 0,
        "clingo_solve_calls": 0,
    }
    real_control = clingo.Control

    class CountingControl:
        def __init__(self, arguments=None, *args, **kwargs):
            counters["clingo_control_calls"] += 1
            self._control = real_control(deterministic_clingo_args(arguments), *args, **kwargs)

        def solve(self, *args, **kwargs):
            counters["clingo_solve_calls"] += 1
            return self._control.solve(*args, **kwargs)

        def __getattr__(self, name):
            return getattr(self._control, name)

    clingo.Control = CountingControl

    argv = ["popper.py", kbpath, "--timeout", str(timeout)]
    if noisy:
        argv.append("-n")
    argv.extend(extra_args or [])
    sys.argv = argv

    from popper import stats
    from popper.loop import popper
    from popper.util import Settings, calc_prog_size, mdl_score

    settings = Settings.from_args()
    prog, score = popper(settings)
    size = calc_prog_size(prog) if prog is not None else None
    cost = None
    if prog is not None and score is not None:
        _tp, fn, _tn, fp = score
        cost = mdl_score(fn, fp, size) if settings.noisy else size

    result = {
        "path": kbpath,
        "noisy": noisy,
        "size": size,
        "cost": cost,
        "score": list(score) if score is not None else None,
        "total_programs": stats.stats.total_programs,
        "clingo_control_calls": counters["clingo_control_calls"],
        "clingo_solve_calls": counters["clingo_solve_calls"],
    }
    print(RESULT_PREFIX + json.dumps(result, sort_keys=True))


def parse_result(completed):
    output = completed.stdout + completed.stderr
    timeout_hit = TIMEOUT_MARKER in output and TIMEOUT_SUFFIX in output
    matches = [
        line for line in output.splitlines()
        if line.startswith(RESULT_PREFIX)
    ]
    if not matches:
        return None, timeout_hit, output
    result = json.loads(matches[-1][len(RESULT_PREFIX):])
    result["timeout_hit"] = timeout_hit
    return result, timeout_hit, output


def run_one(baseline, timeout, child_timeout):
    cmd = [
        sys.executable,
        __file__,
        "--child",
        baseline["path"],
        "--timeout",
        str(timeout),
    ]
    if baseline.get("noisy", False):
        cmd.append("--noisy")
    for arg in baseline.get("extra_args", []):
        cmd.append(f"--extra-arg={arg}")

    try:
        completed = subprocess.run(
            cmd,
            env=child_env(),
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=child_timeout,
        )
    except subprocess.TimeoutExpired as exc:
        output = exc.stdout or ""
        if isinstance(output, bytes):
            output = output.decode(errors="replace")
        return {
            "status": "FAIL",
            "path": baseline["path"],
            "reason": f"child process exceeded {child_timeout}s",
            "output": output,
        }

    result, timeout_hit, output = parse_result(completed)
    if timeout_hit:
        return {
            "status": "FAIL",
            "path": baseline["path"],
            "reason": "Popper timeout",
            "output": output,
        }
    if completed.returncode != 0:
        return {
            "status": "FAIL",
            "path": baseline["path"],
            "reason": f"child exit {completed.returncode}",
            "output": output,
        }
    if result is None:
        return {
            "status": "FAIL",
            "path": baseline["path"],
            "reason": "missing result",
            "output": output,
        }

    if result["size"] != baseline["size"]:
        return {
            "status": "FAIL",
            "path": baseline["path"],
            "reason": f"size {result['size']} != expected {baseline['size']}",
            "result": result,
            "baseline": baseline,
            "output": output,
        }

    if "expected_tp" in baseline:
        actual_tp = result["score"][0] if result["score"] else None
        if actual_tp != baseline["expected_tp"]:
            return {
                "status": "FAIL",
                "path": baseline["path"],
                "reason": f"tp {actual_tp} != expected {baseline['expected_tp']}",
                "result": result,
                "baseline": baseline,
                "output": output,
            }

    warnings = []
    if result["total_programs"] > baseline["max_programs"]:
        warnings.append(
            f"programs {result['total_programs']} > max {baseline['max_programs']}"
        )

    return {
        "status": "PASS",
        "path": baseline["path"],
        "reason": "ok",
        "warnings": warnings,
        "result": result,
        "baseline": baseline,
        "output": output,
    }


def select_baselines(patterns):
    if not patterns:
        return BASELINES
    return [
        baseline for baseline in BASELINES
        if any(pattern in baseline["path"] for pattern in patterns)
    ]


def run_parent(args):
    baselines = select_baselines(args.only)
    if not baselines:
        raise SystemExit("no matching benchmark baselines")

    child_timeout = args.child_timeout or args.timeout + 90
    failures = []

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = {
            executor.submit(run_one, baseline, args.timeout, child_timeout): baseline
            for baseline in baselines
        }
        for future in concurrent.futures.as_completed(futures):
            outcome = future.result()
            if outcome["status"] == "PASS":
                for w in outcome.get("warnings", []):
                    print(f"WARN\t{outcome['path']}\t({w})", flush=True)
                print(f"PASS\t{outcome['path']}", flush=True)
            else:
                print(f"FAIL\t{outcome['path']}\t({outcome['reason']})", flush=True)
                failures.append(outcome)

    print()
    print(f"Benchmarks checked: {len(baselines)}")
    print(f"Passed: {len(baselines) - len(failures)}")
    print(f"Failed: {len(failures)}")

    if failures:
        print()
        print("Failures:")
        for failure in failures:
            print(f"- {failure['path']}: {failure['reason']}")
        raise SystemExit(1)


def main():
    if len(sys.argv) > 1 and sys.argv[1] == "--child":
        parser = argparse.ArgumentParser()
        parser.add_argument("kbpath")
        parser.add_argument("--timeout", type=int, required=True)
        parser.add_argument("--noisy", action="store_true")
        parser.add_argument("--extra-arg", action="append", default=[], dest="extra_args")
        args = parser.parse_args(sys.argv[2:])
        run_child(args.kbpath, args.timeout, args.noisy, args.extra_args)
        return

    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout", type=int, default=60)
    parser.add_argument("--child-timeout", type=int, default=None)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument(
        "--only",
        action="append",
        default=[],
        help="Run only baselines whose path contains this substring. Can be repeated.",
    )
    args = parser.parse_args()
    run_parent(args)


if __name__ == "__main__":
    main()
