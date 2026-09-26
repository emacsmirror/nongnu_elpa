"""Exercise the real public matrix and Git-aware pure source evaluation.

Run outside Nix with an installed Emacs.  No fake Nix or fake ERT receipts:
only executable early-exit faults and one genuine failing ERT are injected.
All evidence is retained in the printed temporary directory.
"""

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


SOURCE = Path(__file__).resolve().parent.parent


def run(command, source, env, log):
    with log.open("w") as output:
        result = subprocess.run(command, cwd=source, env=env, stdout=output,
                                stderr=subprocess.STDOUT, timeout=300, check=False)
    return result.returncode, log.read_text()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacs", default=os.environ.get("THANOS_EMACS", "emacs"))
    args = parser.parse_args()
    emacs = str(Path(shutil.which(args.emacs) or args.emacs).resolve(strict=True))
    root = Path(tempfile.mkdtemp(prefix="jabber-matrix-public-"))
    print(f"Public regression evidence: {root}", flush=True)
    source = root / "source"
    source.mkdir()
    env = os.environ.copy()
    for key in ("IN_NIX_SHELL", "JABBER_ENV_WRAPPED", "MAKEFLAGS", "MFLAGS", "MAKEOVERRIDES"):
        env.pop(key, None)
    # Copy tracked inputs, not private/untracked working-tree contents.
    names = subprocess.check_output(["git", "ls-files", "-z"], cwd=SOURCE).decode().split("\0")
    for name in filter(None, names):
        target = source / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(SOURCE / name, target)
    subprocess.run(["git", "init", "-q", str(source)], check=True)
    subprocess.run(["git", "add", "."], cwd=source, check=True)
    results = []
    for mode in ("positive", "early-exit", "combined-exit", "summary-exit", "ert-failure"):
        wrapper = root / mode
        # The review reproducer's version-only pass-through is kept verbatim
        # in behavior: genuine version query, zero exit for all other work.
        wrapper.write_text(
            f"#!{sys.executable}\nimport json, os, sys\n"
            f"mode = {mode!r}\nreal = {emacs!r}\n"
            "version = '(princ emacs-version)' in sys.argv\n"
            "stop = (mode == 'early-exit' or "
            "(mode == 'combined-exit' and os.environ.get('JABBER_TEST_RUNS') == '2') or "
            "(mode == 'summary-exit' and 'admin/test-summary' in sys.argv))\n"
            "if not version and stop:\n"
            f" with open({str(root / (mode + '.jsonl'))!r}, 'a') as output:\n"
            "  output.write(json.dumps(sys.argv[1:]) + '\\n')\n"
            " sys.exit(0)\n"
            "os.execv(real, [real] + sys.argv[1:])\n")
        wrapper.chmod(0o755)
        if mode == "ert-failure":
            with (source / "tests/jabber-test-time.el").open("a") as test:
                test.write('\n(ert-deftest matrix-intentional-failure () (should nil))\n')
        command = ["make", "test-matrix", "TESTS=tests/jabber-test-time.el", "JOBS=1",
                   "THANOS_EMACS=" + str(wrapper)]
        status, text = run(command, source, env, root / (mode + ".log"))
        match = re.search(r"Matrix evidence: (.+)", text)
        assert match, text
        evidence = Path(match[1])
        assert all((evidence / (lane + ".log")).is_file() for lane in ("minimum", "default", "fork"))
        if mode == "positive":
            assert status == 0, text
            for lane in ("minimum", "default", "fork"):
                assert (evidence / lane / "passed").is_file(), text
                completion = json.loads((evidence / lane / "completion.json").read_text())
                assert completion["totals"] == {"isolated": 1, "combined": 2}, completion
                assert completion["files"] == ["tests/jabber-test-time.el"], completion
                assert len(completion["tests"]) == 1, completion
        else:
            assert status != 0 and "FAIL Thanos Emacs fork" in text, text
            assert not (evidence / "fork/passed").exists(), text
            assert (evidence / "fork/source/.test-results/jabber-test-time.stamp.log").is_file()
            if mode == "ert-failure":
                for lane in ("minimum", "default", "fork"):
                    assert not (evidence / lane / "passed").exists(), text
                    log = evidence / lane / "source/.test-results/jabber-test-time.stamp.log"
                    assert "matrix-intentional-failure" in log.read_text()
            else:
                assert (root / (mode + ".jsonl")).stat().st_size > 0
                assert (evidence / "minimum/passed").is_file(), text
                assert (evidence / "default/passed").is_file(), text
                assert "No such file" in (evidence / "fork.log").read_text()
        results.append({"case": mode, "exit": status, "evidence": str(evidence)})
        print(f"PASS public {mode}", flush=True)
    # No tracked test may disappear through the closed-world source filter.
    (source / "tests/jabber-test-unlisted.el").write_text(";; tracked manifest drift\n")
    subprocess.run(["git", "add", "tests/jabber-test-unlisted.el"], cwd=source, check=True)
    ref = "git+" + source.as_uri()
    command = ["nix", "eval", "--raw", ref + "#checks." +
               subprocess.check_output(["nix", "eval", "--impure", "--raw", "--expr", "builtins.currentSystem"], text=True).strip() + ".matrix-default.drvPath"]
    status, text = run(command, source, env, root / "manifest-drift.log")
    assert status != 0 and "Ordinary test manifest incomplete: tests/jabber-test-unlisted.el" in text, text
    subprocess.run(["git", "rm", "-f", "tests/jabber-test-unlisted.el"], cwd=source, check=True, stdout=subprocess.DEVNULL)
    (source / "tests/jabber-test-private.el").write_text("private untracked sentinel\n")
    with (source / ".git/info/exclude").open("a") as exclude:
        exclude.write("tests/jabber-test-ignored.el\n")
    (source / "tests/jabber-test-ignored.el").write_text("private ignored sentinel\n")
    status, text = run(command, source, env, root / "manifest-private.log")
    assert status == 0, text
    archive = json.loads(subprocess.check_output(["nix", "flake", "archive", "--json", ref], cwd=source, env=env, text=True))
    realized = Path(archive["path"])
    assert realized.is_dir()
    for name in ("tests/jabber-test-private.el", "tests/jabber-test-ignored.el"):
        assert not (realized / name).exists()
    results.append({"case": "tracked-manifest-drift-and-private-exclusion", "archive": str(realized)})
    (root / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    print("PASS tracked manifest drift and private exclusion", flush=True)


if __name__ == "__main__":
    main()
