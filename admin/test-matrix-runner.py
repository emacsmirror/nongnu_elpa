"""Regression tests for matrix failure, provenance, and isolation boundaries."""

from contextlib import redirect_stdout
import copy
import io
import json
import os
from pathlib import Path
import runpy
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


RUNNER = runpy.run_path(str(Path(__file__).with_name("test-matrix")))
SOURCE = Path(__file__).resolve().parent.parent


def completion_fixture(build):
    """Supply explicit synthetic records only for the validator unit tests."""
    files = ["tests/jabber-test-time.el"]
    counts = {"tests": ["time-test"], "total": 1, "completed": 1,
              "expected": 1, "unexpected": 0, "skipped": 0}
    for name, runs in (("jabber-test-time", 1), ("oneshot", 2)):
        stamp = ".test-results/" + name + ".stamp"
        (build / stamp).parent.mkdir(parents=True, exist_ok=True)
        (build / stamp).write_text("0\n")
        (build / (stamp + ".ert.json")).write_text(json.dumps({"files": files, "runs": [counts] * runs}))
        (build / (stamp + ".summary.json")).write_text(json.dumps(
            {"stamps": [stamp], "total": runs, "expected": runs, "unexpected": 0, "skipped": 0}))
    (build / "lisp").mkdir(exist_ok=True)
    (build / "lisp/jabber-autoloads.el").write_text("fixture")
    suffix = ".dylib" if sys.platform == "darwin" else ".so"
    (build / ("lisp/jabber-omemo-core" + suffix)).write_text("fixture")
    return files


class MatrixTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="jabber-matrix-selftest-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def test_header_minimum_is_exact(self):
        self.assertEqual(RUNNER["minimum_version"](SOURCE), "29.1")

    def test_snapshot_omits_private_and_generated_files(self):
        source = self.root / "input"
        (source / "admin").mkdir(parents=True)
        (source / "admin/source-manifest.json").write_text('["source.el"]')
        (source / "source.el").write_text("source")
        for name in ("private.el", "local.mk", "source.elc", "module.so"):
            (source / name).write_text("must not copy")
        destination = self.root / "output"
        RUNNER["snapshot"](source, destination)
        self.assertEqual([p.name for p in destination.iterdir()], ["source.el"])
        (source / "source.el").unlink()
        (source / "source.el").symlink_to(source / "private.el")
        with self.assertRaisesRegex(ValueError, "symlinked"):
            RUNNER["snapshot"](source, self.root / "rejected")

    def run_matrix(self, missing_fork=False, fail_minimum=False, missing_receipt=False):
        calls = []
        resolved = []

        def resolve(command):
            resolved.append(command)
            if command == "user-fork" and missing_fork:
                raise ValueError("missing user-fork")
            return "/resolved/" + command

        def run(command, **_):
            name = command[command.index("--lane") + 1]
            calls.append(name)
            self.assertEqual(resolved[0], "user-fork")
            self.assertEqual(command[2].split("#")[0], str(SOURCE))
            if name == "fork":
                self.assertEqual(command[-2:], ["--emacs", "/resolved/user-fork"])
            if not missing_receipt:
                lane = Path(command[command.index("--root") + 1])
                lane.mkdir()
                (lane / "passed").write_text("29.1\n")
            return subprocess.CompletedProcess(command, 1 if fail_minimum and name == "minimum" else 0)

        with patch.dict(os.environ, {"THANOS_EMACS": "user-fork"}, clear=True), \
             patch.dict(RUNNER["matrix"].__globals__, {"executable": resolve}), \
             patch("tempfile.mkdtemp", return_value=str(self.root)), \
             patch("subprocess.check_output", return_value=json.dumps({"path": str(SOURCE)})), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            if missing_fork or fail_minimum or missing_receipt:
                with self.assertRaisesRegex(RuntimeError, "Required matrix lanes failed"):
                    RUNNER["matrix"](SOURCE)
            else:
                RUNNER["matrix"](SOURCE)
        return calls

    def test_success_resolves_fork_before_nix(self):
        self.assertEqual(self.run_matrix(), ["minimum", "default", "fork"])

    def test_failure_still_attempts_every_lane(self):
        self.assertEqual(self.run_matrix(fail_minimum=True), ["minimum", "default", "fork"])

    def test_missing_fork_does_not_suppress_nix_lanes(self):
        self.assertEqual(self.run_matrix(missing_fork=True), ["minimum", "default"])

    def test_zero_exit_without_receipt_is_failure(self):
        self.assertEqual(self.run_matrix(missing_receipt=True), ["minimum", "default", "fork"])

    def test_lane_isolation_and_focused_options(self):
        dependencies = self.root / "deps"
        (dependencies / "fsm").mkdir(parents=True)
        (dependencies / "fsm/fsm.el").write_text("source")
        observed = []

        def run(command, cwd, env, **_):
            observed.append((cwd, env))
            self.assertIn("TESTS=tests/jabber-test-time.el", command)
            self.assertIn("JOBS=1", command)
            self.assertNotIn("MAKEFLAGS", env)
            self.assertNotIn("EMACSLOADPATH", env)
            self.assertFalse((cwd / "lisp/jabber.elc").exists())
            (cwd / "lisp/jabber.elc").write_text("private bytecode")
            (cwd / "lisp/jabber-omemo-core.so").write_text("private module")
            completion_fixture(cwd)
            return subprocess.CompletedProcess(command, 0)

        with patch.dict(os.environ, {"JABBER_MATRIX_DEPS": str(dependencies),
                                     "MATRIX_TESTS": "tests/jabber-test-time.el", "MATRIX_JOBS": "1",
                                     "MAKEFLAGS": "bad", "EMACSLOADPATH": "bad"}), \
             patch("subprocess.check_output", return_value="29.1"), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            for name in ("minimum", "default", "fork"):
                RUNNER["lane"](SOURCE, self.root / name, name, "/fake/emacs", "29.1")
        for key in ("HOME", "TMPDIR", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME"):
            self.assertEqual(len({env[key] for _, env in observed}), 3)
        self.assertEqual(len({cwd for cwd, _ in observed}), 3)

    def test_wrong_patch_release_is_not_minimum(self):
        dependencies = self.root / "deps"
        dependencies.mkdir()
        with patch.dict(os.environ, {"JABBER_MATRIX_DEPS": str(dependencies)}), \
             patch("subprocess.check_output", return_value="29.4"), \
             patch("subprocess.run") as run, redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, "expected 29.1, got 29.4"):
                RUNNER["lane"](SOURCE, self.root / "minimum", "minimum", "/fake/emacs", "29.1")
            run.assert_not_called()

    def test_independent_completion_validation(self):
        files = completion_fixture(self.root)
        validate = RUNNER["validate_completion"]
        self.assertEqual(validate(self.root, files)["totals"], {"isolated": 1, "combined": 2})
        receipt = self.root / ".test-results/oneshot.stamp.ert.json"
        original = json.loads(receipt.read_text())
        mutants = []
        for key, value in (("completed", 0), ("total", 0), ("unexpected", 1),
                           ("skipped", 1), ("tests", ["omitted-original"]), ("total", True)):
            mutant = copy.deepcopy(original)
            mutant["runs"][1][key] = value
            mutants.append(mutant)
        mutants.extend(({**original, "runs": original["runs"][:1]},
                        {**original, "files": ["wrong-file.el"]}))
        for mutant in mutants:
            with self.subTest(mutant=mutant):
                receipt.write_text(json.dumps(mutant))
                with self.assertRaises(ValueError):
                    validate(self.root, files)
        receipt.write_text(json.dumps(original))
        for name in ("oneshot.stamp.ert.json", "oneshot.stamp.summary.json",
                     "jabber-test-time.stamp.ert.json", "jabber-test-time.stamp.summary.json"):
            path = self.root / ".test-results" / name
            saved = path.read_bytes()
            path.unlink()
            with self.assertRaises(OSError):
                validate(self.root, files)
            path.write_bytes(saved)

    def test_make_zero_without_work_does_not_publish_passed(self):
        deps = self.root / "deps"
        deps.mkdir()
        with patch.dict(os.environ, {"JABBER_MATRIX_DEPS": str(deps)}), \
             patch("subprocess.check_output", return_value="29.1"), \
             patch("subprocess.run", return_value=subprocess.CompletedProcess([], 0)):
            with self.assertRaises(OSError):
                RUNNER["lane"](SOURCE, self.root / "fork", "fork", "/fake/emacs", None)
        self.assertFalse((self.root / "fork/passed").exists())


if __name__ == "__main__":
    unittest.main()
