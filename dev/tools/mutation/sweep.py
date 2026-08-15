#!/usr/bin/env python3
"""Mutation sweep for the VM test suite.

Breaks the code one change at a time and reports whether the tests notice.
A mutation is CAUGHT if a test fails, SURVIVED if none do.  What survives is
code the suite does not exercise, whatever its coverage says.

    ./sweep.py setup                 # a worktree to mutate, built once
    ./sweep.py run                   # sweep every module with a test file
    ./sweep.py run vm-save vm-digest # or just these
    ./sweep.py report                # scores per module, survivors by function
    ./sweep.py verify 8              # do survivors survive the whole suite?
    ./sweep.py clean                 # remove the worktree

Everything happens in a detached git worktree, never in your tree.  That is
not tidiness: a sweep killed mid-mutation leaves a broken source file behind,
and `make' will have compiled the mutant into a .elc that outlives restoring
the source -- which is a confusing afternoon.
"""

import argparse, collections, json, os, random, re, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
GEN = os.path.join(HERE, 'gen-mutants.el')
ROOT = subprocess.run(['git', 'rev-parse', '--show-toplevel'], cwd=HERE,
                      capture_output=True, text=True).stdout.strip()
WORK = os.environ.get('VM_MUTATION_DIR',
                      os.path.join(os.environ.get('TMPDIR', '/tmp'), 'vm-mutation'))
TREE = os.path.join(WORK, 'tree')
RESULTS = os.path.join(WORK, 'results.jsonl')
LOCK = os.path.join(WORK, 'lock')

# Sampling: how many mutations to try per module, and how long one test run of
# it may take.  A module whose tests are slow gets fewer, so a sweep stays
# within an hour rather than a day.
BUDGET_FAST, BUDGET_SLOW = 60, 12
SLOW_SECONDS = 10


def run(cmd, cwd, timeout=None):
    return subprocess.run(cmd, cwd=cwd, capture_output=True, text=True,
                          timeout=timeout)


def modules():
    """The modules that have both a lisp file and a test file naming them."""
    found = []
    for name in sorted(os.listdir(os.path.join(TREE, 'lisp'))):
        if not name.endswith('.el'):
            continue
        module = name[:-3]
        if os.path.exists(os.path.join(TREE, 'test', module + '-test.el')):
            found.append(module)
    return found


def setup():
    os.makedirs(WORK, exist_ok=True)
    if not os.path.exists(TREE):
        head = run(['git', 'rev-parse', 'HEAD'], ROOT).stdout.strip()
        run(['git', 'worktree', 'add', '--detach', TREE, head], ROOT)
    # The worktree has sources but no build.  Copy this build's compiled files
    # and the generated ones, then make them newer than the sources: with
    # load-prefer-newer, a checkout that is newer than every .elc means VM
    # loads entirely from source, which turns a 0.4s test run into two
    # minutes.  A mutated file is written after this, so it is newer again and
    # loads from source -- which is what the sweep needs.
    for name in os.listdir(os.path.join(ROOT, 'lisp')):
        if name.endswith('.elc') or name in ('vm-autoloads.el', 'vm-cus-load.el',
                                             'vm-version-conf.el'):
            src = os.path.join(ROOT, 'lisp', name)
            if os.path.exists(src):
                subprocess.run(['cp', src, os.path.join(TREE, 'lisp', name)])
    now = time.time()
    for name in os.listdir(os.path.join(TREE, 'lisp')):
        if name.endswith('.elc'):
            os.utime(os.path.join(TREE, 'lisp', name), (now, now))
    print("worktree ready at %s" % TREE)


def sites_for(module):
    out = run(['emacs', '-Q', '--batch', '-l', GEN, 'lisp/%s.el' % module],
              TREE, timeout=300).stdout
    return [(int(a), int(b), c, d) for a, b, c, d in
            (line.split('|', 3) for line in out.splitlines() if line.count('|') >= 3)]


def time_tests(module):
    start = time.time()
    test_module(module, timeout=600)
    return time.time() - start


def test_module(module, timeout):
    try:
        p = run(['emacs', '-batch', '-Q', '-L', '../lisp', '-l', 'vm-test-init.el',
                 '-l', '%s-test.el' % module, '-f', 'ert-run-tests-batch-and-exit'],
                os.path.join(TREE, 'test'), timeout=timeout)
    except subprocess.TimeoutExpired:
        return 'TIMEOUT', []
    out = p.stdout + p.stderr
    if 'Ran ' not in out:
        return 'BROKEN', []
    failed = sorted(set(re.findall(r'FAILED\s+\d+/\d+\s+(\S+)', out)))
    return ('CAUGHT' if failed else 'SURVIVED'), failed


def sweep(chosen_modules, seed):
    take_lock()
    try:
        out = open(RESULTS, 'a', buffering=1)
        for module in chosen_modules:
            path = os.path.join(TREE, 'lisp', module + '.el')
            original = open(path, encoding='utf-8').read()
            seconds = time_tests(module)
            budget = BUDGET_FAST if seconds < SLOW_SECONDS else BUDGET_SLOW
            # Deleting a (setq x (cdr x)) inside a while loop hangs, and that
            # is an ordinary outcome rather than a fault, so the timeout is
            # kept close to what the module's tests actually need.
            timeout = max(8, int(seconds * 3) + 2)
            sites = sites_for(module)
            rng = random.Random(seed)
            chosen = sites if len(sites) <= budget else rng.sample(sites, budget)
            tally, started = collections.Counter(), time.time()
            try:
                for off, length, repl, desc in chosen:
                    mutant = original[:off] + repl + original[off + length:]
                    open(path, 'w', encoding='utf-8').write(mutant)
                    # Two sweeps writing one file makes every verdict a lie,
                    # so the mutant is read back before it is judged.
                    if open(path, encoding='utf-8').read() != mutant:
                        out.write(json.dumps({'module': module, 'verdict': 'RACED'}) + "\n")
                        continue
                    verdict, failed = test_module(module, timeout)
                    tally[verdict] += 1
                    out.write(json.dumps(
                        {'module': module, 'line': original[:off].count('\n') + 1,
                         'desc': desc, 'was': original[off:off + length][:70],
                         'verdict': verdict, 'failed': failed}) + "\n")
            finally:
                open(path, 'w', encoding='utf-8').write(original)
            print("%-14s %-46s %.0fs" % (module, dict(tally), time.time() - started),
                  flush=True)
    finally:
        drop_lock()


def take_lock():
    os.makedirs(WORK, exist_ok=True)
    try:
        fd = os.open(LOCK, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
        os.write(fd, str(os.getpid()).encode())
        os.close(fd)
    except FileExistsError:
        sys.exit("a sweep holds %s; remove it if that is stale" % LOCK)


def drop_lock():
    if os.path.exists(LOCK):
        os.remove(LOCK)


def defun_at(module, line):
    lines = open(os.path.join(TREE, 'lisp', module + '.el'),
                 encoding='utf-8').read().split('\n')
    for i in range(min(line, len(lines)) - 1, -1, -1):
        m = re.match(r'\((?:cl-)?def(?:un|subst|macro|custom|var|alias)\s+\'?([^ \n()]+)',
                     lines[i])
        if m:
            return m.group(1)
    return '(top level)'


def load_results():
    return [json.loads(line) for line in open(RESULTS)]


def report():
    rows = load_results()
    by_module = collections.defaultdict(list)
    for r in rows:
        by_module[r['module']].append(r)
    print("%-14s %7s %9s %7s %6s  %s" %
          ("module", "caught", "survived", "broken", "hung", "score"))
    worst = []
    for module, rs in sorted(by_module.items()):
        c = collections.Counter(r['verdict'] for r in rs)
        live = c['CAUGHT'] + c['SURVIVED']
        if not live:
            continue
        score = 100.0 * c['CAUGHT'] / live
        print("%-14s %7d %9d %7d %6d  %4.0f%%" %
              (module, c['CAUGHT'], c['SURVIVED'], c['BROKEN'], c['TIMEOUT'], score))
        worst.append((score, module))
    total = collections.Counter(r['verdict'] for r in rows)
    live = total['CAUGHT'] + total['SURVIVED']
    if live:
        print("\n%d judged, %d caught: %.0f%%" %
              (live, total['CAUGHT'], 100.0 * total['CAUGHT'] / live))
    print("\nsurvivors by function:")
    counted = collections.Counter()
    for r in rows:
        if r['verdict'] == 'SURVIVED':
            counted[(r['module'], defun_at(r['module'], r['line']))] += 1
    for (module, fn), n in counted.most_common(15):
        print("  %3d  %-14s %s" % (n, module, fn))


def verify(count, seed):
    """Do survivors survive the whole suite, or only their own test file?"""
    take_lock()
    try:
        survivors = [r for r in load_results()
                     if r['verdict'] == 'SURVIVED' and 'number' not in r['desc']]
        for r in random.Random(seed).sample(survivors, min(count, len(survivors))):
            module = r['module']
            path = os.path.join(TREE, 'lisp', module + '.el')
            original = open(path, encoding='utf-8').read()
            here = [s for s in sites_for(module)
                    if original[:s[0]].count('\n') + 1 == r['line'] and s[3] == r['desc']
                    and original[s[0]:s[0] + s[1]][:70] == r['was']]
            if not here:
                print("could not relocate %s:%d" % (module, r['line']), flush=True)
                continue
            off, length, repl, desc = here[0]
            try:
                open(path, 'w', encoding='utf-8').write(
                    original[:off] + repl + original[off + length:])
                p = run(['emacs', '-batch', '-Q', '-L', 'lisp', '-l', 'test/vm-test-init.el',
                         '-l', 'test/run-tests.el'], TREE, timeout=1800)
                failed = sorted(set(re.findall(r'FAILED\s+\d+/\d+\s+(\S+)',
                                               p.stdout + p.stderr)))
                print("%-12s line %-5d %-26s -> %s" %
                      (module, r['line'], desc,
                       "caught by " + failed[0] if failed else "SURVIVES THE WHOLE SUITE"),
                      flush=True)
            except subprocess.TimeoutExpired:
                print("%-12s line %-5d %-26s -> hung" % (module, r['line'], desc),
                      flush=True)
            finally:
                open(path, 'w', encoding='utf-8').write(original)
    finally:
        drop_lock()


def clean():
    run(['git', 'worktree', 'remove', '--force', TREE], ROOT)
    print("removed %s" % TREE)


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('command', choices=['setup', 'run', 'report', 'verify', 'clean'])
    parser.add_argument('rest', nargs='*')
    parser.add_argument('--seed', type=int, default=20260814)
    args = parser.parse_args()
    if args.command == 'setup':
        setup()
    elif args.command == 'run':
        if not os.path.exists(TREE):
            setup()
        sweep(args.rest or modules(), args.seed)
    elif args.command == 'report':
        report()
    elif args.command == 'verify':
        verify(int(args.rest[0]) if args.rest else 8, args.seed)
    elif args.command == 'clean':
        clean()


if __name__ == '__main__':
    main()
