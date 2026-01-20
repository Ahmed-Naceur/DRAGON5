#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Generate Fortran dependencies from sources (*.f90, *.F90).

Usage:
    - Default: prints a dependency-ordered list of source files.
    - --make-deps: prints Makefile-style dependency rules.
"""

import sys
import os
import re
import glob

def strip_comment(line):
    idx = line.find('!')
    if idx == -1:
        return line
    else:
        return line[:idx]

class FortranUnit(object):
    USE_RE = re.compile(r"^\s*use\s+([a-z0-9_]+)", re.IGNORECASE)
    MODULE_RE = re.compile(r"^\s*module\s+([a-z0-9_]+)", re.IGNORECASE)
    MODULE_PROC_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
    END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
    CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
    INCLUDE_RE = re.compile(r"^\s*include\s*[\'\"]([^\'\"]+)[\'\"]", re.IGNORECASE)

    def __init__(self, path):
        self.name = path
        self.base = os.path.basename(path)
        self.base_lc = self.base.lower()
        self.module = None
        self.uses = []
        self.includes_in_contains = []
        self._parse()

    def _parse(self):
        in_module = False
        in_contains = False
        lines = []
        try:
            f = open(self.name, 'r')
            try:
                lines = f.readlines()
            finally:
                f.close()
        except IOError:
            return
        for raw in lines:
            line = strip_comment(raw).strip('\n')
            if not line:
                continue
            low = line.lower()
            if self.MODULE_RE.match(low) and not self.MODULE_PROC_RE.match(low):
                m = self.MODULE_RE.match(low)
                if m:
                    self.module = m.group(1)
                    in_module = True
                    in_contains = False
                continue
            if in_module and self.CONTAINS_RE.match(low):
                in_contains = True
                continue
            if in_module and self.END_MODULE_RE.match(low):
                in_module = False
                in_contains = False
                continue
            m_use = self.USE_RE.match(low)
            if m_use:
                mod = m_use.group(1)
                if mod != 'intrinsic' and mod not in self.uses:
                    self.uses.append(mod)
                continue
            if in_module and in_contains:
                m_inc = self.INCLUDE_RE.match(low)
                if m_inc:
                    inc = m_inc.group(1).strip()
                    self.includes_in_contains.append(os.path.basename(inc))

def build_units(paths):
    exts = [".f90", ".F90", ".f", ".F"]
    units = []
    for p in paths:
        base = os.path.basename(p)
        if base.startswith("."):
            continue
        low = p.lower()
        for ext in exts:
            if low.endswith(ext):
                units.append(FortranUnit(p))
                break
    return units

def build_dependency_order(paths):
    units = build_units(paths)
    by_base_lc = dict((u.base_lc, u) for u in units)
    module_to_unit = dict((u.module, u) for u in units if u.module)
    included_to_container = {}
    for u in units:
        if u.module and u.includes_in_contains:
            for inc in u.includes_in_contains:
                included_to_container.setdefault(inc.lower(), set()).add(u)

    edges = dict((u, set()) for u in units)
    indeg = dict((u, 0) for u in units)

    def add_edge(src, dst):
        if src is dst:
            return
        if dst not in edges[src]:
            edges[src].add(dst)
            indeg[dst] += 1

    for u in units:
        for mod in u.uses:
            v = module_to_unit.get(mod)
            if not v:
                continue
            included_containers = included_to_container.get(u.base_lc, set())
            if v in included_containers:
                continue
            add_edge(v, u)

    for container in units:
        if container.module and container.includes_in_contains:
            for inc in container.includes_in_contains:
                inc_u = by_base_lc.get(inc.lower())
                if inc_u:
                    add_edge(inc_u, container)

    ordered = []
    zero = [u for u in units if indeg[u] == 0]
    zero.sort(key=lambda x: x.base_lc)
    while zero:
        n = zero.pop(0)
        ordered.append(n)
        for m in list(edges[n]):
            edges[n].remove(m)
            indeg[m] -= 1
            if indeg[m] == 0:
                zero.append(m)
        zero.sort(key=lambda x: x.base_lc)

    remaining = [u for u in units if u not in ordered]
    if remaining:
        def sort_key(x):
            if not x.module:
                return (0, x.base_lc)
            else:
                return (1, x.base_lc)
        remaining.sort(key=sort_key)
        ordered.extend(remaining)

    excluded_bases = set()
    for u in units:
        if u.module and u.includes_in_contains:
            for inc in u.includes_in_contains:
                excluded_bases.add(inc.lower())

    return [u.name for u in ordered if u.base_lc not in excluded_bases]

def make_obj_name(path):
    base, ext = os.path.splitext(path)
    return base + ".o"

def generate_make_deps(paths):
    units = build_units(paths)
    if not units:
        return ""
    by_base_lc = dict((u.base_lc, u) for u in units)
    module_to_unit = dict((u.module, u) for u in units if u.module)
    included_to_container = {}
    for u in units:
        if u.module and u.includes_in_contains:
            for inc in u.includes_in_contains:
                included_to_container.setdefault(inc.lower(), set()).add(u)

    lines = []
    for u in units:
        deps_objs = set()
        deps_files = set()

        for mod in u.uses:
            v = module_to_unit.get(mod)
            if not v:
                continue
            included_containers = included_to_container.get(u.base_lc, set())
            if v in included_containers:
                continue
            if v is not u:
                deps_objs.add(make_obj_name(v.name))

        if u.includes_in_contains:
            for inc in u.includes_in_contains:
                inc_u = by_base_lc.get(os.path.basename(inc).lower())
                if inc_u:
                    deps_files.add(inc_u.name)
                else:
                    deps_files.add(inc)

        if deps_objs or deps_files:
            target = make_obj_name(u.name)
            deps = sorted(deps_objs) + sorted(deps_files)
            lines.append("%s: %s" % (target, ' '.join(deps)))

    result = "\n".join(lines)
    if lines:
        result += "\n"
    return result

def main():
    argv = sys.argv[1:]
    if not argv:
        print("")
        return

    make_deps = False
    cleaned_argv = []
    for a in argv:
        if a == "--make-deps":
            make_deps = True
        else:
            cleaned_argv.append(a)

    paths = []

    def has_glob_chars(s):
        # Python 2.4 compatible: no any(), no generator expressions.
        return (s.find('*') != -1) or (s.find('?') != -1) or (s.find('[') != -1)

    def accept_path(p):
        lowp = p.lower()
        if make_deps:
            return lowp.endswith('.f90') or lowp.endswith('.F90') or lowp.endswith('.f') or lowp.endswith('.F')
        return lowp.endswith('.f90')

    if cleaned_argv:
        for a in cleaned_argv:
            if has_glob_chars(a):
                for m in glob.glob(a):
                    if accept_path(m):
                        paths.append(m)
            else:
                if accept_path(a):
                    paths.append(a)
    if not paths:
        try:
            cwd = os.getcwd()
            candidates = os.listdir('.')
        except OSError:
            candidates = []
        for name in candidates:
            if accept_path(name):
                paths.append(name)
    if not paths:
        try:
            sys.stderr.write('[make_depend.py] Warning: no Fortran sources found in arguments or current directory.\n')
        except:
            pass

    seen = set()
    unique_paths = []
    for p in paths:
        if p not in seen:
            seen.add(p)
            unique_paths.append(p)

    if make_deps:
        deps = generate_make_deps(unique_paths)
        sys.stdout.write(deps)
    else:
        result = build_dependency_order(unique_paths)
        sys.stdout.write("%s\n" % (" ".join(result)))

if __name__ == '__main__':
    main()
