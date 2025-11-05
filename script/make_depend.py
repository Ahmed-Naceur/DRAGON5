#!/usr/bin/env python
"""
Generate Fortran dependencies from sources (*.f90, *.F90).

Usage:
    - Default: prints a dependency-ordered list of source files.
    - --make-deps: prints Makefile-style dependency rules.
"""

from __future__ import print_function
import sys
import os
import io
import re
import glob

def strip_comment(line):
    idx = line.find('!')
    return line if idx == -1 else line[:idx]

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
        try:
            with io.open(self.name, 'r') as f:
                lines = f.readlines()
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
    exts = (".f90", ".F90", ".f", ".F")
    return [FortranUnit(p) for p in paths if p.lower().endswith(exts)]

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
        remaining.sort(key=lambda x: (0 if not x.module else 1, x.base_lc))
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
            lines.append("{}: {}".format(target, ' '.join(deps)))

    return "\n".join(lines) + ("\n" if lines else "")

def main():
    argv = sys.argv[1:]
    if not argv:
        print("")
        return

    make_deps = False
    if "--make-deps" in argv:
        make_deps = True
        argv = [a for a in argv if a != "--make-deps"]

    paths = []
    for a in argv:
        if any(ch in a for ch in ['*', '?', '[']):
            paths.extend(glob.glob(a))
        else:
            paths.append(a)

    seen = set()
    unique_paths = []
    for p in paths:
        if p not in seen:
            seen.add(p)
            unique_paths.append(p)

    if make_deps:
        sys.stdout.write(generate_make_deps(unique_paths))
    else:
        result = build_dependency_order(unique_paths)
        sys.stdout.write("%s\n" % (" ".join(result)))

if __name__ == '__main__':
    main()
