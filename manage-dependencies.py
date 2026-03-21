#!/usr/bin/env python3
"""
manage-dependencies.py — Unified dependency scanner for Bizard (R + Python + Julia).

Scans all .qmd tutorial files and:
  - Detects Python packages (import/from statements in {python} blocks)
  - Detects Julia packages (using/import statements in {julia} blocks)
  - Updates requirements.txt for Python
  - Reports all discovered dependencies

Usage:
    python manage-dependencies.py --scan          # Scan and report
    python manage-dependencies.py --update        # Scan and update files
"""

import argparse
import re
import sys
from pathlib import Path

PYTHON_IMPORT_TO_PIP = {
    'sklearn': 'scikit-learn', 'cv2': 'opencv-python', 'PIL': 'Pillow',
    'Bio': 'biopython', 'mpl_toolkits': 'matplotlib',
}

# Runtime dependencies required by Quarto for Python execution (not imported in code)
PYTHON_RUNTIME_DEPS = ['jupyter']

PYTHON_STDLIB = {
    'os', 'sys', 're', 'math', 'json', 'csv', 'io', 'pathlib',
    'collections', 'itertools', 'functools', 'typing', 'abc',
    'datetime', 'time', 'random', 'string', 'struct', 'zlib',
    'hashlib', 'copy', 'warnings', 'logging', 'argparse',
    'subprocess', 'shutil', 'glob', 'tempfile', 'textwrap',
    'unittest', 'dataclasses', 'enum', 'contextlib', 'statistics',
}

JULIA_STDLIB = {
    'Base', 'Core', 'Main', 'Printf', 'LinearAlgebra',
    'Statistics', 'Random', 'Dates', 'Test', 'Pkg',
    'InteractiveUtils', 'Markdown', 'REPL', 'Distributed',
    'SparseArrays', 'DelimitedFiles', 'SharedArrays',
}


def extract_code_blocks(content, language):
    pattern = rf'```\{{{language}[^}}]*\}}(.*?)```'
    return re.findall(pattern, content, re.DOTALL)


def scan_python_packages(content):
    blocks = extract_code_blocks(content, 'python')
    packages = set()
    for block in blocks:
        for match in re.finditer(r'^\s*import\s+(\w+)', block, re.MULTILINE):
            packages.add(match.group(1))
        for match in re.finditer(r'^\s*from\s+(\w+)', block, re.MULTILINE):
            packages.add(match.group(1))
    result = set()
    for pkg in packages - PYTHON_STDLIB:
        result.add(PYTHON_IMPORT_TO_PIP.get(pkg, pkg))
    return sorted(result)


def scan_julia_packages(content):
    blocks = extract_code_blocks(content, 'julia')
    packages = set()
    for block in blocks:
        for match in re.finditer(r'^\s*using\s+(.+)$', block, re.MULTILINE):
            for pkg in match.group(1).split(','):
                p = pkg.strip().split(':')[0].strip()
                if p and p not in JULIA_STDLIB:
                    packages.add(p)
        for match in re.finditer(r'^\s*import\s+(\w+)', block, re.MULTILINE):
            pkg = match.group(1)
            if pkg not in JULIA_STDLIB:
                packages.add(pkg)
    return sorted(packages)


def scan_all_qmd(root='.'):
    root = Path(root)
    python_pkgs = set(PYTHON_RUNTIME_DEPS)
    julia_pkgs = set()

    for qmd in sorted(root.rglob('*.qmd')):
        if qmd.name.endswith('.zh.qmd') or 'Template' in qmd.parts or qmd.name.startswith('_'):
            continue
        content = qmd.read_text(encoding='utf-8')
        python_pkgs.update(scan_python_packages(content))
        julia_pkgs.update(scan_julia_packages(content))

    return sorted(python_pkgs), sorted(julia_pkgs)


def update_requirements_txt(packages, filepath='requirements.txt'):
    path = Path(filepath)
    existing = set()
    existing_lines = []

    if path.exists():
        for line in path.read_text().splitlines():
            stripped = line.strip()
            if stripped and not stripped.startswith('#'):
                pkg_name = re.split(r'[>=<!\[]', stripped)[0].strip()
                existing.add(pkg_name.lower())
                existing_lines.append(stripped)

    new_packages = []
    for pkg in packages:
        if pkg.lower() not in existing:
            new_packages.append(pkg)
            existing_lines.append(pkg)

    if new_packages:
        path.write_text('\n'.join(sorted(existing_lines, key=str.lower)) + '\n')
        return new_packages
    return []


def main():
    parser = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--scan', action='store_true', help='Scan and report')
    parser.add_argument('--update', action='store_true', help='Scan and update files')
    parser.add_argument('--root', default='.', help='Root directory')
    args = parser.parse_args()

    if not args.scan and not args.update:
        args.scan = True

    python_pkgs, julia_pkgs = scan_all_qmd(args.root)

    print("=== Dependency Scan Results ===")
    print(f"\nPython packages ({len(python_pkgs)}):")
    for pkg in python_pkgs:
        print(f"  - {pkg}")
    print(f"\nJulia packages ({len(julia_pkgs)}):")
    for pkg in julia_pkgs:
        print(f"  - {pkg}")

    if args.update:
        new_py = update_requirements_txt(python_pkgs)
        if new_py:
            print(f"\nAdded to requirements.txt: {', '.join(new_py)}")
        else:
            print("\nrequirements.txt is up to date")


if __name__ == '__main__':
    main()
