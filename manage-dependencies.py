#!/usr/bin/env python3
"""
manage-dependencies.py — Unified dependency scanner for Bizard (R + Python + Julia).

Scans all .qmd tutorial files and:
  - Detects Python packages (import/from statements in {python} blocks)
  - Detects Julia packages (using/import statements in {julia} blocks)
  - Updates requirements.txt for Python
  - Updates Julia/Project.toml for Julia
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


# Known Julia package UUIDs for Project.toml generation
JULIA_PACKAGE_UUIDS = {
    'CairoMakie': '13f3f980-e62b-5c42-98c6-ff1f3baf88f0',
    'DataFrames': 'a93c6f00-e57d-5684-b7b6-d8193f3e46c0',
    'Statistics': '10745b16-79ce-11e8-11f9-7d13ad32a3b2',
    'Random': '9a3f8284-a2c9-5f02-9a11-845980a1fd5c',
    'Makie': 'ee78f7c6-11fb-53f2-987a-cfe4a2b5a57a',
    'GLMakie': 'e9467ef8-e4e7-5192-8a1a-b1aee30e663a',
    'WGLMakie': '276b4fcb-3e11-5398-bf8b-a0c2d153d008',
    'Plots': '91a5bcdd-55d7-5caf-9e0b-520d859cae80',
    'StatsPlots': 'f3b207a7-027a-5e70-b257-86293d7955fd',
    'CSV': '336ed68f-0bac-5ca0-87d4-7b16caf5d00b',
    'DataFramesMeta': '1313f7d8-7da2-5740-9ea0-a2ca25f37964',
    'Colors': '5ae59095-9a9b-59fe-a467-6f913c188581',
    'ColorSchemes': '35d6a980-a343-548e-a6ea-1d62b119f2f4',
    'StatsBase': '2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91',
    'Distributions': '31c24e10-a181-5473-b8eb-7969acd0382f',
    'KernelDensity': '5ab0869b-81aa-558d-bb23-730f6a60dcf1',
    'AlgebraOfGraphics': 'cbdf2221-f076-402e-a563-3d30da359d67',
}


def update_julia_project_toml(packages, filepath='Julia/Project.toml'):
    """Update Julia/Project.toml with discovered packages."""
    path = Path(filepath)
    existing = {}

    if path.exists():
        for line in path.read_text().splitlines():
            line = line.strip()
            if '=' in line and not line.startswith('['):
                parts = line.split('=', 1)
                pkg_name = parts[0].strip()
                uuid = parts[1].strip().strip('"')
                existing[pkg_name] = uuid

    new_packages = []
    for pkg in packages:
        if pkg not in existing and pkg in JULIA_PACKAGE_UUIDS:
            existing[pkg] = JULIA_PACKAGE_UUIDS[pkg]
            new_packages.append(pkg)

    if new_packages:
        lines = ['[deps]']
        for name in sorted(existing):
            lines.append(f'{name} = "{existing[name]}"')
        lines.append('')
        path.write_text('\n'.join(lines))

    return new_packages


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

        new_jl = update_julia_project_toml(julia_pkgs)
        if new_jl:
            print(f"Added to Julia/Project.toml: {', '.join(new_jl)}")
        else:
            print("Julia/Project.toml is up to date")


if __name__ == '__main__':
    main()
