#!/usr/bin/env sh
set -eu

# HDF5 embeds non-semantic metadata, so bytewise comparison is not a provenance
# check. Generate outside the checkout and compare every dataset with h5diff.
repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture_dir="$repo_dir/imas-python-fixtures"
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/imas-fixture-verify.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

"$fixture_dir/.venv/bin/python" "$fixture_dir/equilibrium_seed.py" "$work_dir"

for version in 3.39.0 4.1.1; do
  for name in equilibrium master; do
    h5diff -q \
      "$fixture_dir/fixtures/dd-$version/$name.h5" \
      "$work_dir/dd-$version/$name.h5"
  done
done
