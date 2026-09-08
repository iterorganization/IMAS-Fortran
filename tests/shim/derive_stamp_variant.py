"""Derive one stamp-state fixture from the checked-in DD 3 pulse.

The source pulse remains untouched.  Only the scalar dataset that stores the
occurrence DD-version stamp changes in each copied pulse.
"""

import argparse
import shutil
from pathlib import Path

import h5py


STAMP_DATASET = "/equilibrium/ids_properties&version_put&data_dictionary"

# A grammar-valid, known DD release (see the shim's own DdVersion parser and
# its KNOWN_RELEASES table) that is neither the HLI version (4.1.1) nor the
# one version the shipped artifact covers (3.39.0).  A stamp naming this
# version parses fine but matches no rule, so it derives the
# mismatched-with-no-artifact scenario rather than the malformed one.
MISMATCH_STAMP = "3.40.0"


def derive(source: Path, destination: Path, state: str) -> None:
    if source.resolve() == destination.resolve():
        raise ValueError("stamp variant destination must differ from its source")

    shutil.rmtree(destination, ignore_errors=True)
    shutil.copytree(source, destination)

    with h5py.File(destination / "equilibrium.h5", "r+") as pulse:
        if STAMP_DATASET not in pulse:
            raise KeyError(f"source pulse has no DD-version stamp: {STAMP_DATASET}")
        if state == "absent":
            del pulse[STAMP_DATASET]
        elif state == "malformed":
            pulse[STAMP_DATASET][()] = "not-a-dd-version"
        else:
            pulse[STAMP_DATASET][()] = MISMATCH_STAMP


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("source", type=Path)
    parser.add_argument("destination", type=Path)
    parser.add_argument("state", choices=("absent", "malformed", "mismatch"))
    args = parser.parse_args()
    derive(args.source, args.destination, args.state)


if __name__ == "__main__":
    main()
