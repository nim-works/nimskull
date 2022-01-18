#!/usr/bin/env python3

# Script to bootstrap `koch` if necessary
#
# Copyright (c) 2021 Leorize <leorize+oss@disroot.org>
#
# Licensed under the terms of the MIT license. See "license.txt" included in
# this distribution for more information.

from pathlib import Path
from typing import Any, ClassVar, Optional
import os
import shutil
import subprocess
import sys

# Nim source directory, which is the directory this script resides in
NimSource = Path(__file__).resolve(strict=True).parent

# The nimcache folder
Nimcache = NimSource / "nimcache"


def exe(p: Path) -> Path:
    """
    Add the ".exe" extension if needed for Windows
    """
    if sys.platform.startswith("win32"):
        if p.suffix != ".exe":
            return p.with_name(p.name + ".exe")
        else:
            return p
    else:
        return p


def run(*args: Any, cwd: Optional[Path] = None, replace=False) -> None:
    """
    Print and run the command described by `args`, raising on failure.

    If `replace` is set to `True`, simulate a process replacement by running
    the command and quit with its exit code. This is done to support Windows,
    of which os.execv have a different semantics.
    """
    # Stringify everything so we don't have to do this site-by-site
    run_args = [str(x) for x in args]
    print("Running:", run_args, flush=True)
    completed = subprocess.run(run_args, check=(not replace), cwd=cwd)
    if replace:
        quit(completed.returncode)


def capture(*args: Any) -> subprocess.CompletedProcess:
    """Run the command described by `args` and capture its output as UTF-8"""
    # Stringify everything so we don't have to do this site-by-site
    run_args = [str(x) for x in args]
    return subprocess.run(run_args, capture_output=True, encoding="utf-8")


class Bootstrap:
    """This class manages bootstrap compilers, including building and downloading them."""

    # The bootstrap source location
    Source: ClassVar = NimSource / "build" / "csources"

    # The bootstrap compiler location
    Compiler: ClassVar = exe(NimSource / "bin" / "nim-boot")

    # The bootstrap compiler binary store location
    Binaries: ClassVar = Source / "bin"

    # The location of build configuration
    ConfigPath: ClassVar = NimSource / "config" / "build_config.txt"

    # The location of bootstrap source release file
    SourceReleasePath: ClassVar = Source / "csources-release"

    def __init__(self) -> None:
        """Load configuration and initialize the object"""

        self.sourceUrl = ""
        self.sourceCommit = ""
        self.sourceRelease = ""

        # The tag used for the versioned bootstrap compiler
        versioned_bin_tag = ""

        try:
            with open(Bootstrap.SourceReleasePath) as releaseFile:
                self.sourceRelease = releaseFile.read().strip()
                versioned_bin_tag = self.sourceRelease
        except OSError:
            # There are no release csources bundled
            pass

        # Read build configuration if there are no release csources bundled
        if self.sourceRelease == "":
            with open(Bootstrap.ConfigPath) as config:
                for line in config:
                    (key, _, value) = line.rstrip().partition("=")

                    if key == "nim_csourcesUrl":
                        self.sourceUrl = value
                    elif key == "nim_csourcesHash":
                        self.sourceCommit = value

            if self.sourceUrl == "":
                raise RuntimeError(
                    "Could not find the configuration for bootstrap source URL"
                )

            if self.sourceCommit == "":
                raise RuntimeError(
                    "Could not find the configuration for bootstrap source commit"
                )

            versioned_bin_tag = self.sourceCommit

        # The name of the compiler binary in the store
        self.versioned_bin = exe(Bootstrap.Binaries / ("nim" + "-" + versioned_bin_tag))

    def is_built(self) -> bool:
        """Returns whether the bootstrap exist in the store"""
        return self.versioned_bin.is_file()

    @classmethod
    def _git_capture(self, *args: Any) -> subprocess.CompletedProcess:
        """Runs git at the source folder and capture its outputs"""
        return capture("git", "-C", self.Source, *args)

    @classmethod
    def _git(self, *args: Any) -> None:
        """Runs git at the source folder, raise an exception on error"""
        run("git", "-C", self.Source, *args)

    def fetch(self) -> None:
        """Download the bootstrap compiler source if needed"""
        # There is no need to fetch if the release source is used
        if self.sourceRelease != "":
            return

        localSourceUrl = Bootstrap._git_capture(
            "remote", "get-url", "origin"
        ).stdout.rstrip()

        if localSourceUrl != self.sourceUrl:
            if localSourceUrl != "":
                print(
                    f"Bootstrap source URL ({localSourceUrl}) differs from configuration ({self.sourceUrl})"
                )
                print(f"Removing bootstrap source at {Bootstrap.Source} and refetch")
                shutil.rmtree(Bootstrap.Source)

            print("Fetching bootstrap compiler source")
            run(
                "git",
                "clone",
                "--depth=1",
                "--single-branch",
                self.sourceUrl,
                Bootstrap.Source,
            )

        localCommit = Bootstrap._git_capture(
            "rev-parse", "--verify", "HEAD"
        ).stdout.rstrip()

        if localCommit != self.sourceCommit:
            print(
                f"Local commit ({localCommit}) differs from configuration ({self.sourceCommit})"
            )

            print(
                f"Downloading and checking out the bootstrap commit ({self.sourceCommit})"
            )
            Bootstrap._git("fetch", "origin", self.sourceCommit)
            Bootstrap._git("checkout", self.sourceCommit)

    def build(self) -> None:
        """Build the bootstrap compiler if necessary"""
        if not self.is_built():
            print(f"Bootstrap compiler {self.versioned_bin} not found, building")
            self.fetch()

            if sys.platform.startswith("win32"):
                # Make on Windows is a hit and miss, use the batch file here instead.
                run(Bootstrap.Source / "build.bat", cwd=Bootstrap.Source)
            else:
                makeprg = "make"
                if shutil.which("gmake") is not None:
                    makeprg = "gmake"

                # Check if this is GNU make
                is_gnu_make = False
                try:
                    is_gnu_make = capture(makeprg, "--version").stdout.startswith(
                        "GNU Make"
                    )
                except OSError:
                    # Guess we don't even have make
                    pass

                if is_gnu_make:
                    cpus = os.cpu_count()
                    # If the number of CPUs can't be detected, default to 1.
                    if cpus is None:
                        cpus = 1

                    run(makeprg, "-s", "-C", Bootstrap.Source, "-j", cpus)
                else:
                    # If GNU make is not found, use the (slower) build script
                    run(
                        "sh",
                        Bootstrap.Source / "build.sh",
                        cwd=Bootstrap.Source,
                    )

            # Copy the built binary into a "versioned" variant
            shutil.copy(exe(Bootstrap.Binaries / "nim"), self.versioned_bin)

        print(f"Instantiating {self.versioned_bin} as {Bootstrap.Compiler}")
        shutil.copy(self.versioned_bin, Bootstrap.Compiler)

    def run(self, *args) -> None:
        """Build (if needed) and run the bootstrap compiler, raise on error"""
        self.build()
        run(Bootstrap.Compiler, *args)


def main() -> None:
    bootstrap = Bootstrap()

    # Implements the fetch-bootstrap command. See koch.nim for more
    # information.
    if len(sys.argv) > 1 and sys.argv[1] == "fetch-bootstrap":
        bootstrap.fetch()
        return

    # In case --help is passed before the bootstrap compiler is built, warn the
    # user about it.
    if "-h" in sys.argv or "--help" in sys.argv:
        if not bootstrap.is_built():
            print(
                "Warning: koch and its dependencies will be downloaded and built before --help is processed."
            )

    print("Building koch.nim")

    # The path to koch's binary
    kochSourceDir = NimSource / "tools" / "koch"
    kochBinary = exe(kochSourceDir / "koch")
    kochSource = kochSourceDir / "koch.nim"
    bootstrap.run(
        "c",
        "--hints:off",
        # (as of v1.0.11) This is required to silent "CC:" even with --hints:off.
        "--hint:CC:off",
        # Silence "UnknownMagic" warnings that are common place due to the
        # bootstrapping compiler being older than the stdlib.
        "--warning:UnknownMagic:off",
        # Prevent users configuration and/or configuration placed in a parent
        # directory from interfering, as they might specify flags that are not
        # in the bootstrap compiler.
        "--skipParentCfg",
        "--skipUserCfg",
        f"--nimcache:{Nimcache}/koch",
        f"--out:{kochBinary}",
        kochSource,
    )

    print("Running koch.nim")
    # Tell koch.nim where the source code is
    os.environ["KOCH_NIM_SOURCE"] = str(NimSource)
    run(kochBinary, *sys.argv[1:], replace=True)


if __name__ == "__main__":
    main()
