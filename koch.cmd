@echo off
rem Convenience script for Windows users to run koch.py

rem Use Python installed via the official installer if it exist
rem Otherwise use python3, which will be the MS Store version
where /q py
if ERRORLEVEL 1 (
  python3 %~dpn0.py %*
) ELSE (
  py -3 %~dpn0.py %*
)
