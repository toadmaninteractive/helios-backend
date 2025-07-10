@echo off
setlocal enableextensions enabledelayedexpansion

set out_dir="generated"

bin\igorc.exe -d -v -erlang ^
  -p "igor" ^
  -o %out_dir% ^
  *.igor

if errorlevel 1 pause

endlocal
