@echo off

if "%1" == "" goto fail

grep -r -E -n "^%1\([a-zA-Z,_ ]*\) *:-" *
goto exit
:fail
echo usage: findrule rulename
:exit