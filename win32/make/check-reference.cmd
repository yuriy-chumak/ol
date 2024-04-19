@echo off
echo Testing reference samples:
::set files=
for /f "delims=" %%a in ('dir /b doc/reference/*.md') do (
	call set files=%%files%% doc/reference/%%a
	)
ol.exe tools/check-reference.lisp %files%
exit