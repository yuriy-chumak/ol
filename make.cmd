@echo off
@echo Windows build helper

IF [%1] == [] (
	GOTO OL_EXE
)

IF [%1] == [ol.exe] (
:OL_EXE
	mint run make ol.exe
)

IF [%1] == [all] (
	GOTO OL_EXE
)

IF [%1] == [check] (
:CHECK
	mint run make check
)

:END
