@echo off
@echo Windows build helper

IF [%1] == []    GOTO OL_EXE
IF [%1] == [all] GOTO OL_EXE

IF [%1] == [ol.exe] (
:OL_EXE
	mint run make ol.exe
)

IF [%1] == [ol32.exe] (
	del /q ol32.exe
	mint run make ol32.exe
	#tools\cv2pdb.exe ol32.exe
)
IF [%1] == [ol64.exe] (
	del /q ol64.exe
	mint run make ol64.exe
	#tools\cv2pdb.exe ol64.exe
)

IF [%1] == [check] (
:CHECK
	mint run WINE= make check
)

:END
