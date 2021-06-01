:: This batch file is meant to be used to run CRMMS in batch mode using preconfigured input files 
::		and an initialized RiverWare model
:: Set CRMMS environment variable CRMMS_DIR to current directory

@echo off
echo ------------------------------------------------
echo This script sets CRMMS_DIR to the directory that 
echo   currently contains this script file. 
echo ------------------------------------------------
echo.
echo Setting local CRMMS_DIR environment variable to:
echo %cd%
echo.
echo ------------------------------------------------
setx CRMMS_DIR "%cd%
echo.
echo ------------------------------------------------
pause