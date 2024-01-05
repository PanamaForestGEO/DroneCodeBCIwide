@echo off
set loopcount=844
:loop
echo Iter %loopcount%
CloudCompare -SILENT -AUTO_SAVE OFF -O -GLOBAL_SHIFT -620000.00 -1010000.00 0 "path\to\CURRENT\cloud\cloud_%LOOPCOUNT%.laz"  -O -GLOBAL_SHIFT -620000.00 -1010000.00 0  "path\to\PREVIOUS\cloud\BCI18r_%LOOPCOUNT%.laz" -ICP -OVERLAP 98 -RANDOM_SAMPLING_LIMIT 100000 -C_EXPORT_FMT LAS -SAVE_CLOUDS FILE  "path\to\OUTPUT\cloud\cloud_%LOOPCOUNT%.las <path\to\PREVIOUS\cloud\BCI18r_%LOOPCOUNT%.las" -CLEAR
set /a loopcount=loopcount-1
if %loopcount%==0 goto exitloop
goto loop
:exitloop
pause
