@echo off
brass.exe dcs7.asm -l dcs7.html
rabbitsign -p -t 8xk -f -g -vv dcs7.hex
rem del dcs7.hex
rem pause
