#!/bin/sh
#count per minute script for Sparkfun Geiger counter https://www.sparkfun.com/products/9848?
#beforehand run in the same directory this command: screen -L /dev/ttyUSB0 9600
#this logs the serial output of the counter to the file screenlog.0

while true; do
        rm screenlog.0
        sleep 60
        echo `date +%s`","`cat screenlog.0 | wc -c` | tee -a cpm.geiger.csv
done
