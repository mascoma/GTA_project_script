#!/bin/bash

for a in $(seq 1 1 49) ; do

cat /isi/olga/xin/GTA_project/output/20160709/*_$a.txt > /isi/olga/xin/GTA_project/output/20160722/GTAmodels_N$a.txt

done