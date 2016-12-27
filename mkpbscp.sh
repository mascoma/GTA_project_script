#!/bin/bash

for a in $(seq 16 1 50) ; do

cp runRscript0_1.pbs runRscript$a'_1.pbs' 
sed -i '' 's/GTAmodelpars_0/GTAmodelpars_'$a'/g' runRscript$a'_1.pbs'
cp runRscript0_2.pbs runRscript$a'_2.pbs'
sed -i '' 's/GTAmodelpars_0/GTAmodelpars_'$a'/g' runRscript$a'_2.pbs'

done