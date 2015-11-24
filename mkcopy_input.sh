for a in $(seq 1 1 50) ; do
cp input_2_3.txt  input_2_3_run$a.pbs
sed -i '' 's/simulation_XaXnXr_2_3/simulation_XaXnXr_2_3_run'$a'/g'  input_2_3_run$a.pbs

done
