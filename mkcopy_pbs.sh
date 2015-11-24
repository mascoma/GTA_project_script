for a in $(seq 1 1 50) ; do
cp model_XaXnXr_2.pbs  model_XaXnXr_2_run$a.pbs
sed -i '' 's/input_2_3/input_2_3_run'$a'/g'  model_XaXnXr_2_run$a.pbs

done
