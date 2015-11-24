for a in $(seq 1 1 100) ; do
cp neutralsim_R.pbs  neutralsim_R_$a.pbs
sed -i -e "s/GTAneutralmodel_1"/"GTAneutralmodel_$a/" neutralsim_R_$a.pbs

done
