for a in $(seq 1 1 100) ; do
cp GTAneutralmodel_0.R  GTAneutralmodel_$a.R
echo "save.image(file=\"nenutralmodel-lpestimation"$a".RData\")" >> GTAneutralmodel_$a.R

done
