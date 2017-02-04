command=`sed -n '3p' ${1} | sed 's/-- //'`
eval $command $1 $2 $3
