# disable-output-check
# Environment-dependent variables
echo "\$! ---------"
ls &
echo $!

echo "\$\$ ---------"
echo $$

wait
