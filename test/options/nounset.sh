# exit-with=1
defined=123

echo $defined
echo $undefined

echo "enabling nounset"
set -u

echo $defined
echo $undefined

echo "unreachable"

