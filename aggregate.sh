
# makes build outputs available in a coherent way

set -e

# debug:
# set -x

cp=$coreutils/bin/cp
mkdir=$coreutils/bin/mkdir
ln=$coreutils/bin/ln

$mkdir -p $out/bin $out/haddock
$ln -s ${nomadbase_toy}/bin/nomadbase-toy $out/bin
$ln -s ${nomadbase_server}/bin/nomadbase-server $out/bin
$ln -s ${timekeeper}/share/doc/*/* $out/haddock
$ln -s ${netcat}/bin/nc $out/bin/nc
$cp $src/demo-script $out/
