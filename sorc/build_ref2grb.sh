#!/bin/sh
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed
if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi
echo $dir_list

#source ./setlibs.rc  # use this if existing library modules don't quite cover all that is needed.

source ../versions/build.ver
source ../modulefiles/v1.2.0

dir_list='radar_reflectivity_ref2grb.fd'
cd $dir_list
make clean
make -f makefile
ls -l
cd ..
mkdir -p -m 775 ../exec/
cp -p radar_reflectivity_ref2grb.fd/radar_reflectivity_ref2grb ../exec/.
