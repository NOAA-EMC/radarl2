#!/bin/sh
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

source ../versions/build.ver
source ../modulefiles/v1.2.0

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi
echo $dir_list

#source ./setlibs.rc  # use this if existing library modules don't quite cover all that is needed.

dir_list='radar_reflectivity_mosaic.fd'
cd $dir_list
make clean
make -f Makefile
ls -l
cd ..
cp -p radar_reflectivity_mosaic.fd/radar_reflectivity_mosaic ../exec/.
