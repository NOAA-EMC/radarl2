#!/bin/bash
#set -ax
pwd=`pwd`;

source ../versions/build.ver
source ../modulefiles/v1.2.0

cd $pwd/wsr88d_level2.fd
make clean
rm ncep_vr_qc_lib.a  nssl_reflec_src_lib.a

cd $pwd/wsr88d_level2.fd/level2_qc_lib
cd nssl_reflec_src.fd; 
make clean
make

cd ../ncep_vr_qc.fd; 
make clean
make

cd $pwd/wsr88d_level2.fd/; 
make

cp wsr88d_level2 ../../exec 
