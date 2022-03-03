#!/bin/bash -login

#module load envvar/1.0
#module load PrgEnv-intel/8.1.0
#module load intel/19.1.3.304
#module load craype
#module load cray-mpich
#module load grib_util/1.2.2
#module load prod_util/2.0.5
#module load prod_envir/2.0.4
#module load cray-python

source ~/.bashrc

cd /lfs/h2/emc/da/noscrub/Shun.Liu/nwprod/radarl2.v1.2.1/ecf/dev

qsub jradar_level2.ecf 
