#!/bin/sh

date
echo "------------------------------------------------"
echo "This script is to convert 3D reflectivity mosaic in "
echo "binary format to GRIB format "
echo "Author: Shun Liu"
echo "NCEP/EMC Feb 2009"
echo "------------------------------------------------"

set -xa

msg="Start conversion of reflectivity mosaic from binary to GRIB."
postmsg "$jlogfile" "$msg"

####################################
# ref2grb areas
####################################
FIRST=`echo $mydate | cut -c1-8`
SECOND=`echo $mydate | cut -c9-10`
YY=`echo $mydate | cut -c3-4`
MM=`echo $mydate | cut -c5-6`
DD=`echo $mydate | cut -c7-8`
HH=`echo $mydate | cut -c9-10`
MN='00'

BINdir=${COMIN}/tile

cd $DATA

echo ${FIRST}_${SECOND}00 > card
echo $BINdir >> card
echo $YY,$MM,$DD,$HH,$MN > tmp

#mosaic_file=${FIRST}_${SECOND}00.mosaic
#mosaic_input=${COMIN}/tile7/${mosaic_file}

#ic=1
#while [ $ic -lt 30 ]
#  do
#    if [ -s ${mosaic_input} ]
#     then
#        #$utilexec/fsync_file ${mosaic_input} # synchronize the nodes
#        sleep 10
#        break
#     else
#       let "ic=ic+1"
#       sleep 10
#     fi
#     if [ $ic -ge 30 ]
#     then
#       err_exit "COULD NOT LOCATE:${mosaic_input}"
#     fi
#  done

mosaic_file=${FIRST}_${SECOND}00.mosaic
files=`find ${BINdir}* -name $mosaic_file |wc -l`
  
if [ $files -eq 8 ]; then
    #mpirun -l ${EXECradar}/radar_reflectivity_ref2grb < tmp > test.log
    mpiexec -n 8 -ppn 8 ${EXECradar}/radar_reflectivity_ref2grb < tmp > test.log
    export err=$?; err_chk
else
    # echo "WARNING: the number of SRC tiles is not 8 and ref2grb not start" 
    echo "FATAL ERROR: the number of SRC tiles is not 8 and ref2grb not start" 
    echo "Number of mosaic files is $files"
    export err=1; err_chk
fi

###################################
# Convert to grib1
###################################

$CNVGRIB -g21 refd3d.t${HH}z.grb2f00 refd3d.t${HH}z.grbf00
cp refd3d.t${HH}z.grbf00 ttt.grb

####################################
# end ref2grb areas
####################################

###################################
# plot figures
###################################

run=conus4km

LLAT=21.0
LLON=-121.0
ULAT=49.0
ULON=-62.6

size1=1000
size2=800

### 1 h precip, 3 h precip only

datestr=`date`
echo DONE wgribbing $hr at $datestr

$GEMEXE/nagrib << endgrib
 GBFILE   = ttt.grb
 INDXFL   =
 GDOUTF   = today_1hprecip.grd
 PROJ     =                 
 GRDAREA  =
 KXKY     =
 MAXGRD   = 4999
 CPYFIL   = GDS
 GAREA    = dset
 OUTPUT   = T
 GBTBLS   =
 GBDIAG   =
 PDSEXT   = NO

r

exit

endgrib

datestr=`date`
echo DONE nagribbing $hr at $datestr

#cp $FIXgempak/coltbl.xwp.wbg coltbl.xwp
cp $FIXgempak/colors/coltbl.xwp coltbl.xwp

gdplot << endplot

 GDFILE = today_1hprecip.grd
 GDATTIM  = f
 GLEVEL   = 0
 GVCORD   = none
 PANEL    = 0/4//1
 SKIP     = /9
 SCALE    = 0
 GFUNC    = p01i
 CTYPE    = f
 CONTUR   = 0
 CINT     = 2
 LINE     = 2/-12/1/1
 FINT     = 0.01;5;10;15;20;25;30;35;50;75;100;125;150;175
 FINT     = 0.01;.05;.1;.15;.2;.3;.5;.75;1;1.5;2
 FLINE    = 0;23;22;21;20;19;10;17;16;15;14;29;28;24;25
 HILO     =
 HLSYM    =
 CLRBAR   = 1/V/LL/.005;.02/.9;.013|.82/2/.82/111/SW
 CLRBAR   = 1/H/LL/.005;.02/.8;.01|.82/2/.82/111/SW
 CLRBAR   = 1/H/LL/.07;.02/.85;.006|.82/2/.82/111/SW
 CLRBAR   = 1/H/LL/.07;.024/.85;.007|.82/2/.82/111/SW
 GVECT    =
 REFVEC   =
 TITLE    = 1/0/ ^ P01I, EMSL, 10 m winds
 TEXT     = 1/1//SW
 CLEAR    = y
 GAREA    = dset
 garea    = $LLAT;$LLON;$ULAT;$ULON
 IJSKIP   = 1;1
 PROJ     = lcc/39;-98;39
 MAP      = 1/12/1
 LATLON   = 0
 DEVICE   =
 STNPLT   =

r

 GFUN    = REFC
cle=y
ctype=f
gvect=
fint=5;10;15;20;25;30;35;40;45;50;55;60;65
fline=0;25;24;4;21;22;23;5;18;17;15;14;28;29
DEVICE = gif|refc_obs.gif|$size1;$size2|C
title=1/0/
r

exit

endplot

gpend

datestr=`date`
echo DONE plotting $hr at $datestr
                                          
###################################
# plot figures
###################################

mv refc_obs.gif refc_obs_${mydate}.gif
rm -f ttt.grb
cp refc_obs_${mydate}.gif ${COMOUT}
mv refd3d.t${HH}z.grbf00 ${COMOUT}
mv refd3d.t${HH}z.grb2f00 ${COMOUT}

# GOOD RUN
set +x
echo " "
echo " ****** CONVERSION OF REFLECTIVITY MOSAIC FROM BINARY TO GRIB COMPLETED NORMALLY"
echo " ****** CONVERSION OF REFLECTIVITY MOSAIC FROM BINARY TO GRIB COMPLETED NORMALLY"
echo " ****** CONVERSION OF REFLECTIVITY MOSAIC FROM BINARY TO GRIB COMPLETED NORMALLY"
echo " ****** CONVERSION OF REFLECTIVITY MOSAIC FROM BINARY TO GRIB COMPLETED NORMALLY"
echo " "
set -x

sleep 10

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

