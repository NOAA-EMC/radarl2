#!/bin/sh

set -xa
date

echo "------------------------------------------------"
echo "Executing exradar_level2.sh.sms XXXX "
echo "This script uses all the level 2 radar stations "
echo "compressed bzip (full volume scan) data, uncompresses "
echo "quality controls the radial and reflectivity data and "
echo "creates SRC transformed reflectivity data for processing "
echo "mosiacing codes and saves the qc-ed reflectivity and radial "
echo "wind output data in NCEP dcom bufr tanks. At present 1 node "
echo "is used with a maximum of 30 tasks. In each task "
echo "the all the processing steps above - decoding and QC are "
echo "performed for the selected list of radar stations "
echo "The complete list of bufr output files are concatenated in each "
echo "task. In the main JOB script all the individual concatenated bufr " 
echo "files in each task are further concatenated into a single total "
echo "bufr file which is the input to the call to the tranjb script "
echo "to write into radar level 2 bufr tanks on ${DCOMROOT} "
echo "This script uses poe features with 1 node (30 tasks) "
echo "Radial Wind QC, Reflectivity QC and SRC processing are "
echo "codes derived from NSSL/University of Oklohoma and further "
echo "modified by Shun Liu of NCEP/EMC/Mesoscale Modeling Branch "
echo "Author: V. Krishna Kumar"
echo "NCEP/NCO/Systems Integration Branch May 2008"
echo "------------------------------------------------"

##  extend processing time window from 1/2 hour to 2 hour
##  limit data volume to be processed to 5 in a job
##  Shun Liu /NCEP/EMC   07/31/2012 

ntask=$1
echo "running ntask=$ntask"
export PS4='NTASK${ntask}:$SECONDS + '

set -x
##
##---------------------------------------------------------------------
##
       for stng in `cat file.station_$ntask`; do ##### all stations in a single task
          cfiles=`find ${COMIN}/${stng} -name '*bz2' -exec ls -1 {} \; | wc -l`
          cfilesm1=`expr $cfiles - 1`
          find ${COMIN}/${stng} -name '*bz2' -exec ls -1 {} \; | sort | head -$cfilesm1 > current_files_${stng}

          new_files_found=YES

          if [ -s $DCOM_IN/radar_level2_${stng}.history ] ; then


#  Sort list of files in history file (files that have been processed)
#  -------------------------------------------------------------------

   sort -d -o radar_level2_${stng}.history_tempsort $DCOM_IN/radar_level2_${stng}.history
   if [ $? -eq 0 ] ; then
      awk ' { print $1 } ' radar_level2_${stng}.history_tempsort > \
                           $DCOM_IN/radar_level2_${stng}.history
   else
      msg="**FAILURE IN SORT OF FILES IN $DCOM_IN/radar_level2_${stng}.history\
 in task $ntask - NO NEXRAD LEVEL 2 DATA PROCESSED --> non-fatal"
      postmsg "$jlogfile" "$msg"
      set +x
      echo
      echo $msg
      echo
      set -x
    fi

#  Determine if there are any new files to process, and update history file
#  ------------------------------------------------------------------------

   comm -13 $DCOM_IN/radar_level2_${stng}.history current_files_${stng} > new_files_${stng}
   comm -23 $DCOM_IN/radar_level2_${stng}.history current_files_${stng} > old_files_${stng}
   if [ -s old_files_${stng} ] ; then
      set +x
      echo
      echo
      echo "NCO has deleted the following files in $INDIR :"
      cat old_files_${stng}
      echo
      echo
      set -x
      comm -12 $DCOM_IN/radar_level2_${stng}.history current_files_${stng} > \
       remaining_files_${stng}
      mv remaining_files_${stng} $DCOM_IN/radar_level2_${stng}.history
      nfiles=`wc -l old_files_${stng} | awk '{ print $1 }'`
      msg="$nfiles old files removed from $INDIR since last time this job ran"
#      postmsg "$jlogfile" "$msg"
   fi

   if [ ! -s new_files_${stng} ] ; then
      msg=" No new files to process in $INDIR"
      set +x
      echo
      echo
      echo $msg
      echo
      echo
      set -x
#      postmsg "$jlogfile" "$msg"
      new_files_found=NO
   fi
else
    echo "Copying current files to new files"
    cp current_files_${stng} new_files_${stng}
fi


if [ $new_files_found = YES ]; then ##### new_files_found=YES

echo "inside new_files_found=YES"
set +x
echo
echo
echo "The following are new files found in $INDIR :"
cat new_files_${stng} >bf_tail_new_file_${stng}
echo
set -x
tail -5 new_files_${stng} > new_files_${stng}_temp
echo
echo "Processing is limited to, at max, the 5 most recent new files in $INDIR, these are :"
for filename1 in `cat new_files_${stng}_temp`; do
   echo data in processing from ${stng}:: $filename1
done
mv new_files_${stng}_temp new_files_${stng}
echo
echo
nfiles=`wc -l new_files_${stng} | awk '{ print $1 }'`
msg="$nfiles new files in $INDIR are available for processing"
#postmsg "$jlogfile" "$msg"


> radar_level2_${stng}.history_temp

icount_in=0
icount_good=0

for filename in `cat new_files_${stng}`; do ##### new_files

  echo $filename >> radar_level2_${stng}.history_temp
  echo $filename >> $DCOM_IN/radar_level2_${stng}.history

#
#
# Find the current date/hour/minute minus 1/2 hour date
# pick files in the selected time window for processing
#
# For 1 or 'n' hour window(s) set hourm2 as follows
#     hourm2=`/nwprod/util/exec/ndate -1``date +%M`
#
    hourmc=`$NDATE``date +%M`
#
# For 1/2 hour window setting only - Begin
#
    hourch=`date +%M`
    echo "current minute  " $hourch
    hourhh=`date -u +%H`
    echo "current hour  " $hourhh

    if [ $hourch -lt '30' ];then
       if [ $hourhh -eq '00' ];then
          hourmcc=`expr $hourmc - 40 - 7600`
       else
         hourmcc=`expr $hourmc - 40`
       fi
    else
       hourmcc=$hourmc
    fi
    echo "updated ndate  " $hourmcc
#   hourm2=`expr $hourmcc - 30`
#   echo "current ndate - 1/2 hour  " $hourm2
    hourm2=`$NDATE -2``date +%M`
    echo "current ndate - 2 hour  " $hourm2
#==================================
#   test wcoss2
#==================================
   mydate=202108270600
   hourmc=${mydate}
   mydate=202108270400
   hourm2=${mydate}
#==================================
#   end test
#==================================

#
# For 1/2 hour window setting only - End
#
    echo $filename > filename_temp_sel
#
# The current directory of the raw files is in:
# $DCOMROOT/prod/ldmdata/obs/upperair/nexrad_level2/KABR
# but if this directory changes the field in the cut command
# "-f9" should be changed appropriately
#
    fdate=`basename $filename | cut -c6-13`
    fhourmin=`basename $filename | cut -c15-18`
    fddhhmm=`echo $fdate``echo $fhourmin`
#
 if [ $fddhhmm -ge $hourm2 -a $fddhhmm -lt $hourmc ];then     ### Time window checking 
                                                              ### Pick files for processing 
                                                              ### in the chosen time window
                                                              
 msg="Level II FILE WITHIN ALLOWABLE TIME WINDOW, WILL PROCESS $filename file"
#   set +x
    echo
    echo $msg
    echo
    set -x
#    postmsg "$jlogfile" "$msg"
    echo "File with time: " $fddhhmm " will be processed between "$hourm2 " and " $hourmc
    icount_in=`expr $icount_in + 1`
    isize=`ls -l $filename  | awk '{ print $5 }'`
#   set +x
    echo "given_file_name = $filename"
    echo "filesize=$isize"
    echo $filename > filename_temp
    echo $filename >> processed_files_$ntask
    filename_short=`basename $filename`
#
# Uncompress the BZIP2 compressed level 2 data and perform the decoding,
# radial wind, reflectivity qc, generate the src files and tank the     
# qc-ed output into /dcom/us*/ bufr tanks
#
   iradar=`echo ${filename_short} | cut -c1-4`
   grid_nx=`grep $iradar mosaic.table | awk '{ print $3 }'`
   grid_ny=`grep $iradar mosaic.table | awk '{ print $4 }'`
   echo "radar, grid_nx & grid_ny  " $iradar $grid_nx $grid_ny
#
   export pgm=wsr88d_level2
   set +u
   . prep_step
   set -u
  export FORT52="$FIXbufr/bufrtab.002"
  export FORT62="$FIXbufr/bufrtab.007"
  export FORT71="${filename_short}_bufr"
  export FORT51="${filename_short}_vadbufr"
  export FORT61="${filename_short}_pblbufr"

   thisflnm=`echo $filename_short | cut -d "." -f1`
   #$EXECradar/wsr88d_level2 -f $FIXbufr -c $FIXradar -n $DICbufr -w ${filename_short} <  $filename >stdout_2_${thisflnm}
   $EXECradar/wsr88d_level2 -f $FIXbufr -c $FIXradar -n $DICbufr/nexrad.tbl  -w ${filename_short} <  $filename >stdout_2_${thisflnm}
   err=$?

## Added by Keyser to keep 1000 line rotating QC info files for each station
##  in $DCOMROOT/prod/nexrad_level2/QC_INFO directory
##  (most recent output at top of file)
##  modified by Shun to check if QC completed normally and put information to QC_INFO

   mv qcinfor qcinfor_$thisflnm
   err1=$?

   stn=`echo $filename_short | cut -d "_" -f1`
   head -n 4000 $DCOM_IN/QC_INFO/$stn > $DCOM_IN/QC_INFO/$stn.tmp
   echo "###################################################################\
####################################" > $DCOM_IN/QC_INFO/$stn
   echo $filename_short | cut -d "." -f1 >> $DCOM_IN/QC_INFO/$stn
# here $stn is the 4-letter stn id, this may already be present as $stng ???

   if [ $err1 -eq 0 ]; then
   cat ${filename_short}_bufr >> bufrout.task$ntask
   cat ${filename_short}_vadbufr >> vadbufrout.task$ntask
   cat ${filename_short}_pblbufr >> pblbufrout.task$ntask
   radarnm=`echo $filename_short | cut -c 1-4`
   mkdir -p $COMOUT_SRCtank/$radarnm
   mv *.src $COMOUT_SRCtank/$radarnm
   cat qcinfor_$thisflnm >> $DCOM_IN/QC_INFO/$stn

## End of script added by Keyser
   else

   echo "this radar data is not processed due to data problem " >> $DCOM_IN/QC_INFO/$stn

   fi

   cat $DCOM_IN/QC_INFO/$stn.tmp >> $DCOM_IN/QC_INFO/$stn
   rm $DCOM_IN/QC_INFO/$stn.tmp

## End of script added by Keyser


   if [ $err -eq 0 ]
   then
      set +x
      icount_good=`expr $icount_good + 1`
      set +x
      echo "$pgm completed NORMALLY for the radar # $icount_good with filename $filename"
      set -x
   else
      msg="**ERROR: $pgm FAILED  with r.c. $err for station in $filename in \
task $ntask - DATA IN THIS FILE NOT PROCESSED, MOVE ON TO NEXT FILE \
--> non-fatal"

      postmsg "$jlogfile" "$msg"
   fi
#
 else   ### Time window checking 
#
      msg="New file name $filename is outside the time window in task $ntask -\
 NO LEVEL 2 DATA PROCESSED"
      postmsg "$jlogfile" "$msg"

 fi    ### Time window checking

#
done ##### new_files
 
#      msg="After all new_files completed"
#      postmsg "$jlogfile" "$msg"
      msg="For radar station ${stng} in task $ntask: $icount_in new files read in; $icount_good new files successfully processed"
      nfiles=`wc -l radar_level2_${stng}.history_temp | awk '{ print $1 }'`
      msg="$nfiles new files added to $DCOM_IN/radar_level2_${stng}.history in\
 task $ntask"
##########      postmsg "$jlogfile" "$msg"

  
fi ##### new_files_found=YES

done ##### all stations in a single task

###
### Create the list of rejected/accepted vcps by the wind
### QC for all stations for all tasks
###
grep -i reject qcinfor_* > rejected_vcps_by_level2_wind_qc_$ntask
grep -i accept qcinfor_* > accepted_vcps_by_level2_wind_qc_$ntask       


msg="Level 2 radar data processing has completed NORMALLY in task $ntask !"
postmsg "$jlogfile" "$msg"


################## END OF SCRIPT #######################

exit 0

