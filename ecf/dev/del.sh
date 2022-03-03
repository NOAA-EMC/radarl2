

mydate=20220125
cyc=00
cdate=$mydate$cyc


logdir=/gpfs/dell2/ptmp/Shun.Liu/fv3lamdax/log
slogdir=/gpfs/dell2/emc/modeling/noscrub/Shun.Liu/test/lamlog/fv3lamdax_shun/$mydate/$cyc

cd $slogdir

files=`ls -1tr level2*`
for ifile in $files
do
  echo $ifile
   tdate=`grep COMPLETE $ifile | cut -c7-17`
   thisdate=${tdate:0:10}
   echo $thisdate
   if [ $thisdate =  $cdate ]; then
       echo $thisdate
   else
      rm -f $ifile
   fi
   exit
done
