#! /usr/bin/perl

#=======================================================================
#
#  PURPOSE:
#  This script runs the 3D Mosaic  executable for a set of times
#
#  USE:
#  To run this script successfully, use following command-line call
#  exradar_reflectivity_mosaic.pl.sms <config file>
#
#=======================================================================
#  AUTHOR:  Carrie Langston  (CIMMS/NSSL)
#  May 2005
#  modification:  Shun Liu  (EMC/NCEP)
#  Nov 2013
#
#=======================================================================

#-----------------------------------------------------------------------
#
#  Edit the variables below for your case
#
#-----------------------------------------------------------------------
#@centmin = (201311261200, 201311261215);
@centmin = (mymin);

#Try to make a log file directory
#mkdir "logs" or print "mkdir logs: $!\n";

#Set path to executable
$exe_path = "./";

#-----------------------------------------------------------------------
#
#  Run for each time
#
#-----------------------------------------------------------------------

    foreach $timin (@centmin)
    {

      $centyear  = substr $timin, 0,4;
      $centmonth = substr $timin, 4,2;
      $idy       = substr $timin, 6,2;
      $ihr       = substr $timin, 8,2;
      $imin      = substr $timin, 10,2;
    
      #Create a tmp satellite_qcmask.config to reflect archive time
      $org_config_file = $ARGV[0];
      $tile = $ARGV[1];

      open(C_FILE, $org_config_file);
      open(TMP_FILE, ">", "./src_mosaic_tmp.config");
      
      while( !(eof C_FILE) )
      {
        $line = <C_FILE>;
        $line =~ s/YYYYYY/$centyear/;
        $line =~ s/MMMMMM/$centmonth/;
        $line =~ s/DDDDDD/$idy/;
        $line =~ s/HHHHHH/$ihr/;
        $line =~ s/NNNNNN/$imin/g;
        
        print TMP_FILE $line;
      }

      close(TMP_FILE);


      #format time variables
      $yr_str = $centyear;
      $mth_str = $centmonth;

      if($idy<0) { $dy_str = "0$idy"; }
      else { $dy_str = $idy; }

      if($ihr<0) { $hr_str = "0$ihr"; }
      else { $hr_str = $ihr; }

      if($imin<0) { $min_str = "0$imin"; }
      else { $min_str = $imin; }

      #Build time strings
      $date = "$yr_str$mth_str$dy_str";
      $time = "$hr_str$min_str";

      #build log file name
      $logfile = "./src_mosaic.$date.$time.$tile.log";

      #Run programs
      $command = "./mosaic src_mosaic_tmp.config > $logfile";
      system($command);

      print "$date.$time Complete\n";

    }
