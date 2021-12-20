!//////////////////////////////////////////////////////////////////////////
!//    FILE:  BRIGHTBAND.cc						//
!// PURPOSE:  IMPLEMENT BRIGHTBAND CLASS FROM BRIGHTBAND.h FILE		//
!//  AUTHOR:  CHRIS CALVERT						//
!//    DATE:  4/19/00							//
!//////////////////////////////////////////////////////////////////////////
     
      module BRIGHTBAND

         type CCOUT_BRIGHTBAND
            real     :: volume_bb_top
            real     :: volume_bb_bottom
            integer  :: volume_bb_exist
            real     :: global_bb_top
            real     :: global_bb_bottom
            real     :: latest_bb_top
            real     :: latest_bb_bottom
            integer  :: time_series_file_exist
            integer  :: number_of_past_records
            integer  :: bb_mode
            real     :: volume_top_std_dev
            real     :: volume_max_dbz_ratio
            integer  :: volume_high_dbz_count
            integer  :: volume_good_points_count
            integer  :: radials_per_section
         end type CCOUT_BRIGHTBAND

       end module BRIGHTBAND
