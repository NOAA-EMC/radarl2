
!//////////////////////////////////////////////////////////////////////////
!//    FILE:  RADAR.h							//
!//   CLASS:  RADAR							//
!// PURPOSE:  REPRESENT RADAR SITE INFORMATION				//
!//  AUTHOR:  CHRIS CALVERT                                              //
!//    DATE:  4/19/00                                                    //
!//////////////////////////////////////////////////////////////////////////

      module RADAR2

         type CCOUT_RADAR
           real   :: height
           real   :: latitude
           real   :: latitude_deg
           real   :: latitude_min
           real   :: latitude_sec
           real   :: longitude
           real   :: longitude_deg
           real   :: longitude_min
           real   :: longitude_sec
         end type CCOUT_RADAR

      end module RADAR2


