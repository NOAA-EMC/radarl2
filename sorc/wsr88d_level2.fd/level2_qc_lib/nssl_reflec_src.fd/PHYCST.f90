!/* ##########################################################################
! *
! * phycst.h:  Declaring physical constants. 
! *
! * Author: Jian Zhang (CIMMS/NSSL)
! * Sept. 15, 2001
! *
! * Modification History:
! *
! * ########################################################################*/


!#ifndef PHYCST_H
!#define PHYCST_H

!#include <cmath>


!//#######################################################################
!//
!//     Earth and other physical constants
!//
!//#######################################################################

      module PHYCST
   
       implicit none
       real, parameter :: eradius = 6.371e6
       real, parameter :: d2rad   = 0.01745329252
       real, parameter :: r2deg   = 180./3.141592654
       real, parameter :: er_temp =eradius*d2rad

       real, parameter :: pi   = 3.1415926
       real, parameter :: pi2r = 1.0/pi/pi
 
      end module PHYCST
  
 
