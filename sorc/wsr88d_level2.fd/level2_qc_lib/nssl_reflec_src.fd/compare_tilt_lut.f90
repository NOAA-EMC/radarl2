!// ##########################################################################
!//  Function compare_tilt_lut
!// ##########################################################################
!//  To compare the polar grid parameters in the current observed tilt
!//  and those in a mosaic look up table.  If there is inconsistency,
!//  printout error message and abort.
!// ##########################################################################
!//  Author:  Jian Zhang (CIMMS/NSSL)
!//  Nov.27, 2002
!// ##########################################################################


      SUBROUTINE compare_tilt_lut (pp,lut_hdr,t_hdr, iflag)

      
       use CONFIG_PARS
       implicit none

       type (CCOUT_CONFIG_PARS),  intent(in)    :: pp
       type (tilt_moslut_header), intent(in)    :: lut_hdr
       type (tilt_data_header),   intent(in)    :: t_hdr
       integer                  , intent(inout) :: iflag

       integer i
   

       if(pp.debug_flag.eq.1) then
      
         print*,"radarnam= ",  lut_hdr.radarname(1:4)
         print*,"latrad=   ",  lut_hdr.latrad
         print*,"lonrad=   ",  lut_hdr.lonrad
         print*,"hgtrad=   ",  lut_hdr.hgtrad
         print*,"ctrlat=   ",  lut_hdr.ctrlat
         print*,"ctrlon=   ",  lut_hdr.ctrlon
         print*,"nx=       ",  lut_hdr.nx
         print*,"ny=       ",  lut_hdr.ny
         print*,"nz=       ",  lut_hdr.nz
         print*,"dx=       ",  lut_hdr.dx
         print*,"dy=       ",  lut_hdr.dy
         print*,"zp=       ",  (lut_hdr.zp(i),i=1,lut_hdr.nz)
         print*,"mapproj=  ",  lut_hdr.mapproj
         print*,"trulat1=  ",  lut_hdr.trulat1
         print*,"trulat1=  ",  lut_hdr.trulat2
         print*,"trulon=   ",  lut_hdr.trulon
         print*,"elv_lut=  ",  lut_hdr.elv_angle
         print*,"ng_lut=   ",  lut_hdr.ngates
         print*,"nr_lut=   ",  lut_hdr.nrays
      
         print*,"tilt_hdr.elv_angle= ", t_hdr.elv_angle
         print*,"tilt_hdr.nrays=     ", t_hdr.nrays
         print*,"tilt_hdr.ngates=    ", t_hdr.ngates

       endif

!!######################################################################
!!
!!  Check if the mosaic lookup table is correct for the current
!!  Cartesian grid/domain.
!!
!!######################################################################
      
       if(lut_hdr.nx .ne. pp.nx .or. lut_hdr.ny .ne.pp.ny.or.        &
          lut_hdr.nz .ne. pp.nz .or.                                 &
          abs(lut_hdr.ctrlat-pp.ctrlat).gt. 0.1 .or.                 &
          abs(lut_hdr.ctrlon-pp.ctrlon).gt. 0.1 .or.                 &
          abs(lut_hdr.dx-pp.dx) .gt. 1.0e-5 .or.                     &
          abs(lut_hdr.dy-pp.dy) .gt. 1.0e-5    )      then
     
         print*,"++ERROR inconsistent horizontal cartesian grid"
         print*," with mosaic_lut."
         print*,"pp.ctrlat=   ", pp.ctrlat
         print*,"pp.ctrlon=   ", pp.ctrlon
         print*,"pp.nx=       ", pp.nx
         print*,"pp.ny=       ", pp.ny
         print*,"pp.nz=       ", pp.nz
         print*,"pp.dx=       ", pp.dx
         print*,"pp.dy=       ", pp.dy
         
         iflag = 0
         goto 999
        
       endif
      
       do i=1, lut_hdr.nz, 1
        if(abs(lut_hdr.zp(i)-pp.zp(i)).gt.1.0e-5) then
          print*,"++ERROR  inconsistent vertical cartesian grid "
          print*,"with mosaic lut."
          iflag = 0
          goto 999
        endif
       enddo

       if( lut_hdr.mapproj .ne. pp.mapproj) then 
         
         print*,"++ERROR  inconsistent map projection with mosaic lut"
         iflag = 0
         goto 999

       else if(  lut_hdr.mapproj.ne.4 .and.                          & 
               ( abs(lut_hdr.trulat1-pp.trulat1).gt.1.0e-5.or.       &
                 abs(lut_hdr.trulat2-pp.trulat2).gt.1.0e-5.or.       &
                 abs(lut_hdr.trulon-pp.trulon)  .gt.1.0e-5 )  ) then
         print*,"++ERROR  inconsistent map projection with mosaic lut"
         iflag = 0
         goto 999
       endif

!!######################################################################
!!
!!  Check if the mosaic lookup table is correct for the current
!!  polar grid/domain.
!!
!!######################################################################
      
    !   if( lut_hdr.radarname.ne.t_hdr.radarname  .or.           & 
    !       abs(lut_hdr.latrad-t_hdr.latrad) .gt. 1.0e-2 .or.    &
    !       abs(lut_hdr.lonrad-t_hdr.lonrad) .gt. 1.0e-2 .or.    &
    !       abs(lut_hdr.hgtrad-t_hdr.hgtrad) .gt. 1.0e-2 ) then
       
        if (abs(lut_hdr.latrad-t_hdr.latrad) .gt. 1.0e-1 .or.    &
            abs(lut_hdr.lonrad-t_hdr.lonrad) .gt. 1.0e-1 .or.    &
            abs(lut_hdr.hgtrad-t_hdr.hgtrad) .gt. 30.       ) then


        print*,"++ERROR  inconsistent radar with mosaic lut."
        print*,t_hdr.radarname, lut_hdr.radarname
        print*,t_hdr.latrad,lut_hdr.latrad
        print*,t_hdr.lonrad,lut_hdr.lonrad
        print*,t_hdr.hgtrad,lut_hdr.hgtrad
        iflag = 0
        goto 999
       endif
    
       if(lut_hdr.azm1.ne.t_hdr.azm1) then
        print*,"ERROR t/lut:  azm1_data= ",t_hdr.azm1,lut_hdr.azm1
        iflag = 0
        goto 999
       endif
     
       if(lut_hdr.azm_spc.ne.t_hdr.azm_spc) then
        print*,"ERROR t/lut:  azm_spc= ",t_hdr.azm_spc,lut_hdr.azm_spc
        iflag = 0
        goto 999
       endif 

       if(lut_hdr.gate1.ne.t_hdr.gate1) then
        print*,"ERROR t/lut:  gate1_data= ",t_hdr.gate1,lut_hdr.gate1
        iflag = 0
        goto 999
       endif
 
       if(lut_hdr.gate_spc.ne.t_hdr.gate_spc) then
        print*,"ERROR t/lut gate_spc= ",t_hdr.gate_spc,lut_hdr.gate_spc
        iflag = 0
        goto 999
       endif

       iflag = 1

       print*,"Consistent"   

999    RETURN

      END SUBROUTINE compare_tilt_lut  
