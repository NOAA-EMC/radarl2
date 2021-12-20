!//////////////////////////////////////////////////////////////////////////
!//  MODULE:  REPRESENT QCed and ALIGNed REFLECTIVITY 		         //
!//  AUTHOR:  XIAOYONG XU                                                //
!//    DATE:  09/18/06                                                   //
!//////////////////////////////////////////////////////////////////////////

      module reflectivity
        
         implicit none
         integer, parameter :: nazim = 360, ngate = 460,  nazim1=760,ngate1=1000


         type ccout_strct
           character(len=8) :: radar_name
           integer ::  vcpnum
           integer ::  year
           integer ::  month
           integer ::  day
           integer ::  hour
           integer ::  minute
           integer ::  second
           real ::  radlat
           real ::  radlon
           real ::  radhgt
           real ::  elev_angle
           real ::  fstgatdis
           real ::  nyq_vel
           integer ::   num_beam
           integer ::   num_gate
           real ::  gateWidth(nazim1)
           real ::  elev(nazim1)
           real ::  azim(nazim1)
           real ::  field(ngate1,nazim1)
         end type ccout_strct

         type ccout_QCed_ALIGNed_ref
            character(len=5) :: radar_name
            integer          :: vcpnum
            integer          :: year
            integer          :: month
            integer          :: day
            integer          :: hour
            integer          :: minute
            integer          :: second
            real             :: radlat
            real             :: radlon
            real             :: radhgt
            real             :: elev_angle
            real             :: fstgatdis
            integer          :: num_beam
            integer          :: num_gate
            real             :: gatewidth
            real             :: azim(nazim)
            real             :: field(ngate,nazim)
         end type ccout_QCed_ALIGNed_ref

          
         type ccout_strct_src
            character(len=5) :: radar_name
            integer          :: vcpnum
            integer          :: year
            integer          :: month
            integer          :: day
            integer          :: hour
            integer          :: minute
            integer          :: second
            real             :: radlat
            real             :: radlon
            real             :: radhgt
            integer          :: num_beam
            integer          :: num_gate
            real             :: elev
            real             :: azim(nazim)
            real             :: fstgatdis
            real             :: azim_spc
            real             :: gate_spc
            real             :: scale_value
            real             :: missing_data
            real             :: field_data(ngate,nazim)
         end type ccout_strct_src

         type ccout_strct_src_header
            integer*4        :: nx 
            integer*4        :: ny
            integer*4        :: nz
            real *4          :: dx
            real *4          :: dy
            real *4          :: nw_lat
            real *4          :: nw_lon 
            real *4          :: zp(31)
            real *4          :: missing_value 
!missing_value for the 2nd status: no echo but covered by radar  
         end type ccout_strct_src_header

             
      end module reflectivity


       

