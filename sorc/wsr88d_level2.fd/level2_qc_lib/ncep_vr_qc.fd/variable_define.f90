MODULE VARIABLE_DEFINE

!-----------------------------------------------------------------------
!  PURPOSE: Common reference point for constants and arrays
!
!  HISTORY: 04/29/2003 - Creation.                      Gong, Jiandong
!           03/15/2007 - Modify the comments.           Zhang, Pengfei     
!           08/14/2007 - introduce tendence method.     Nai, Kang
!-----------------------------------------------------------------------

   IMPLICIT NONE

!-----------------------------------------------------------------------
!  [1.0]  Parameter for radar array dimension
!-----------------------------------------------------------------------

!  Fundamental constants:
   REAL, PARAMETER    :: pi = 3.1415926535897932346
   REAL, PARAMETER    :: rdn = pi/180.0
   REAL, PARAMETER    :: invrdn = 1.0/rdn
   REAL, PARAMETER    :: spval = 999.0        ! No observation flag

!  Radar observation dimension parameters:
   INTEGER, PARAMETER :: nr = 1000 ! gates in a beam of vilocity
   INTEGER, PARAMETER :: np = 400 ! number of beams of a tilt
   INTEGER, PARAMETER :: nt =  30 ! elevation in vertical
   INTEGER, PARAMETER :: nr2= 1000 ! gate in one beam for reflectivity

!-----------------------------------------------------------------------
!  [2.0] parameter for radar observation
!-----------------------------------------------------------------------

!  Variable:
   INTEGER            :: ireftim  ! reference time
   INTEGER            :: itime    ! real time
   INTEGER            :: vcpnum   ! radar mode (4 kind: )

   INTEGER,DIMENSION(0:nt) :: iyr      ! Observation year
   INTEGER,DIMENSION(0:nt) :: imon     ! Observation month
   INTEGER,DIMENSION(0:nt) :: iday     ! Observation day
   INTEGER,DIMENSION(0:nt) :: ihr      ! Observation hour
   INTEGER,DIMENSION(0:nt) :: imin     ! Observation minute
   INTEGER,DIMENSION(0:nt) :: isec     ! Observation second

   INTEGER,DIMENSION(nt)   :: k_121    ! vcp=121 be chosen level
   REAL,DIMENSION(nt)      :: spc_thet ! vcp=121,re-design thet.
   REAL,DIMENSION(nt)      :: wrt_thet ! write out re-design thet.

   REAL               :: rfstgat    ! 1st gate distance of velocity 
   REAL               :: refgatsp   ! radial velocity gate spacing
   REAL               :: rfstgat2   ! 1st gate distance of reflectivity
   REAL               :: refgatsp2  ! reflectivity gate spacing

   CHARACTER*(4)      :: radid    ! Input radar name

   INTEGER            :: iminrng  ! index variable for first gate
   INTEGER            :: iminrng2 ! index variable for first gate

   INTEGER            :: nrang    ! index variable for last gate
   INTEGER            :: nrang2   ! index variable for last gate
   INTEGER            :: nbeam    ! index variable for azimuth 
   INTEGER            :: nthet    ! index variable for elevation

   REAL               :: radlat   ! radar position for latitude
   REAL               :: radlon   ! radar position for longtitude
   REAL               :: radelv   ! radar position for vertical height

!  Parameter for map projector
   INTEGER            :: mapproj  ! map projection
                                  ! 0:no map projection
                                  ! 1:polar projection
                                  ! 2:Lambert projection
                                  ! 3:Mercator projection
   REAL               :: trulat1  ! trucation of latitude
   REAL               :: trulat2  ! trucation of latitude
   REAL               :: trulon   ! trucation of longtitude
   REAL               :: sclfct   ! 1~meter
   REAL,DIMENSION(2)  :: latnot

   REAL               :: azmth    ! azimuth
   REAL               :: elvng    ! elevate angle
   REAL               :: hgtrad   ! radar beam heigh
   REAL               :: sfcrng   ! radar beam surface project
   REAL               :: range    ! distance along beam
   REAL               :: dhdr     ! dh/dr
   REAL               :: latgate  ! latitude
   REAL               :: longate  ! longatitude
   REAL               :: xr0      ! x position
   REAL               :: yr0      ! y position

!-----------------------------------------------------------------------
!  [3.0] arrays :
!-----------------------------------------------------------------------

!  Arrays for raw radar data:

   REAL,DIMENSION(nr)    :: ran    ! range distance for velocity
   REAL,DIMENSION(nr2)   :: ran2   ! range distance for reflect
   REAL,DIMENSION(np,nt) :: phi    ! azimuth angle (Degree) of velocity
   REAL,DIMENSION(np,nt) :: phi2   ! azimuth angle (Degree) of reflect
   REAL,DIMENSION(nt)    :: thet   ! elevation (Degree) of velocity
   REAL,DIMENSION(nt)    :: thet2  ! elevation (Degree) of reflect
   REAL,DIMENSION(nt)    :: thet_nyq_vel   ! Nyquist velocity(thet)
 
   INTEGER,DIMENSION(nt) :: nphi     ! number of beams of velocity
   INTEGER,DIMENSION(nt) :: nphi2    ! number of beams of reflect
   INTEGER,DIMENSION(nt) :: imaxrng  ! max number of range of velocity
   INTEGER,DIMENSION(nt) :: imaxrng2 ! max number of range of reflect

   REAL, POINTER      :: vel (:,:,:)    ! velocity
   REAL, POINTER      :: ref (:,:,:)    ! reflectivity
   REAL, POINTER      :: swg (:,:,:)    ! spectral width
   REAL, POINTER      :: vref (:,:,:)   ! reference radial velocity
   REAL, POINTER      :: rho (:,:,:)    ! coefficient
   REAL, POINTER      :: kdp (:,:,:)    ! phi differ
   REAL, POINTER      :: zdr (:,:,:)    ! Z differ
   REAL, POINTER      :: no_mark(:,:,:) ! no atmospheric sign mark
   REAL, POINTER      :: rf_mark(:,:,:) ! no atmospheric sign mark
   REAL, POINTER      :: vr_mark(:,:,:) ! no atmospheric sign mark
   REAL, POINTER      :: velref (:,:,:) ! reference radial velocity

   REAL, POINTER      :: orgvel (:,:,:) ! original radial velocity
   REAL, POINTER      :: wrkhgt(:,:)    ! radial point's height for working array
   REAL, POINTER      :: wrksfc(:,:)    ! radial point's hozizonal distance.
   REAL               :: vnyq           ! Nyquist velocity
   REAL               :: vnyq_min       ! minimum Nyquist velocity
   REAL               :: vnyq_max       ! maximum Nyquist velocity

!------------------------------------------------------------------------------
!  [4.0] parameters to control data stream
!------------------------------------------------------------------------------

!  Variables:
   INTEGER             :: ivadflag=0
   INTEGER             :: ipoint=0
   INTEGER             :: nprfl
   INTEGER             :: s_number     !=40 continue start gate number(knai)

!------------------------------------------------------------------------------
!  [5.0] parameters for dealiasing
!------------------------------------------------------------------------------

!  Working arrays:
   REAL, POINTER      :: obsvel(:,:)  ! radial velocity obs.
   REAL, POINTER      :: vadvel(:,:)  ! radial velocity from VAD
   REAL, POINTER      :: unfvel(:,:)  ! unfolding radial velocity 
   REAL, POINTER      :: wrkvel(:,:)  ! radial velocity for working array
   INTEGER, POINTER   :: index (:,:)  ! index for gate dealiasing by VAD 
   REAL, POINTER      :: chkvel(:,:)  ! noise free velocity
   REAL, POINTER      :: refvel(:,:)  ! reference radial velocity

   REAL, POINTER      :: cosx (:,:)   
   REAL, POINTER      :: cosy (:,:)   
   REAL, POINTER      :: cosz (:,:)   

!------------------------------------------------------------------------------
!  [6.0] parameter for wind profile
!------------------------------------------------------------------------------

!  Working arrays for vertical  direction:
   INTEGER, PARAMETER :: zlevel = 200 ! 200*50=10000 m
   REAL               :: zstep  = 50  ! m 
   INTEGER            :: ilevel       ! z level index
   INTEGER            :: k_tilt       ! k tilt 

   REAL,DIMENSION(zlevel) :: ustor    ! u wind keep array
   REAL,DIMENSION(zlevel) :: vstor    ! v wind keep array
   INTEGER,DIMENSION(zlevel) :: vad_intp   ! index of VAD
   REAL,DIMENSION(nt,zlevel) :: wstor ! w wind keep array
   REAL,DIMENSION(zlevel) :: hstor    ! heigh  keep array
   REAL,DIMENSION(zlevel) :: cf1stor  ! cf1 keep array
   REAL,DIMENSION(zlevel) :: cf2stor  ! cf2 keep array
   REAL,DIMENSION(zlevel) :: cf3stor  ! cf3 keep array

   INTEGER,DIMENSION(nt,zlevel) :: select_circ
   REAL,DIMENSION(nt,zlevel)    :: miss_gap
   REAL,DIMENSION(nt,zlevel)    :: vabs   ! scale wind keep array
   REAL,DIMENSION(nt,zlevel)    :: U0ref  ! scale wind keep array
   REAL,DIMENSION(nt,zlevel)    :: V0ref  ! scale wind keep array
   INTEGER,DIMENSION(nt,zlevel) :: Acnjgt  ! loan index
   INTEGER,DIMENSION(nt,zlevel) :: Bcnjgt  ! loan index
   REAL,DIMENSION(nt,zlevel)    :: c_fnct  ! cost-function
   REAL,DIMENSION(nt,zlevel)    :: betaref  ! beta angle
   LOGICAL :: no_qualify_job
   LOGICAL :: need_dealiase_job

   REAL,DIMENSION(zlevel) :: check_small_phi
   REAL,DIMENSION(zlevel) :: check_large_phi
   REAL,DIMENSION(zlevel) :: check_crit

   INTEGER            :: start_tilt
   INTEGER            :: end_tilt
   INTEGER            :: step_tilt

   REAL,DIMENSION(6) :: cost_f

!------------------------------------------------------------------------------
!  [7.0] count parameters for wind profile
!------------------------------------------------------------------------------

   INTEGER :: count_no_select_circle
   INTEGER,DIMENSION(nt,zlevel) :: count_toomuch_0Vr

END MODULE VARIABLE_DEFINE
