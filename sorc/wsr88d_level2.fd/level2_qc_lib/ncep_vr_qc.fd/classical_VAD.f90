!     ##################################################################
!     ####                                                          ####
!     ####                   subroutine classical_VAD               ####
!     ####                                                          ####
!     ##################################################################
      SUBROUTINE classical_VAD
!
!     ==================================================================
!     PURPOSE:
!        Using the radar data( after QC ) to do the classical VAD to 
!        get the wind profile.
!
!     Author   : Kang Nai
!     Date     : 01/14/2008
!     Action   : created

!     Author   : Kang Nai
!     Date     : 07/14/2009
!     Action   : dump vad wind
!
!     ==================================================================
      use variable_define
      use vadhead

      IMPLICIT NONE

      INTEGER    :: i,j,k,ip,jp,jstep,jref
      integer    :: ii,jj,kk
      real       :: a1,a2,a3,a4
      integer    :: irfstgat,irefgatsp
      real,dimension(1:np) :: tmpphi,tmpvel
      integer    :: dnpt
      real       :: cf1,cf2,cf3,cos_elv
      character(len=4)   :: yy,mm,dd,hh,minute,radnm 
      character(len=8)   :: radnm8
      integer, parameter :: MAX_MNEMONICS = VAD_MNEMONICS ! =15
      integer, parameter :: MAX_LEVELS=VAD_LEVELS ! = 400 maximum records for a station
      integer :: lunout, lundx
      integer :: obs_yyyy, obs_mm, obs_dd, obs_hh, obs_mn
      real(8), dimension(MAX_LEVELS):: height, uwind, vwind
      real(8) :: vadlat8,vadlon8,vadhgt8 
!     integer, dimension(MAX_LEVELS):: vad_qc_flag
      integer:: izvad
      integer :: num_records !number of data records for a single station

!
      ustor=spval
      vstor=spval
      vad_intp=1
      write(*,*)'enter classical VAD:'
      do ilevel=1, zlevel

!       ----------------------------------------------------------------
!       choice data.
!       ----------------------------------------------------------------
        a1=0.0
        a2=0.0
        a3=0.0
        do k_tilt=1,nthet
          k=k_tilt
          if ( select_circ(k,ilevel) > 0 ) then    ! if select_circ>0
            nbeam=nphi(k)
            nrang = imaxrng(k)
            elvng = thet(k)
            cos_elv=cos(elvng*rdn)

            ii=select_circ(k,ilevel)

            ip=0
            do j=1,nbeam
               if ( abs(vel(ii,j,k))<spval ) then
                    ip=ip+1
                    tmpvel(ip) = -vel(ii,j,k)
                    tmpphi(ip) = 90.0-phi(j,k)
               endif
            enddo
            if ( ip>1 ) then
              call vadlsf(nr,ip,spval,tmpphi,tmpvel,dnpt,cf1,cf2,cf3)
              if ( dnpt .gt. 5 ) then
                 a1=a1-cf2/cos_elv
                 a2=a2+cf3/cos_elv
                 a3=a3+1.0
              endif
            endif
          endif
        enddo

!       ----------------------------------------------------------------
!       do mean job.
!       ----------------------------------------------------------------
        if ( a3>1.0 ) then
          ustor(ilevel)=a1/a3
          vstor(ilevel)=a2/a3
          vad_intp(ilevel)=0
        endif

      enddo

!     ------------------------------------------------------------------
!     do profile qc and full profile.
!     ------------------------------------------------------------------
      call profile_continue_qc
      call profile_differ_qc
      call full_profile

!     ------------------------------------------------------------------
!     write out wind profile
!     ------------------------------------------------------------------
      radnm=vadstaid(1:4)
      yy=vadstaid(6:9)
      mm=vadstaid(10:11)
      dd=vadstaid(12:13)
      hh=vadstaid(14:15)
      minute=vadstaid(16:17)
      write(*,*)'output yy::',yy,mm,dd,hh,minute

      izvad=0
      vadlat8=vadlat
      vadlon8=vadlon
      vadhgt8=vadhgt
      open(31,file='radar_prfl.dat',form='formatted')
             write(31,*)radnm
             write(31,*)vadlat,vadlon,vadhgt
             write(31,*)yy,mm,dd,hh,minute
      do ip=1,zlevel
        if ( abs(ustor(ip))<spval .and.                             &
             abs(vstor(ip))<spval ) then
             a1 = ustor(ip)*ustor(ip)
             a2 = vstor(ip)*vstor(ip)
             a3 = a1+a2
             if ( a3 .gt. 0.0 ) then
                  a4 = sqrt(a3)
             else
                  a4 = 0.0
             endif
             a1 = hstor(ip)/1000.0
             a2 = 0.0
             izvad=izvad+1
             height(izvad)=a1
             vad_uwind(izvad)=ustor(ip)
             vad_vwind(izvad)=vstor(ip)
             vad_height(izvad)=a1
             vad_qcflag(izvad)=vad_intp(ip)
             write(31,'(3e15.5,i8,i6,i8)') a1,ustor(ip),vstor(ip) &
                                         ,vad_intp(ip),ip,izvad
        endif
      enddo
      nvad=izvad
!     write(31,*)'nvad=',nvad
      close(31)
!     lunout=51
!     lundx=52
!     read(yy,*)obs_yyyy
!     read(mm,*)obs_mm
!     read(dd,*)obs_dd
!     read(hh,*)obs_hh
!     read(minute,*)obs_mn
!     num_records=izvad
!     radnm8=radnm
!     if(izvad>0) then
!             write(*,*)'entering vad2bufr'
!     call vad2_bufr_out(lunout, lundx, radnm8, vadlat8, vadlon8, &
!                         vadhgt8, obs_yyyy, obs_mm, obs_dd, obs_hh, &
!                         obs_mn, height, uwind, vwind, qc_flag, num_records)
!             write(*,*)'out vad2bufr'
!     end if


      return

      END SUBROUTINE classical_VAD
