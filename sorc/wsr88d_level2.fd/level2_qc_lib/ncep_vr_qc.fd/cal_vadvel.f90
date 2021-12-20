
SUBROUTINE CAL_VADVEL

!     ==================================================================
!     PURPOSE:
!       calculate the wind filed of each tilt by wind profile.
!     ==================================================================

      use variable_define

      implicit none

      integer :: i,j,k
      real    :: ddrot,dsdr,ygate,uazmrad,ygcr,xgcr,vazmrad,v_wind   &
                ,u_wind,p,cf1,xgate,cf3,cf2,ustar,vstar,cf1star      &
                ,cf2star,cf3star

!     ------------------------------------------------------------------
!     Calculate horizontal wind in the tilt
!     ------------------------------------------------------------------
      k=k_tilt

      do i=iminrng,nrang

        range=ran(i)
        call beamhgt(elvng,range,hgtrad,sfcrng)

        if ( hgtrad > hstor(zlevel) ) cycle

        do ilevel=1,zlevel-1

          u_wind=spval
          v_wind=spval

          if ( hgtrad <  hstor(1) ) then
!           ------------------------------------------------------------
!           calculate land surface speed.
!           ------------------------------------------------------------
            if (abs(ustor(1))<spval .and. abs(vstor(1))<spval ) then
              ustar   =   ustor(1)*0.4/log( hstor(1) )
              vstar   =   vstor(1)*0.4/log( hstor(1) )
              cf1star = cf1stor(1)*0.4/log( hstor(1) )
              cf2star = cf2stor(1)*0.4/log( hstor(1) )
              cf3star = cf3stor(1)*0.4/log( hstor(1) )
              u_wind=  ustar/0.4*log( hgtrad )
              v_wind=  vstar/0.4*log( hgtrad )
              cf1   =cf1star/0.4*log( hgtrad )
              cf2   =cf2star/0.4*log( hgtrad )
              cf3   =cf3star/0.4*log( hgtrad )
              do j=1,nbeam
                if ( abs(obsvel(i,j))<spval ) then
                  azmth=phi(j,k)
                  call beamhgt(elvng,range,hgtrad,sfcrng)
                  call gcircle(radlat,radlon,azmth,sfcrng,latgate,longate)
                  call lltoxy(1,1,latgate,longate,xgate,ygate)

                  call dhdrange(elvng,range,dhdr)
                  dsdr=sqrt(amax1(0.,(1.-dhdr*dhdr)))
                  call ddrotuv(1,longate,azmth,1.,ddrot,uazmrad,vazmrad)
                  xgcr = -uazmrad*dsdr
                  ygcr = -vazmrad*dsdr
                  vadvel(i,j) = u_wind*xgcr + v_wind*ygcr
                endif
              enddo
            endif
          endif

!         --------------------------------------------------------------
!         linear interpolate u and v ( or cf1,cf2,cf3)
!         --------------------------------------------------------------
          if ( hgtrad>=hstor(ilevel) .and. hgtrad<hstor(ilevel+1) ) then
            if ( abs(ustor(ilevel))<spval .and.                      &
                 abs(ustor(ilevel+1))<spval ) then
              p=( hgtrad - hstor(ilevel) )/zstep
              u_wind=(1-p)*  ustor(ilevel) + p*  ustor(ilevel+1)
              v_wind=(1-p)*  vstor(ilevel) + p*  vstor(ilevel+1)
              cf1   =(1-p)*cf1stor(ilevel) + p*cf1stor(ilevel+1)
              cf2   =(1-p)*cf2stor(ilevel) + p*cf2stor(ilevel+1)
              cf3   =(1-p)*cf3stor(ilevel) + p*cf3stor(ilevel+1)
              do j=1,nbeam
                if ( abs(obsvel(i,j))<spval ) then
                  azmth=phi(j,k)
                  call beamhgt(elvng,range,hgtrad,sfcrng)
                  call gcircle(radlat,radlon,azmth,sfcrng,latgate,longate)
                  call lltoxy(1,1,latgate,longate,xgate,ygate)

                  call dhdrange(elvng,range,dhdr)
                  dsdr=sqrt(amax1(0.,(1.-dhdr*dhdr)))
                  call ddrotuv(1,longate,azmth,1.,ddrot,uazmrad,vazmrad)
                  xgcr = -uazmrad*dsdr
                  ygcr = -vazmrad*dsdr
                  vadvel(i,j) = u_wind*xgcr + v_wind*ygcr
                endif
              enddo
            endif
          endif

        enddo       ! enddo ilevel
      enddo      ! enddo i

END SUBROUTINE CAL_VADVEL
