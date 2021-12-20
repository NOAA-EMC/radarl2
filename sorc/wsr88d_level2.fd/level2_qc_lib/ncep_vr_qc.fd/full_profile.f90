
SUBROUTINE FULL_PROFILE

      use variable_define

      implicit none

      integer :: i,j,ii,ip,ipst,ipen,ippp
      integer,dimension(:),allocatable :: kgoodbgn,kgoodend
      integer :: kgoodnum,ka,kb
      real    :: p
      real    :: ustar,vstar,cf1star,cf2star,cf3star

      allocate ( kgoodbgn(1:zlevel) )
      allocate ( kgoodend(1:zlevel) )

      kgoodbgn=0
      kgoodend=0
      kgoodnum=0
!     ==================================================================
!     find good gate for beginning and end.
!     ==================================================================
      ippp=0
      ip=1
1     continue
      ipst=0
      if ( ip<zlevel ) then
        do i=ip,zlevel
          if ( abs(ustor(i))<spval ) then
            ipst=i ; exit
          endif
        enddo

        if ( ipst>0 .AND. ipst<zlevel ) then
          ipen=zlevel
          do i=ipst+1,zlevel
            if ( abs(ustor(i))>900.0 ) then
              ipen=i-1 ; exit
            endif
          enddo

          if ( (ipen-ipst) >= 0 ) then
            ippp=ippp+1
            kgoodnum=ippp
            kgoodbgn(ippp)=ipst
            kgoodend(ippp)=ipen
          endif
        endif
        ip=i
        go to 1
      endif

      if ( kgoodnum==0 ) then
        print*,' '
        print*,'No Profile at all. Do nothing !!!!!!!!'
        print*,' '
        ustor=spval;vstor=spval
        cf1stor=spval;cf2stor=spval;cf3stor=spval
        RETURN
      endif

      ka=kgoodbgn(1)
      if ( ka>20 ) then
        print*,' '
        print*,'This Profile is not credible. The initial level is too high !!!!!!!!'
        print*,' '
        ustor=spval;vstor=spval
        cf1stor=spval;cf2stor=spval;cf3stor=spval
        RETURN
      endif

!     ==================================================================
!     full all profile
!     ==================================================================
      if ( kgoodnum==1 ) then
        ka=kgoodbgn(1)
        kb=kgoodend(1)
        if ( kb==ka ) then       ! only one level VAD
          if ( hstor(ka) > 1000.0 ) then
            print*,' '
            print*,'Only one high level VAD, can not full the profile !!!!'
            print*,' '
            ustor=spval;vstor=spval
            cf1stor=spval;cf2stor=spval;cf3stor=spval
            RETURN
          endif

!         --------------------------------------------------------------
!         downward extention the profile.
!         --------------------------------------------------------------
          ustar      =   ustor(ka)*0.4/log( hstor(ka) )
          vstar      =   vstor(ka)*0.4/log( hstor(ka) )
          cf1star    = cf1stor(ka)*0.4/log( hstor(ka) )
          cf2star    = cf2stor(ka)*0.4/log( hstor(ka) )
          cf3star    = cf3stor(ka)*0.4/log( hstor(ka) )

          do i=ka,1,-1
            if ( hstor(i)>200.0 ) then
              ustor(i)   =   ustor(ka)
              vstor(i)   =   vstor(ka)
              cf1stor(i) = cf1stor(ka)
              cf2stor(i) = cf2stor(ka)
              cf3stor(i) = cf3stor(ka)
            else
              ustor(i)   =   ustar/0.4*log( hstor(i) )
              vstor(i)   =   vstar/0.4*log( hstor(i) )
              cf1stor(i) = cf1star/0.4*log( hstor(i) )
              cf2stor(i) = cf2star/0.4*log( hstor(i) )
              cf3stor(i) = cf3star/0.4*log( hstor(i) )
            endif
          enddo
!         --------------------------------------------------------------
!         upward extention the profile.
!         --------------------------------------------------------------
          ipen=ka+4
          if ( ipen>=20 ) RETURN
          do i=ka,ipen
             ustor(i)   =   ustor(ka)
             vstor(i)   =   vstor(ka)
             cf1stor(i) = cf1stor(ka)
             cf2stor(i) = cf2stor(ka)
             cf3stor(i) = cf3stor(ka)
          enddo
          RETURN

        else

!         --------------------------------------------------------------
!         downward extention the profile.
!         --------------------------------------------------------------
          if ( hstor(ka)>1000.0 ) then
            do i=ka-1,1,-1
              ustor(i)   = spval
              vstor(i)   = spval
              cf1stor(i) = spval
              cf2stor(i) = spval
              cf3stor(i) = spval
            enddo
          else
            ustar      =   ustor(ka)*0.4/log( hstor(ka) )
            vstar      =   vstor(ka)*0.4/log( hstor(ka) )
            cf1star    = cf1stor(ka)*0.4/log( hstor(ka) )
            cf2star    = cf2stor(ka)*0.4/log( hstor(ka) )
            cf3star    = cf3stor(ka)*0.4/log( hstor(ka) )
            do i=ka,1,-1
              if ( hstor(i)>200.0 ) then
                ustor(i)   =   ustor(ka)
                vstor(i)   =   vstor(ka)
                cf1stor(i) = cf1stor(ka)
                cf2stor(i) = cf2stor(ka)
                cf3stor(i) = cf3stor(ka)
              else
                ustor(i)   =   ustar/0.4*log( hstor(i) )
                vstor(i)   =   vstar/0.4*log( hstor(i) )
                cf1stor(i) = cf1star/0.4*log( hstor(i) )
                cf2stor(i) = cf2star/0.4*log( hstor(i) )
                cf3stor(i) = cf3star/0.4*log( hstor(i) )
              endif
            enddo
          endif

!         --------------------------------------------------------------
!         upward extention the profile.
!         --------------------------------------------------------------
          ipen=kb+4
          if ( ipen>=20 ) RETURN
          do i=kb,ipen
             ustor(i)   =   ustor(kb)
             vstor(i)   =   vstor(kb)
             cf1stor(i) = cf1stor(kb)
             cf2stor(i) = cf2stor(kb)
             cf3stor(i) = cf3stor(kb)
          enddo
          RETURN
        endif
      endif

      DO ip=1,kgoodnum-1
        ipst=kgoodend(ip  )+1
        ipen=kgoodbgn(ip+1)-1
        if ( (ipen-ipst+1)>=40 ) then    ! Kang Nai added
          do i=ipst,zlevel
            ustor(i)=spval
            vstor(i)=spval
            cf1stor(i)=spval
            cf2stor(i)=spval
            cf3stor(i)=spval
          enddo
          exit                           ! end added
        else
          do i=ipst,ipen
            p=real((i-ipst+1))/(ipen-ipst+2)
            ustor(i)=(1-p)*ustor(kgoodend(ip))+p*ustor(kgoodbgn(ip+1))
            vstor(i)=(1-p)*vstor(kgoodend(ip))+p*vstor(kgoodbgn(ip+1))
            cf1stor(i)=(1-p)*cf1stor(kgoodend(ip))+p*cf1stor(kgoodbgn(ip+1))
            cf2stor(i)=(1-p)*cf2stor(kgoodend(ip))+p*cf2stor(kgoodbgn(ip+1))
            cf3stor(i)=(1-p)*cf3stor(kgoodend(ip))+p*cf3stor(kgoodbgn(ip+1))
          enddo
        endif
      ENDDO

!     ------------------------------------------------------------------
!     downward extention the profile.
!     ------------------------------------------------------------------
      ipst=1
      ipen=kgoodbgn(1)

      if ( hstor(ipen) > 1000.0 ) then
        do i=ipen,ipst,-1
          ustor(i)   = spval
          vstor(i)   = spval
          cf1stor(i) = spval
          cf2stor(i) = spval
          cf3stor(i) = spval
        enddo
      else
        ustar      =   ustor(ipen)*0.4/log( hstor(ipen) )
        vstar      =   vstor(ipen)*0.4/log( hstor(ipen) )
        cf1star    = cf1stor(ipen)*0.4/log( hstor(ipen) )
        cf2star    = cf2stor(ipen)*0.4/log( hstor(ipen) )
        cf3star    = cf3stor(ipen)*0.4/log( hstor(ipen) )
      
        do i=ipen,ipst,-1
          if ( hstor(i)>200.0 ) then
            ustor(i)   =   ustor(ipen)
            vstor(i)   =   vstor(ipen)
            cf1stor(i) = cf1stor(ipen)
            cf2stor(i) = cf2stor(ipen)
            cf3stor(i) = cf3stor(ipen)

          else
            ustor(i)   =   ustar/0.4*log( hstor(i) )
            vstor(i)   =   vstar/0.4*log( hstor(i) )
            cf1stor(i) = cf1star/0.4*log( hstor(i) )
            cf2stor(i) = cf2star/0.4*log( hstor(i) )
            cf3stor(i) = cf3star/0.4*log( hstor(i) )
          endif
        enddo
      endif

!     ------------------------------------------------------------------
!     upward extention.
!     ------------------------------------------------------------------
      ipst=kgoodend(1)
      kb=zlevel
      do i=ipst,zlevel
        if ( abs(ustor(i))>900.0 ) then
             kb=i-1
             exit
        endif
      enddo
      ipen=kb+4
      if ( ipen>=20 ) RETURN

      do i=kb,ipen
        ustor(i)   =   ustor(kb)
        vstor(i)   =   vstor(kb)
        cf1stor(i) = cf1stor(kb)
        cf2stor(i) = cf2stor(kb)
        cf3stor(i) = cf3stor(kb)
      enddo

      deallocate ( kgoodbgn )
      deallocate ( kgoodend )

END SUBROUTINE FULL_PROFILE
