!// ##########################################################################
!//
!//      maplib3d.cc: Implement class function of maplib3d
!//
!// ##########################################################################
!//
!//      Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
!//      May 20, 2000
!//
!//      Modification History:
!//
!// ##########################################################################




      module MAPLIB3D

       implicit none
       real, parameter :: map_scale_fct = 1.0

!//     ##################################################################
!//     ######                                                      ######
!//     ######                CLASS SETMAPR                         ######
!//     ######                                                      ######
!//
!//    PURPOSE:
!
!//     Set constants for map projections.
!
!//     INPUT:
!//
!//       iproj        Map projection number
!//                    1=North Polar Stereographic   (-1 South Pole)
!//                    2=Northern Lambert Conformal  (-2 Southern)
!//                    3=Mercator
!//                    4=Lat,Lon
!//
!//       scale        Map scale factor,  at latitude=latnot
!//                    Distance on map = (Distance on earth) * scale
!//                    For ARPS model runs, generally this is 1.0
!//                    For ARPS plotting this will depend on window
!//                    size and the area to be plotted.
!//
!//       latnot(2)    Real "True" latitude(s) of map projection
!//                    (degrees, positive north)
!//                    Except for iproj=1, only latnot(1) is used
!//
!//       orient       Longitude line that runs vertically on the map.
!//                    (degrees, negative west, positive east)
!//
!//#######################################################################

       type setmapr

        integer         :: jproj,jpole
        real            :: trulat1,trulat2,rota,scmap,xorig,yorig
        real            :: projc1,projc2,projc3,projc4,projc

       end type setmapr

      end module MAPLIB3D


      SUBROUTINE setmap_proj(mp, sm)

       use CONFIG_PARS
       use MAPLIB3D
       use PHYCST
       implicit none

       type (CCOUT_CONFIG_PARS)   mp
       type (setmapr)             sm 

       ! local variables: 
       real       denom1,denom2,denom3
       integer    iproj
       real       scale2           ! map scale factor
       real       latnot1          ! true latitude (degrees N)
       real       latnot2          ! true latitude (degrees N)
       real       orient           ! orientation longitude (degrees E)

       iproj   = mp.mapproj
       latnot1 = mp.trulat1
       latnot2 = mp.trulat2
     
       print*,mp.mapproj, mp.trulat1, mp.trulat2

       if(map_scale_fct.ne. 1.0) then
         scale2  = 1.0/(map_scale_fct)
       else 
         scale2  = 1.0
       endif

       orient=mp.trulon

       sm.xorig=0.
       sm.yorig=0.
       sm.jproj=abs(iproj)

       if(iproj.lt.0) then
        sm.jpole = -1
       else 
        sm.jpole = 1
       endif

       print*,iproj,sm.jproj,scale2

       IF( sm.jproj.eq.0 ) THEN      

!!!   No map projection

       ELSE IF( sm.jproj.eq.1 ) THEN

!!!  Polar Stereographic projection
!!!  For this projection:
!!!    projc1 is the scaled earth's radius, scale times eradius
!!!    projc2 is the numerator of emfact, the map image scale factor.
!!!    projc3 is projc2 times the scaled earth's radius.

        sm.trulat1 = latnot1
        sm.trulat2 = latnot2
        sm.rota    = orient
        sm.scmap   = scale2
        sm.projc1  = scale2*eradius
        sm.projc2  = 1. + sin(d2rad*sm.jpole*sm.trulat1)
        sm.projc3  = sm.projc1*sm.projc2;

        if(sm.jpole .gt. 0 .and. mp.debug_flag.eq.1 ) then
         print*," Map projection set to Polar Stereographic "
         print*," X origin, Y origin set to 0.,0. at the North Pole "
        else   
         print*," Map projection set to Polar Stereographic "
         print*," X origin, Y origin set to 0.,0. at the South Pole "
        endif

       ELSE IF( sm.jproj.eq.2 ) THEN

!!!  Lambert Conformal Conic Projection.
!!!  For this projection:
!!!    projc1 is the scaled earth's radius, scale times eradius/n
!!!    projc2 is cos of trulat(1)
!!!    projc3 is tan (45. - trulat/2) a const for local map scale
!!!    projc4 is the cone constant, n

        sm.trulat1 = latnot1
        sm.trulat2 = latnot2
        sm.rota    = orient
        sm.scmap   = scale2
        sm.projc2  = cos(d2rad*sm.trulat1)
        sm.projc3  = tan(d2rad*(45.-0.5*sm.jpole*sm.trulat1))
        denom1     = cos(d2rad*sm.trulat2)
        denom2     = tan(d2rad*(45.-0.5*sm.jpole*sm.trulat2))
        if(denom2 .ne. 0.) then
          denom3=log( sm.projc3/denom2 )
        else   
          denom3=0.
        endif

        if(denom1 .ne.0. .and. denom3 .ne. 0.) then
      
         sm.projc4=log( sm.projc2/denom1 ) / denom3

          if(sm.projc4 .lt. 0.) then
       
           print*, "  Warning in SETMAPR for Lambert Projection"
           print*, "  For the true latitudes provided, "
           print*,  sm.trulat1, " and ", sm.trulat2
           print*, " projection must be from opposite pole"
           print*, " changing pole."
          
           sm.jpole  =  -sm.jpole
           sm.projc3 =  tan(d2rad*(45.-0.5*sm.jpole*sm.trulat1))
           denom2    =  tan(d2rad*(45.-0.5*sm.jpole*sm.trulat2))

           if(denom2 .ne. 0.) then
              denom3=log( sm.projc3/denom2)
           else 
              denom3=0.
           endif

           if(denom1.ne.0..and.denom3.ne.0.) then 
            sm.projc4=log( sm.projc2/denom1 ) / denom3
           else 
            print*,"  Error (1) in SETMAPR for Lambert Projection"
            print*,"  Illegal combination of trulats one: "
            print*,sm.trulat1, " and two: ", sm.trulat2
            return
           endif
          
          endif    ! end if projc4<0.

         sm.projc1=scale2*eradius/sm.projc4

        !!! // end if denom1!=0.&&denom3!=0.

        else if(denom3.eq.0. .and. denom2.ne.0.) then
      
         sm.projc4=sin(d2rad*sm.jpole*sm.trulat1)

         if(sm.projc4 .lt. 0.) then
        
          print*,"  Warning in SETMAPR for Lambert Projection"
          print*,"  For the true latitudes provided, "
          print*,sm.trulat1," and ",sm.trulat2
          print*,"projection must be from opposite pole"
          print*,"  changing pole."
          
          sm.jpole  = -sm.jpole
          sm.projc4 = sin(d2rad*sm.jpole*sm.trulat1)
         
         endif

         sm.projc1=scale2*eradius/sm.projc4
      
        !!!  end if denom3.eq.0. .and. denom2.ne.0.

        else 

         print*,"  Error (1) in SETMAPR for Lambert Projection"
         print*,"  Illegal combination of trulats one: "
         print*,sm.trulat1," and two: ",sm.trulat2
         return 
      
        endif
 

       ELSE IF( sm.jproj .eq. 3 ) THEN
  
!!!  Mercator Projection.
!!!  For this projection:
!!!    projc1 is the scaled earth's radius, scale times eradius
!!!    projc2 is cos of trulat(1)
!!!    projc3 is projc1 times projc2

        sm.trulat1  = latnot1
        sm.trulat2  = latnot2
        sm.rota     = orient
        sm.scmap    = scale2
        sm.projc1   = scale2*eradius
        sm.projc2   = cos(d2rad*sm.trulat1)
        sm.projc3   = sm.projc1*sm.projc2

        if(sm.projc2.le.0.) then 
         print*,"  Error (1) in SETMAPR for Mercator Projection"
         print*,"  Illegal true latitude provided: ",sm.trulat1
         return
        endif
      
    
       ELSE IF( sm.jproj .eq. 4 ) THEN

!!!  Lat, Lon Projection.
!!!  For this projection:
!!!    projc1 is the scaled earth's radius, scale times eradius
!!!    projc2 is cos of trulat(1)
!!!    projc3 is projc1 times projc2 times 180/pi

        sm.trulat1  = latnot1
        sm.trulat2  = latnot2
        sm.rota     = orient
        sm.scmap    = scale2
        sm.projc1   = scale2*eradius
        sm.projc2   = cos(d2rad*sm.trulat1)
     
        if(sm.projc2.le.0.) then 
         print*,"  Error (1) in SETMAPR for Lat,Lon Projection"
         print*,"  Illegal true latitude provided: ",sm.trulat1
         return
        endif
        
        sm.projc3=sm.projc1*sm.projc2/d2rad
    
       ELSE

        print*, iproj, " projection is not supported"
        return
       
       ENDIF

      END SUBROUTINE setmap_proj  




 
!//     ##################################################################
!//     ##################################################################
!//     ######                                                      ######
!//     ######                FUNCTION   GETMAPR                    ######
!//     ######                                                      ######
!//     ##################################################################
!//     ##################################################################
!//
!void getmapr(int &iproj,float &scale,float *latnot,float &orient,
!                   float &x0,float &y0, setmapr &mp)
!{
!//
!//#######################################################################
!//
!//     PURPOSE:
!//
!//     Get the constants for the current map projection, which are stored
!//     in the common block named /projcst/.
!//
!//#######################################################################
!//
!//     OUTPUT:
!//
!//       iproj        Map projection number
!//                    1=North Polar Stereographic   (-1 South Pole)
!//                    2=Northern Lambert Conformal  (-2 Southern)
!//                    3=Mercator
!//                    4=Lat,Lon
!//
!//       scale        Map scale factor,  at latitude=latnot
!//                    Distance on map = (Distance on earth) * scale
!//                    For ARPS model runs, generally this is 1.0
!//                    For ARPS plotting this will depend on window
!//                    size and the area to be plotted.
!//
!//       latnot(2)    Real "True" latitude(s) of map projection
!//                    (degrees, positive north)
!//                    Except for iproj=2, only latnot(1) is used 
!//
!//       orient       Longitude line that runs vertically on the map.
!//                    (degrees, negative west, positive east)
!//
!//       x0           x coordinate of origin
!//       y0           y coordinate of origin
!//
!//#######################################################################
! 
!    iproj=mp.jproj*mp.jpole;
!    scale=mp.scmap;
!    latnot[0]=mp.trulat1;
!    latnot[1]=mp.trulat2;
!    orient=mp.rota;
!    x0=mp.xorig;
!    y0=mp.yorig;
!};




      SUBROUTINE lltoxy_func(rrlat, rrlon, xloc, yloc,a)

!!#######################################################################
!!
!!  PURPOSE:
!!
!!  Find the x- and y-coordinates for given latitude and longitude
!!  on a specific map projection.  Before calling this function,
!!  a proper map projection needs to be setup (e.g., by calling
!!  "setmapr".
!!
!!#######################################################################
       use PHYCST 
       use MAPLIB3D
       implicit none
        
       real,           intent(in)    :: rrlat, rrlon
       real,           intent(out)   :: xloc,yloc
       type (setmapr), intent(in)    :: a

       real  radius,denom,dlon,ratio,rlat,rlon
     
       rlat = rrlat
       rlon = rrlon

       IF( a.jproj.eq.0 )THEN 

!!#######################################################################
!!
!!  No map projection
!!
!!#######################################################################
    
        ratio=d2rad*eradius
        xloc = ratio*rlon - a.xorig
        yloc = ratio*rlat - a.yorig

       ELSE IF( a.jproj.eq.1 ) THEN

!!#######################################################################
!!
!!  Polar Stereographic projection
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is the numerator of emfact, the map image scale factor.
!!    projc3 is projc2 times the scaled earth's radius.
!!
!!#######################################################################

        denom=(1. + sin(d2rad*a.jpole*rlat))
        if(denom.eq.0.) denom=1.0E-10
        radius=a.jpole*a.projc3*cos(d2rad*rlat)/denom
        dlon=a.jpole*d2rad*(rlon-a.rota)
        xloc= radius*sin(dlon) - a.xorig
        yloc=-radius*cos(dlon) - a.yorig

       ELSE IF(a.jproj.eq.2 ) THEN

!!#######################################################################
!!
!!  Lambert Conformal Conic Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius/n
!!    projc2 is cos of trulat(1)
!!    projc3 is tan (45. - trulat/2) a const for local map scale
!!    projc4 is the cone constant, n
!!
!!#######################################################################

        radius = a.projc1*a.projc2*real(                           & 
              (real(tan(d2rad*(45.0-0.5*a.jpole*rlat)))/a.projc3) &
              **real(a.projc4)        )   
        dlon=a.projc4*d2rad*(rlon-a.rota)
        xloc=         radius*sin(dlon) - a.xorig
        yloc=-a.jpole*radius*cos(dlon) - a.yorig
   

       ELSE IF(a.jproj.eq.3) THEN

!!#######################################################################
!!
!!  Mercator Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is cos of trulat(1)
!!    projc3 is projc1 times projc2
!!
!!#######################################################################

        dlon=rlon-a.rota
        if(dlon.lt.-180.) dlon=dlon+360.
        if(dlon.lt. 180.) dlon=dlon-360.
        xloc=a.projc3*d2rad*dlon - a.xorig
        denom=tan(d2rad*(45. - 0.5*rlat))
        if( denom.le.0. ) denom=1.0E-10
        yloc=-a.projc3*log(denom) - a.yorig

       ELSE IF(a.jproj.eq.4) THEN
!!#######################################################################
!!
!!  Lat, Lon Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is cos of trulat(1)
!!    projc3 is projc1 times projc2 times 180/pi
!!
!!#######################################################################

        xloc=rlon-a.xorig
        yloc=rlat-a.yorig
    
       ELSE
     
        print*,a.jproj," projection is not supported"

       ENDIF

!        print*, eradius, d2rad, r2deg, er_temp
        print*,'lltoxy',xloc,yloc,rlon,rlat,a.xorig,a.yorig
     
       RETURN
 
      END SUBROUTINE lltoxy_func



      SUBROUTINE xytoll_func(rlat,rlon,x,y,idem,jdem,m)

       use PHYCST
       use MAPLIB3D
       implicit none

       integer,       intent(in)    :: idem,jdem
       real,          intent(out)   :: rlat(idem,jdem),rlon(idem,jdem)
       real,          intent(in)    :: x(idem),y(jdem)
       type (setmapr),intent(in)    :: m

       real    xabs,yabs,yjp,radius,ratio,dlon
       integer  i,j
    
       IF ( m.jproj.eq.0 ) THEN

!!!    No map projection

        ratio=r2deg/eradius

        do i=1,idem,1
        do j=1,jdem,1
          rlat(i,j) = ratio*(y(j)+m.yorig)
          rlon(i,j) = ratio*(x(i)+m.xorig)
        enddo
        enddo

       ELSE IF ( m.jproj.eq.1 ) THEN
    
!!#######################################################################
!!
!!  Polar Stereographic projection
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is the numerator of emfact, the map image scale factor.
!!    projc3 is projc2 times the scaled earth's radius.
!!
!!#######################################################################

        do i=1,idem,1
        do j=1,jdem,1
          
          yabs=y(j)+m.yorig
          xabs=x(i)+m.xorig
          radius=sqrt( xabs*xabs + yabs*yabs )/m.projc3
          rlat(i,j) = m.jpole*(90. - 2.*r2deg*atan(radius))
          if(rlat(i,j).gt. 90.) rlat(i,j) =  90.0
          if(rlat(i,j).lt.-90.) rlat(i,j) = -90.0

          if((m.jpole*yabs).gt.0.) then
            dlon=180. + r2deg*atan(-xabs/yabs)
          else if((m.jpole*yabs).lt.0.) then
            dlon=r2deg*atan(-xabs/yabs)
          else if (xabs.gt.0.) then                    ! y=0.
            dlon=90.
          else 
            dlon=-90.
          endif

          rlon(i,j) = m.rota + m.jpole*dlon
          if(rlon(i,j).gt. 180.) rlon(i,j)=rlon(i,j)-360.
          if(rlon(i,j).lt.-180.) rlon(i,j)=rlon(i,j)+360.
          if(rlon(i,j).gt. 180.) rlon(i,j)= 180.0
          if(rlon(i,j).lt.-180.) rlon(i,j)=-180.0

        enddo
        enddo

       ELSE IF ( m.jproj.eq.2 ) THEN
    
!!#######################################################################
!!
!!  Lambert Conformal Conic Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius/n
!!    projc2 is cos of trulat(1)
!!    projc3 is tan (45. - trulat/2) a const for local map scale
!!    projc4 is the cone constant, n
!!
!!#######################################################################

        do i=1,idem,1
        do j=1,jdem,1

          yabs=y(j)+m.yorig
          xabs=x(i)+m.xorig
          radius=sqrt( xabs*xabs+ yabs*yabs )
          ratio=m.projc3*real(                        &                  
               real(radius/(m.projc1*m.projc2))**real(1./m.projc4) )
          rlat(i,j)=m.jpole*(90. -2.*r2deg*(atan(ratio)))
          if(rlat(i,j).gt. 90.)rlat(i,j)= 90.0
          if(rlat(i,j).lt.-90.)rlat(i,j)=-90.0
 
          yjp=m.jpole*yabs
          if(yjp.gt.0.) then
            dlon=180. + r2deg*atan(-xabs/yabs)/m.projc4
          else if(yjp.lt.0.)then 
            dlon=r2deg*atan(-xabs/yabs)/m.projc4
          else if (xabs.gt.0.) then                    ! y=0.
            dlon=90./m.projc4
          else
            dlon=-90./m.projc4
          endif
          
          rlon(i,j) = m.rota + m.jpole*dlon
          if(rlon(i,j).gt. 180.) rlon(i,j)=rlon(i,j)-360.
          if(rlon(i,j).lt.-180.) rlon(i,j)=rlon(i,j)+360.
          if(rlon(i,j).gt. 180.) rlon(i,j)= 180.0
          if(rlon(i,j).lt.-180.) rlon(i,j)=-180.0

        enddo
        enddo

       ELSE IF( m.jproj.eq.3 ) THEN
    
!!#######################################################################
!!
!!  Mercator Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is cos of trulat(1)
!!    projc3 is projc1 times projc2
!!
!!#######################################################################
        do i=1,idem,1
        do j=1,jdem,1
          
          yabs=y(j)+m.yorig
          xabs=x(i)+m.xorig
          rlat(i,j)=(90. - 2.*r2deg*atan(exp(-yabs/m.projc3)))
          if(rlat(i,j).gt. 90.)rlat(i,j) =  90.0
          if(rlat(i,j).lt.-90.)rlat(i,j) = -90.0
          dlon=r2deg*(xabs/m.projc3)
          rlon(i,j)=m.rota + dlon
          if(rlon(i,j).gt. 180.) rlon(i,j)=rlon(i,j)-360.
          if(rlon(i,j).lt.-180.) rlon(i,j)=rlon(i,j)+360.

        enddo
        enddo

       ELSE IF( m.jproj.eq.4 ) THEN

!!#######################################################################
!!
!!  Lat, Lon Projection.
!!  For this projection:
!!    projc1 is the scaled earth's radius, scale times eradius
!!    projc2 is cos of trulat(1)
!!    projc3 is projc1 times projc2 times 180/pi
!!
!!#######################################################################

        do i=1,idem,1
        do j=1,jdem,1
          rlon(i,j)=x(i)+m.xorig
          rlat(i,j)=y(j)+m.yorig
        enddo
        enddo
    
       ELSE

        print*,m.jproj," projection is not supported"

       ENDIF

       RETURN

      END SUBROUTINE xytoll_func
    


!// ##########################################################################
!//
!//   setorig.cc: A function to set up the origin on a specific map projection
!//
!// ##########################################################################
!//
!// ##########################################################################
!//
!//   Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
!//   May 20, 2000
!//
!//   Modification History:
!//   09/10/2001  Jian Zhang (CIMMS/NSSL)
!//   Code cleanup.  Documentation improvement.
!//
!// ##########################################################################

      SUBROUTINE set_orig(m,iopt,x0,y0)

       use PHYCST
       use MAPLIB3D
       implicit none

       type (setmapr), intent(inout) :: m
       integer,        intent(in)    :: iopt
       real,           intent(in)    :: x0,y0
  
       real, allocatable :: rlat(:,:),rlon(:,:),cx(:),cy(:)
 
!!!  iopt=1 input origin is given in x,y in absolute coordinates.

       IF( iopt.eq.1 ) THEN
            
        m.xorig=x0
        m.yorig=y0
       
        allocate(rlat(1,1),rlon(1,1),cx(1),cy(1)) 

        cx(1)=0.
        cy(1)=0.
 
        call  xytoll_func(rlat,rlon,cx,cy, 1, 1,m)

        print*,"Coordinate origin set to absolute x,y ="
        print*,m.xorig,"  ",m.yorig
        print*,"  Latitude, longitude="
        print*,rlat(1,1),"  ",rlon(1,1)

        deallocate(rlat,rlon,cx,cy)
 
!!!  iopt=2 input origin is given in lat,lon on earth

       ELSE IF( iopt.eq.2 ) THEN
    
        m.xorig=0.
        m.yorig=0.

        call  lltoxy_func(x0,y0,m.xorig,m.yorig,m)
        
        print*,"Coordinate origin set to absolute x,y ="
        print*,m.xorig,"  ",m.yorig
        print*,"  Latitude, longitude="
        print*,x0,"  ",y0
        
       ELSE    

        allocate(rlat(1,1),rlon(1,1),cx(1),cy(1))

        cx(1)=0.
        cy(1)=0.

        call  xytoll_func(rlat,rlon,cx,cy, 1, 1,m)

        print*,"Coordinate origin set to absolute x,y ="
        print*,m.xorig,"  ",m.yorig
        print*,"  Latitude, longitude="
        print*,rlat(1,1),"  ",rlon(1,1)

        deallocate(rlat,rlon,cx,cy)

       ENDIF 

       RETURN

      END SUBROUTINE set_orig
