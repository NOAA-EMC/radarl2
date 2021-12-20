c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######            ARPS Map Projection Subsystem.            ######
c     ######                                                      ######
c     ######              Copyright (c) 1993-1995                 ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c      General Information
c
c      This set of subroutines allows for transformation between
c      lat-lon coordinates and any one of three map projections: Polar
c      Stereographic, Lambert Conformal or Mercator.
c
c      In order for the transformation subroutines to work, the 
c      map projection must first be set up by calling setmapr.  The
c      user may wish to call setorig immediately after setmapr to 
c      established an origin (given a lat-long or x-y in the default
c      system) other than the default origin (e.g., the north pole).
c
c      All lat-lons are in degrees (positive north, negative south,
c      positive east and negative west).  Note carefully the dimensions
c      of x,y -- it differs among the subroutines to conform to ARPS usage.
c      x,y coordinates are meters on earth but may be changed using the scale
c      parameter in setmapr to change to km (scale=0.001) or to a different
c      sphere (e.g., scale=mars_radius/earth_radius).
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE SETMAPR                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE SETMAPR(iproj,scale,latnot,orient)
c
c#######################################################################
c
c     PURPOSE:
c
c     Set constants for map projections, which are stored in 
c     the common block named /projcst/.
c
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/13/93.   
c
c     MODIFICATION HISTORY:
c     03/30/1995 (K. Brewster)
c     Corrected error in Lambert Conformal scaling and added code to
c     allow Lambert Tangent projection (lat1=lat2 in Lambert Conformal).
c     Resulted in redefinition of projc1 for option 2.
c
c#######################################################################
c
c     INPUT:
c
c       iproj        Map projection number
c                    1=North Polar Stereographic   (-1 South Pole)
c                    2=Northern Lambert Conformal  (-2 Southern)
c                    3=Mercator
c                    4=Lat,Lon
c
c       scale        Map scale factor,  at latitude=latnot
c                    Distance on map = (Distance on earth) * scale
c                    For ARPS model runs, generally this is 1.0
c                    For ARPS plotting this will depend on window
c                    size and the area to be plotted.
c
c       latnot(2)    Real "True" latitude(s) of map projection
c                    (degrees, positive north)
c                    Except for iproj=1, only latnot(1) is used 
c
c       orient       Longitude line that runs vertically on the map.
c                    (degrees, negative west, positive east)
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      integer iproj
      real scale                       ! map scale factor
      real latnot(2)                   ! true latitude (degrees N)
      real orient                      ! orientation longitude (degrees E)
 
      real d2rad,eradius
      parameter (d2rad=3.141592654/180.,
     :           eradius = 6371000. )  ! mean earth radius in m
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      real denom1,denom2,denom3

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      xorig=0.
      yorig=0.
      jproj=iabs(iproj)
      jpole=isign(1,iproj)
      print *, ' jpole = ',jpole
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF ( jproj.eq.0 ) THEN
        write(6,'(a)')
     :  '  No map projection will be used.'
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSEIF( jproj.eq.1 ) THEN
        trulat(1)=latnot(1)
        rota=orient
        scmap=scale
        projc1=scale*eradius
        projc2=(1. + sin(d2rad*jpole*trulat(1)) )
        projc3=projc1*projc2
        IF(jpole.gt.0) THEN
          write(6,'(a/,a)')
     :    '  Map projection set to Polar Stereographic',
     :    '  X origin, Y origin set to 0.,0. at the North Pole.'
        ELSE
          write(6,'(a/,a)')
     :    '  Map projection set to Polar Stereographic',
     :    '  X origin, Y origin set to 0.,0. at the South Pole.'
        END IF
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        trulat(1)=latnot(1)
        trulat(2)=latnot(2)
        rota=orient
        scmap=scale
        projc2=cos(d2rad*trulat(1))
        projc3=tan(d2rad*(45.-0.5*jpole*trulat(1)))
        denom1=cos(d2rad*trulat(2))
        denom2=tan(d2rad*(45.-0.5*jpole*trulat(2)))
        IF(denom2.ne.0.) THEN
          denom3=alog( projc3/denom2 )
        ELSE
          denom3=0.
        END IF
        IF(denom1.ne.0. and. denom3.ne.0.) THEN
          projc4=alog( projc2/denom1 ) / denom3
          print *, '  The cone constant is : ',projc4
          IF( projc4.lt.0.) THEN
            write(6,'(a/,a,f9.2,a,f9.2,/a)')
     :    '  Warning in SETMAPR for Lambert Projection',
     :    '  For the true latitudes provided, ',
     :       trulat(1),' and ',trulat(2),
     :    '  projection must be from opposite pole...changing pole.'
            jpole=-jpole
            projc3=tan(d2rad*(45.-0.5*jpole*trulat(1)) )
            denom2=tan(d2rad*(45.-0.5*jpole*trulat(2)))
            IF(denom2.ne.0.) THEN
              denom3=alog( projc3/denom2 )
            ELSE
              denom3=0.
            END IF
            IF(denom1.ne.0. and. denom3.ne.0.) THEN
              projc4=alog( projc2/denom1 ) / denom3
              print *, '  The revised cone constant is : ',projc4
            ELSE 
              write(6,'(a/,a,f9.2,a,f9.2)')
     :      '  Error (1) in SETMAPR for Lambert Projection',
     :      '  Illegal combination of trulats one: ',
     :         trulat(1),' and two: ',trulat(2)
              STOP
            END IF
          END IF
          projc1=scale*eradius/projc4
        ELSE IF(denom3.eq.0. .and. denom2.ne.0.) THEN   ! tangent
          write(6,'(a/,a,f9.2,a,f9.2)')
     :    '  Using Tangent Lambert Projection',
     :    '  Based on input combination of trulats one: ',
     :       trulat(1),' and two: ',trulat(2)
          projc4=sin(d2rad*jpole*trulat(1))
          print *, '  The cone constant is : ',projc4
          IF( projc4.lt.0.) THEN
            write(6,'(a/,a,f9.2,a,f9.2,/a)')
     :    '  Warning in SETMAPR for Lambert Projection',
     :    '  For the true latitudes provided, ',
     :       trulat(1),' and ',trulat(2),
     :    '  projection must be from opposite pole...changing pole.'
            jpole=-jpole
            projc4=sin(d2rad*jpole*trulat(1))
          END IF
          projc1=scale*eradius/projc4
        ELSE
          write(6,'(a/,a,f9.2,a,f9.2)')
     :    '  Error (1) in SETMAPR for Lambert Projection',
     :    '  Illegal combination of trulats one: ',
     :       trulat(1),' and two: ',trulat(2)
          STOP
        END IF
 
        IF(jpole.gt.0) THEN
          write(6,'(a/,a)')
     :    '  Map projection set to Lambert Conformal',
     :    '  X origin, Y origin set to 0.,0. at the North Pole.'
        ELSE
          write(6,'(a/,a)')
     :    '  Map projection set to Lambert Conformal',
     :    '  X origin, Y origin set to 0.,0. at the South Pole.'
        END IF
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF( jproj.eq.3 ) THEN
        trulat(1)=latnot(1)
        rota=orient
        scmap=scale
        projc1=scale*eradius
        projc2=cos(d2rad*trulat(1))
        projc3=projc1*projc2
        IF(projc2.le.0.) THEN 
          write(6,'(a/,a,f9.2,a,f9.2)')
     :    '  Error (1) in SETMAPR for Mercator Projection',
     :    '  Illegal true latitude provided: ',trulat(1)
          STOP
        END IF
        write(6,'(a/,a,f6.1/,a)')
     :    '  Map projection set to Mercator',
     :    '  X origin, Y origin set to 0.,0. at the equator,',rota,
     :    '  Y positive toward the North Pole.'
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF( jproj.eq.4 ) THEN
        trulat(1)=latnot(1)
        rota=orient
        scmap=scale
        projc1=scale*eradius
        projc2=cos(d2rad*trulat(1))
        IF(projc2.le.0.) THEN 
          write(6,'(a/,a,f9.2,a,f9.2)')
     :    '  Error (1) in SETMAPR for Lat,Lon Projection',
     :    '  Illegal true latitude provided: ',trulat(1)
          STOP
        END IF
        projc3=projc1*projc2/d2rad
        write(6,'(a/,a,/a)')
     :    '  Map projection set to Lat, Lon',
     :    '  X origin, Y origin set to 0.,0. at the equator, 0. long',
     :    '  Y positive toward the North Pole.'
      ELSE
        write(6,'(i4,a)') iproj,' projection is not supported'
        STOP
      END IF
 
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE GETMAPR                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1994                    ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE GETMAPR(iproj,scale,latnot,orient,x0,y0)
c
c#######################################################################
c
c     PURPOSE:
c
c     Get the constants for the current map projection, which are stored
c     in the common block named /projcst/.
c
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     9/17/94.   
c
c     MODIFICATION HISTORY:
c     1/17/96  Corrected retrieval of iproj to assign sign from jpole.
c
c#######################################################################
c
c     OUTPUT:
c
c       iproj        Map projection number
c                    1=North Polar Stereographic   (-1 South Pole)
c                    2=Northern Lambert Conformal  (-2 Southern)
c                    3=Mercator
c                    4=Lat,Lon
c
c       scale        Map scale factor,  at latitude=latnot
c                    Distance on map = (Distance on earth) * scale
c                    For ARPS model runs, generally this is 1.0
c                    For ARPS plotting this will depend on window
c                    size and the area to be plotted.
c
c       latnot(2)    Real "True" latitude(s) of map projection
c                    (degrees, positive north)
c                    Except for iproj=2, only latnot(1) is used 
c
c       orient       Longitude line that runs vertically on the map.
c                    (degrees, negative west, positive east)
c
c       x0           x coordinate of origin
c       y0           y coordinate of origin
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      integer iproj       ! map projection number
      real scale          ! map scale factor
      real latnot(2)      ! true latitude (degrees N)
      real orient         ! orientation longitude (degrees E)
      real x0             ! x coordinate of origin
      real y0             ! y coordinate of origin
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      iproj=jproj*jpole
      scale=scmap
      latnot(1)=trulat(1)
      latnot(2)=trulat(2)
      orient=rota
      x0=xorig
      y0=yorig
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE SETORIG                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE SETORIG(iopt,x0,y0)
c
c#######################################################################
c
c     PURPOSE:
c
c     Set the origin for the map projection.
c     This is call after subroutine mapproj if the origin
c     must be moved from the original position, which is the
c     pole for the polar stereographic projection and the
c     Lambert conformal, and the equator for Mercator.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/20/93.   
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       iopt        origin setting option
c                   1: origin given in corrdinate x,y
c                   2: origin given in lat,lon on earth
c
c       x0          first coordinate of origin
c       y0          second coordinate of origin
c 
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      integer iopt       ! origin setting option
      real x0            ! first coordinate of origin
      real y0            ! second coordinate of origin
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      real xnew,ynew,rlat,rlon

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c
c#######################################################################
c
c     iopt=1 origin is given in x,y in absolute coordinates.
c
c#######################################################################
c
      IF( iopt.eq.1 ) THEN
        xorig=x0
        yorig=y0
        CALL xytoll(1,1,0.,0.,rlat,rlon)

        write(6,'(/a,f18.2,f18.2,/a,f16.2,f16.2/)')
     : '  Coordinate origin set to absolute x,y =',xorig,yorig,
     : '    Latitude, longitude= ',rlat,rlon 
c
c#######################################################################
c
c     iopt=2 origin is given in lat,lon on earth
c
c#######################################################################
c
c
      ELSE IF( iopt.eq.2 ) THEN
        xorig=0.
        yorig=0.
        CALL lltoxy(1,1,x0,y0,xnew,ynew)
        xorig=xnew
        yorig=ynew
        write(6,'(/a,f16.2,f16.2,/a,f16.2,f16.2/)')
     : '  Coordinate origin set to absolute x,y =',xorig,yorig,
     : '    Latitude, longitude= ',x0,y0
 
      ELSE
        CALL xytoll(1,1,0.,0.,rlat,rlon)
        write(6,'(/a,i4,a,/a,f16.2,f16.2,/a,f16.2,f16.2)')
     : ' Setorig option ',iopt,' not supported.',
     : '    Coordinate origin unchanged at x,y =',xorig,yorig,
     : '    Latitude, longitude= ',rlat,rlon
      END IF
      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE XYTOLL                     ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
      SUBROUTINE XYTOLL(idim,jdim,x,y,rlat,rlon)
c
c#######################################################################
c
c     PURPOSE:
c
c     Determine latitude and longitude given X,Y coordinates on 
c     map projection.  SETMAPR must be called before this routine
c     to set-up the map projection constants.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/13/93.   
c
c     MODIFICATION HISTORY:
c     01/17/96  Bug in southern hemisphere for Polar Stereo and
c               Mercator projections fixed.
c
c#######################################################################
c
c     INPUT:
c
c       idim     Number of points in x direction.
c       jdim     Number of points in y direction.
c
c       rlat     Array of latitude.
c                (degrees, negative south, positive north)
c
c       rlon     Array of longitude.
c                (degrees, negative west, positive east)
c
c     OUTPUT:
c
c       x        Vector of x in map coordinates 
c       y        Vector of y in map coordinates
c                Units are meters unless the scale parameter is 
c                not equal to 1.0
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      integer idim,jdim
      real x(idim),y(jdim),rlat(idim,jdim),rlon(idim,jdim)
 
      real r2deg,eradius
      parameter (r2deg=180./3.141592654,
     :           eradius = 6371000. )  ! mean earth radius in m
c
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i,j
      real xabs,yabs,yjp
      real radius,ratio,dlon

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF ( jproj.eq.0 ) THEN
        ratio=r2deg/eradius
        DO 10 j = 1, jdim
        DO 10 i = 1, idim
          rlat(i,j) = ratio*(y(j)+yorig)
          rlon(i,j) = ratio*(x(i)+xorig)
10      CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSEIF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          yabs=y(j)+yorig
          xabs=x(i)+xorig
          radius=sqrt( xabs*xabs + yabs*yabs )/projc3
          rlat(i,j) = jpole*(90. - 2.*r2deg*atan(radius))
          rlat(i,j)=amin1(rlat(i,j), 90.)
          rlat(i,j)=amax1(rlat(i,j),-90.)
 
          IF((jpole*yabs).gt.0.) THEN
            dlon=180. + r2deg*atan(-xabs/yabs)
          ELSE IF((jpole*yabs).lt.0.) THEN
            dlon=r2deg*atan(-xabs/yabs)
          ELSE IF (xabs.gt.0.) THEN     ! y=0.
            dlon=90.
          ELSE
            dlon=-90.
          END IF
          rlon(i,j)= rota + jpole*dlon
          IF(rlon(i,j).gt. 180) rlon(i,j)=rlon(i,j)-360.
          IF(rlon(i,j).lt.-180) rlon(i,j)=rlon(i,j)+360.
          rlon(i,j)=amin1(rlon(i,j), 180.)
          rlon(i,j)=amax1(rlon(i,j),-180.)
c
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF ( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          yabs=y(j)+yorig
          xabs=x(i)+xorig
          radius=sqrt( xabs*xabs+ yabs*yabs )
          ratio=projc3*((radius/(projc1*projc2))**(1./projc4))
          rlat(i,j)=jpole*(90. -2.*r2deg*(atan(ratio)))
          rlat(i,j)=amin1(rlat(i,j), 90.)
          rlat(i,j)=amax1(rlat(i,j),-90.)
 
          yjp=jpole*yabs
          IF(yjp.gt.0.) THEN
            dlon=180. + r2deg*atan(-xabs/yabs)/projc4
          ELSE IF(yjp.lt.0.) THEN
            dlon=r2deg*atan(-xabs/yabs)/projc4
          ELSE IF (xabs.gt.0.) THEN     ! y=0.
            dlon=90./projc4
          ELSE
            dlon=-90./projc4
          END IF
          rlon(i,j)= rota + jpole*dlon
          IF(rlon(i,j).gt. 180) rlon(i,j)=rlon(i,j)-360.
          IF(rlon(i,j).lt.-180) rlon(i,j)=rlon(i,j)+360.
          rlon(i,j)=amin1(rlon(i,j), 180.)
          rlon(i,j)=amax1(rlon(i,j),-180.)
 
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF( jproj.eq.3 ) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          yabs=y(j)+yorig
          xabs=x(i)+xorig
          rlat(i,j)=(90. - 2.*r2deg*atan(exp(-yabs/projc3)))
          rlat(i,j)=amin1(rlat(i,j), 90.)
          rlat(i,j)=amax1(rlat(i,j),-90.)
          dlon=r2deg*(xabs/projc3)
          rlon(i,j)=rota + dlon
          IF(rlon(i,j).gt. 180) rlon(i,j)=rlon(i,j)-360.
          IF(rlon(i,j).lt.-180) rlon(i,j)=rlon(i,j)+360.
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF( jproj.eq.4 ) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          rlon(i,j)=x(j)-xorig
          rlat(i,j)=y(j)-yorig
  400   CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
 
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE LLTOXY                     ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE LLTOXY(idim,jdim,rlat,rlon,xloc,yloc)
c
c#######################################################################
c
c     PURPOSE:
c
c     Determine x, y coordinates on map projection from the given latitude
c     and longitude. SETMAPR must be called before this routine to set-up
c     the map projection constants.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/11/93.   
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       idim     Array dimension in x direction
c       jdim     Array dimension in y direction
c
c       rlat     Real vector of latitude.
c                (degrees, negative south, positive north)
c
c       rlon     Real vector of longitude.
c                (degrees, negative west, positive east)
c
c     OUTPUT:
c
c       xloc     Real vector of x in map coordinates
c       yloc     Real vector of y in map coordinates
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim
      real rlat(idim,jdim),rlon(idim,jdim)
      real xloc(idim,jdim),yloc(idim,jdim)
 
      real d2rad,eradius
      parameter (d2rad=3.141592654/180.,
     :           eradius = 6371000. )  ! mean earth radius in m
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i,j
      real radius,denom,dlon,ratio

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        ratio=d2rad*eradius
        DO 10 j = 1, jdim
        DO 10 i = 1, idim
          xloc(i,j) = ratio*rlon(i,j) - xorig
          yloc(i,j) = ratio*rlat(i,j) - yorig
10      CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          denom=(1. + sin(d2rad*jpole*rlat(i,j)))
          IF(denom.eq.0.) denom=1.0E-10
          radius=jpole*projc3*cos(d2rad*rlat(i,j))/denom
          dlon=jpole*d2rad*(rlon(i,j)-rota)
          xloc(i,j)= radius*sin(dlon) - xorig
          yloc(i,j)=-radius*cos(dlon) - yorig
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          radius=projc1*projc2
     :          *(tan(d2rad*(45.-0.5*jpole*rlat(i,j)))/projc3)**projc4
          dlon=projc4*d2rad*(rlon(i,j)-rota)
          xloc(i,j)=       radius*sin(dlon) - xorig
          yloc(i,j)=-jpole*radius*cos(dlon) - yorig
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          dlon=rlon(i,j)-rota
          IF(dlon.lt.-180.) dlon=dlon+360.
          IF(dlon.gt. 180.) dlon=dlon-360.
          xloc(i,j)=projc3*d2rad*dlon - xorig
          denom=tan(d2rad*(45. - 0.5*rlat(i,j)))
          IF( denom.le.0. ) denom=1.0E-10
          yloc(i,j)=-projc3*alog(denom) - yorig
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          xloc(i,j)=rlon(i,j)-xorig
          yloc(i,j)=rlat(i,j)-yorig
  400   CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE LATTOMF                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE LATTOMF(idim,jdim,rlat,emfact)
c
c#######################################################################
c
c     PURPOSE:
c
c     Determine the map scale factor, emfact, at a given latitude.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/11/93.   
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       idim        Array dimension in x direction
c       jdim        Array dimension in y direction
c
c       rlat        Real vector of latitudes. 
c                   (degrees, negative south, positive north)
c
c     OUTPUT:
c
c       emfact      Vector of map scale factors corresponding to the
c                   input latitudes (map scale includes the projection
c                   image scale times the overall scale of the map).
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim         ! dimensions of arrays
      real rlat(idim,jdim)      ! latitude (degrees) 
      real emfact(idim,jdim)    ! local map scale factor
 
      real d2rad
      parameter (d2rad=3.141592654/180.)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i,j
      real denom

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        DO 10 j=1,jdim
        DO 10 i=1,idim
          emfact(i,j)=1.0
 10     CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          denom=(1. + sin(d2rad*jpole*rlat(i,j)))
          IF(denom.eq.0.) denom=1.0E-10
          emfact(i,j)=scmap*projc2/denom
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          denom=cos( d2rad*rlat(i,j) )
          IF(denom.lt.1.0E-06) THEN
            emfact(i,j)=1.0e+10
          ELSE
            emfact(i,j)=scmap*(projc2/denom)
     :               *(tan(d2rad*(45.-0.5*jpole*rlat(i,j)))
     :               /projc3)**projc4
          END IF
          emfact(i,j)=amax1(emfact(i,j),1.0e-10)
          emfact(i,j)=amin1(emfact(i,j),1.0e+10)
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          denom=cos( d2rad*rlat(i,j) )
          IF(denom.eq.0.) denom=1.0E-10
          emfact(i,j)=projc2/denom
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          denom=cos( d2rad*rlat(i,j) )
          IF(denom.eq.0.) denom=1.0E-10
          emfact(i,j)=projc3/denom
  400   CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE XYTOMF                     ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE XYTOMF(idim,jdim,x,y,emfact)
c
c#######################################################################
c
c     PURPOSE:
c
c     Determine the map scale factor, emfact, given x,y in the projected
c     space.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/11/93.   
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       idim     Array dimension in x direction.
c       jdim     Array dimension in y direction.
c
c       x        x coordinate values (meters if scmap=1.0)
c       y        y coordinate values (meters if scmap=1.0)
c
c     OUTPUT:
c
c       emfact    Vector of map scale factors corresponding to the
c                input x,y's.
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim       ! array dimensions
      real x(idim)            ! x map coordinate
      real y(jdim)            ! y map coordinate
      real emfact(idim,jdim)  ! local map scale factor
 
      real d2rad,r2deg
      parameter (d2rad=3.141592654/180.,
     :           r2deg=180./3.141592654)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i,j
      real xabs,yabs,rlat,ratio,radius,denom
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
      IF( jproj.eq.0 ) THEN
        DO 10 j=1,jdim
        DO 10 i=1,idim
          emfact(i,j)=1.0
  10    CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          xabs=x(i)+xorig
          yabs=y(j)+yorig
          radius=sqrt( xabs*xabs + yabs*yabs )/projc3
          rlat = 90. - 2.*r2deg*atan(radius)
          rlat=amin1(rlat, 90.)
          rlat=amax1(rlat,-90.)
          denom=(1. + sin(d2rad*rlat))
          IF(denom.eq.0.) denom=1.0E-10
          emfact(i,j)=scmap*projc2/denom
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          xabs=x(i)+xorig
          yabs=y(j)+yorig
          radius=sqrt( xabs*xabs+ yabs*yabs )
          ratio=projc3*((radius/(projc1*projc2))**(1./projc4))
          rlat=90. -2.*r2deg*(atan(ratio))
          rlat=amin1(rlat, 90.)
          rlat=amax1(rlat,-90.)
          denom=cos( d2rad*rlat )
          IF(denom.eq.0.) denom=1.0E-10
          emfact(i,j)=scmap*(projc2/denom)
     :               *(tan(d2rad*(45.-0.5*rlat))/projc3)**projc4
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
          yabs=y(j)+yorig
          rlat=90. - 2.*r2deg*atan(exp(-yabs/projc3))
          rlat=amin1(rlat, 90.)
          rlat=amax1(rlat,-90.)
          denom=cos( d2rad*rlat )
          IF(denom.eq.0.) denom=1.0E-10
          DO 300 i=1,idim
            emfact(i,j)=projc2/denom
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
          yabs=y(j)+yorig
          denom=cos( d2rad*yabs )
          IF(denom.eq.0.) denom=1.0E-10
          DO 400 i=1,idim
            emfact(i,j)=projc3/denom
 400    CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE DDROTUV                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE DDROTUV(nsta,stalon,dd,ff,ddrot,umap,vmap)
c
c#######################################################################
c
c     PURPOSE:
c
c     Rotate wind from earth direction to map orientation.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/20/93.   
c
c     MODIFICATION HISTORY:
c     03/30/95  (K. Brewster)
c     Removed the map scale factor from the conversion of winds
c     from u,v on the earth to projection u,v.  Affected argument
c     list of ddrotuv.
c
c#######################################################################
c
c     INPUT:
c
c       nsta      array dimension
c
c       stalon    longitude (degrees E)
c
c       dd        wind direction (degrees from north)
c       ff        wind speed
c
c     OUTPUT:
c
c       ddrot     wind direction rotated to map orientation
c
c       umap      u wind component on map (same units as ff)
c       vmap      v wind component on map (same units as ff)
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer nsta               ! array dimension
      real stalon(nsta)          ! longitude (degrees E)
      real dd(nsta)              ! wind direction
      real ff(nsta)              ! speed
      real ddrot(nsta)           ! wind direction rotated to map orientation
      real umap(nsta)            ! u wind component on map
      real vmap(nsta)            ! v wind component on map
 
      real d2rad,r2deg
      parameter (d2rad=3.141592654/180.,
     :           r2deg=180./3.141592654)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i
      real arg

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection.
c     Just do conversion from ddff to u,v.
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        DO 50 i=1,nsta
          ddrot(i)=dd(i)
          arg = (ddrot(i) * d2rad)
          umap(i) = -ff(i) * sin(arg)
          vmap(i) = -ff(i) * cos(arg)
  50    CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 i=1,nsta
          ddrot(i)=dd(i) + rota - stalon(i)
          arg = (ddrot(i) * d2rad)
          umap(i) = -ff(i) * sin(arg)
          vmap(i) = -ff(i) * cos(arg)
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 i=1,nsta
          ddrot(i)=dd(i) + projc4*(rota - stalon(i))
          arg = (ddrot(i) * d2rad)
          umap(i) = -ff(i) * sin(arg)
          vmap(i) = -ff(i) * cos(arg)
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 i=1,nsta
          ddrot(i)=dd(i)
          arg = (ddrot(i) * d2rad)
          umap(i) = -ff(i) * sin(arg)
          vmap(i) = -ff(i) * cos(arg)
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 i=1,nsta
          ddrot(i)=dd(i)
          arg = (ddrot(i) * d2rad)
          umap(i) = -ff(i) * sin(arg)
          vmap(i) = -ff(i) * cos(arg)
  400   CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE UVROTDD                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE UVROTDD(idim,jdim,elon,umap,vmap,dd,ff)
c
c#######################################################################
c
c     PURPOSE:
c     Convert winds u, v in map coordinates to wind direction and speed 
c     in earth coordinates.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     11/20/93.   
c
c     MODIFICATION HISTORY:
c     03/30/95  (K. Brewster)
c     Removed the map scale factor from the conversion of winds
c     from u,v on the earth to projection u,v.  Affected argument
c     list of uvrotdd.
c
c#######################################################################
c
c     INPUT:
c       idim       Array dimension in the x direction
c       jdim       Array dimension in the y direction
c
c       elon       Earth longitude (degrees E)
c
c       umap       u wind component on map
c       vmap       v wind component on map
c
c     OUTPUT:
c       dd         wind direction on earth
c       ff         wind speed on earth
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim       ! array dimensions
      real elon(idim,jdim)    ! longitude (degrees E)
      real umap(idim,jdim)    ! u wind component on map
      real vmap(idim,jdim)    ! v wind component on map
 
      real dd(idim,jdim)      ! direction
      real ff(idim,jdim)      ! wind speed 
 
      real d2rad,r2deg
      parameter (d2rad=3.141592654/180.,
     :           r2deg=180./3.141592654)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables: 
c
c#######################################################################
c
      integer i,j
      real dlon

c#######################################################################
c
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        DO 50 j=1,jdim
        DO 50 i=1,idim
          ff(i,j) = sqrt(umap(i,j)*umap(i,j) + vmap(i,j)*vmap(i,j))
 
          IF(vmap(i,j).gt.0.) THEN
            dlon=r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(vmap(i,j).lt.0.) THEN
            dlon=180. + r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(umap(i,j).ge.0.) THEN
            dlon=90.
          ELSE
            dlon=-90.
          END IF
 
          dd(i,j)= dlon + 180.
          dd(i,j)= dd(i,j)-360.*(nint(dd(i,j))/360)
  50    CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
 
          ff(i,j) = sqrt(umap(i,j)*umap(i,j) + vmap(i,j)*vmap(i,j))
 
          IF(vmap(i,j).gt.0.) THEN
            dlon=r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(vmap(i,j).lt.0.) THEN
            dlon=180. + r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(umap(i,j).ge.0.) THEN
            dlon=90.
          ELSE
            dlon=-90.
          END IF
 
          dd(i,j)= dlon + 180. + elon(i,j) - rota
          dd(i,j)= dd(i,j)-360.*(nint(dd(i,j))/360)
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          ff(i,j) = sqrt(umap(i,j)*umap(i,j) + vmap(i,j)*vmap(i,j))
 
          IF(vmap(i,j).gt.0.) THEN
            dlon=r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(vmap(i,j).lt.0.) THEN
            dlon=180. + r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(umap(i,j).ge.0.) THEN
            dlon=90.
          ELSE
            dlon=-90.
          END IF
 
          dd(i,j)= dlon + 180. + projc4*(elon(i,j) - rota)
          dd(i,j)= dd(i,j)-360.*(nint(dd(i,j))/360)
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          ff(i,j) = sqrt(umap(i,j)*umap(i,j) + vmap(i,j)*vmap(i,j))
 
          IF(vmap(i,j).gt.0.) THEN
            dlon=r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(vmap(i,j).lt.0.) THEN
            dlon=180. + r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(umap(i,j).ge.0.) THEN
            dlon=90.
          ELSE
            dlon=-90.
          END IF
 
          dd(i,j)= dlon + 180.
          dd(i,j)= dd(i,j)-360.*(nint(dd(i,j))/360)
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          ff(i,j) = sqrt(umap(i,j)*umap(i,j) + vmap(i,j)*vmap(i,j))
 
          IF(vmap(i,j).gt.0.) THEN
            dlon=r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(vmap(i,j).lt.0.) THEN
            dlon=180. + r2deg*atan(umap(i,j)/vmap(i,j))
          ELSE IF(umap(i,j).ge.0.) THEN
            dlon=90.
          ELSE
            dlon=-90.
          END IF
 
          dd(i,j)= dlon + 180.
          dd(i,j)= dd(i,j)-360.*(nint(dd(i,j))/360)
  400   CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE UVETOMP                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1993-1994               ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE UVETOMP(idim,jdim,uear,vear,lon,umap,vmap)
c
c#######################################################################
c
c     PURPOSE:
c
c     Transform u, v wind from earth coordinates to map coordinates.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     04/30/94.
c
c     MODIFICATION HISTORY:
c     03/30/95  (K. Brewster)
c     Removed the map scale factor from the conversion of winds
c     from u,v on the earth to projection u,v.  Affected argument
c     list of uvetomp.
c     04.30/96  (KB)
c     Streamlined the computation for iproj=1 and iproj=2.
c
c#######################################################################
c
c     INPUT:
c
c       idim       Array dimension in the x direction
c       jdim       Array dimension in the y direction
c
c       uear       u (eastward) wind component on earth
c       vear       v (northwrd) wind component on earth
c
c       lon        earth longitude
c
c     OUTPUT:
c
c       umap       u wind component on map
c       vmap       v wind component on map
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim        ! array dimensions
      real uear(idim,jdim)     ! u (eastward) wind component on earth
      real vear(idim,jdim)     ! v (northward) wind component on earth
      real lon(idim,jdim)      ! longitude (degrees east)
 
      real umap(idim,jdim)     ! u wind component on map
      real vmap(idim,jdim)     ! v wind component on map
 
      real d2rad,r2deg
      parameter (d2rad=3.141592654/180.,
     :           r2deg=180./3.141592654)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j
      real dlon,arg,dxdlon,dydlon,utmp,vtmp
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        DO 50 j=1,jdim
        DO 50 i=1,idim
          umap(i,j) = uear(i,j)
          vmap(i,j) = vear(i,j)
  50    CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          dlon=(rota-lon(i,j))
          arg=d2rad*dlon
          dxdlon=cos(arg)
          dydlon=sin(arg)
          utmp=uear(i,j)
          vtmp=vear(i,j)
          umap(i,j)=utmp*dxdlon - vtmp*dydlon
          vmap(i,j)=vtmp*dxdlon + utmp*dydlon
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          dlon=(rota-lon(i,j))
          arg=d2rad*projc4*(dlon - 360.*nint(dlon/360.))
          dxdlon=cos(arg)
          dydlon=sin(arg)
          utmp=uear(i,j)
          vtmp=vear(i,j)
          umap(i,j)=utmp*dxdlon - vtmp*dydlon
          vmap(i,j)=vtmp*dxdlon + utmp*dydlon
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          umap(i,j) = uear(i,j)
          vmap(i,j) = vear(i,j)
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          umap(i,j) = uear(i,j)
          vmap(i,j) = vear(i,j)
 400    CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE UVMPTOE                    ######
c     ######                                                      ######
c     ######                Copyright (c) 1994                    ######
c     ######    Center for Analysis and Prediction of Storms      ######
c     ######    University of Oklahoma.  All rights reserved.     ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE UVMPTOE(idim,jdim,umap,vmap,lon,uear,vear)
c
c#######################################################################
c
c     PURPOSE:
c
c     Transform u, v wind from map coordinates to earth coordinates.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     04/30/94.
c
c     MODIFICATION HISTORY:
c     03/30/95  (K. Brewster)
c     Removed the map scale factor from the conversion of winds
c     from u,v on the map to earth u,v.  Affected argument
c     list of uvmptoe.
c     04/30/96  (KB)
c     Streamlined the computation for iproj=1 and iproj=2.
c
c#######################################################################
c
c     INPUT:
c
c       idim           Array dimension in x direction
c       jdim           Array dimension in y direction
c
c       umap           u wind component on map
c       vmap           v wind component on map
c
c       lon            Longitude (degrees E)
c
c     OUTPUT:
c
c       uear           u (eastward) wind component on earth
c       vear           v (northward) wind component on earth
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer idim,jdim       ! array dimensions
      real lon(idim,jdim)     ! longitude (degrees E)
      real umap(idim,jdim)    ! u wind component on map
      real vmap(idim,jdim)    ! v wind component on map
 
      real uear(idim,jdim)    ! u (eastward) wind component on earth
      real vear(idim,jdim)    ! v (northward) wind component on earth
 
      real d2rad,r2deg
      parameter (d2rad=3.141592654/180.,
     :           r2deg=180./3.141592654)
 
      integer jproj,jpole
      real trulat(2),rota,scmap,xorig,yorig,
     :     projc1,projc2,projc3,projc4,projc5
      common /projcst/ jproj,jpole,trulat,rota,scmap,xorig,yorig,
     :                 projc1,projc2,projc3,projc4,projc5
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j
      real ff,dlon,dd,ddrot,arg,utmp,vtmp,dxdlon,dydlon
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c#######################################################################
c
c     No map projection
c
c#######################################################################
c
      IF( jproj.eq.0 ) THEN
        DO 50 j=1,jdim
        DO 50 i=1,idim
          uear(i,j) = umap(i,j)
          vear(i,j) = vmap(i,j)
  50    CONTINUE
c
c#######################################################################
c
c     Polar Stereographic projection
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is the numerator of emfact, the map image scale factor.
c         projc3 is projc2 times the scaled earth's radius.
c
c#######################################################################
c
      ELSE IF( jproj.eq.1 ) THEN
        DO 100 j=1,jdim
        DO 100 i=1,idim
          dlon=(rota-lon(i,j))
          arg=d2rad*dlon
          dxdlon=cos(arg)
          dydlon=sin(arg)
          utmp=umap(i,j)
          vtmp=vmap(i,j)
          uear(i,j)=utmp*dxdlon + vtmp*dydlon
          vear(i,j)=vtmp*dxdlon - utmp*dydlon
 100    CONTINUE
c
c#######################################################################
c
c     Lambert Conformal Conic Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius/n
c         projc2 is cos of trulat(1)
c         projc3 is tan (45. - trulat/2) a const for local map scale
c         projc4 is the cone constant, n
c
c#######################################################################
c
      ELSE IF( jproj.eq.2 ) THEN
        DO 200 j=1,jdim
        DO 200 i=1,idim
          dlon=(rota-lon(i,j))
          arg=d2rad*projc4*(dlon - 360.*nint(dlon/360.))
          dxdlon=cos(arg)
          dydlon=sin(arg)
          utmp=umap(i,j)
          vtmp=vmap(i,j)
          uear(i,j)=utmp*dxdlon + vtmp*dydlon
          vear(i,j)=vtmp*dxdlon - utmp*dydlon
 200    CONTINUE
c
c#######################################################################
c
c     Mercator Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2
c
c#######################################################################
c
      ELSE IF(jproj.eq.3) THEN
        DO 300 j=1,jdim
        DO 300 i=1,idim
          uear(i,j) = umap(i,j)
          vear(i,j) = vmap(i,j)
 300    CONTINUE
c
c#######################################################################
c
c     Lat, Lon Projection.
c     For this projection:
c         projc1 is the scaled earth's radius, scale times eradius
c         projc2 is cos of trulat(1)
c         projc3 is projc1 times projc2 times 180/pi
c
c#######################################################################
c
      ELSE IF(jproj.eq.4) THEN
        DO 400 j=1,jdim
        DO 400 i=1,idim
          uear(i,j) = umap(i,j)
          vear(i,j) = vmap(i,j)
 400    CONTINUE
      ELSE
        write(6,'(i4,a)') jproj,' projection is not supported'
        STOP
      END IF
      RETURN
      END
