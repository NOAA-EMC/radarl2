c      program test
c      implicit none
c      real lat1,lon1,lat2,lon2,dist,head
c      real elev,range,azim
c      real sfcrng,height,dhdr
c  10  CONTINUE
c      print *, ' Enter height, sfcrange: '
c      read(5,*) height,sfcrng
c      IF(height.lt.-1000.) STOP
c      CALL beamelv(height,sfcrng,elev,range)
c      print *, ' elv, range = ',elev,range
c      CALL beamhgt(elev,range,height,sfcrng)
c      print *, ' height,sfcrng = ',height,sfcrng

c      range=0.0
c      height=0.0
c      CALL beamrng(elev,sfcrng,range,height)
c      print *, ' range,height = ', range,height
 
c      print *, ' Enter lat1,lon1: '
c      read (5,*) lat1,lon1
c      lat1=35.
c      lon1=-100.
c      print *, ' Enter elev,azim,range '
c      read (5,*) elev,azim,range
c      IF(elev.gt.90.) STOP
c      CALL beamhgt(elev,range,height,sfcrng)
c      print *, ' beam height = ',height
c      print *, ' sfc range   = ',sfcrng
c      CALL dhdrange(elev,range,dhdr)
c      print *, ' local elv   = ',locelva 
c      CALL gcircle(lat1,lon1,azim,sfcrng,lat2,lon2)
c      print *, ' gate lat,lon = ',lat2,lon2
c      CALL disthead(lat1,lon1,lat2,lon2,head,dist)
c      print *, ' distance, heading: ',dist,head
c
c      GO TO 10
c      END
 
      SUBROUTINE beamhgt(elvang,range,height,sfcrng)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the height of the radar beam and the along-
c     ground distance from the radar as a function
c     distance along the radar beam (range) and radar
c     elevation angle (elvang).
c
c     This method assumes dn/dh is constant such that the
c     beam curves with a radius of 4/3 of the earth's radius.
c     This is from Eq. 2.28 of Doviak and Zrnic', Doppler Radar
c     and Weather Observations, 1st Ed.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     06/22/95
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       elvang   Elevation angle (degrees) of radar beam
c       range    Distance (meters) along radar beam from radar
c
c     OUTPUT:
c       height   Height (meters) of beam above ground. 
c       sfcrng   Distance (meters) of point along ground from radar.
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      real elvang
      real range
      real height
      real sfcrng
c
      double precision eradius,frthrde,eighthre,fthsq,deg2rad
      parameter (eradius=6371000.,
     :           frthrde=(4.*eradius/3.),
     :           eighthre=(8.*eradius/3.),
     :           fthsq=(frthrde*frthrde),
     :           deg2rad=(3.14592654/180.))
c
      double precision elvrad,hgtdb,rngdb,drange
c
      elvrad=deg2rad*dble(elvang)
      drange=dble(range)
      hgtdb = sqrt(drange*drange + fthsq + 
     :              eighthre*drange*sin(elvrad)) -
     :              frthrde
      height=hgtdb
      rngdb = frthrde * 
     :         asin (drange*cos(elvrad)/(frthrde + hgtdb) )
      sfcrng=rngdb
      RETURN
      END
c
      SUBROUTINE beamelv(height,sfcrng,elvang,range)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the elevation angle (elvang) and the along
c     ray-path distance (range) of a radar beam
c     crossing through the given height and along-ground
c     distance.
c
c     This method assumes dn/dh is constant such that the
c     beam curves with a radius of 4/3 of the earth's radius.
c     This is dervied from Eq. 2.28 of Doviak and Zrnic',
c     Doppler Radar and Weather Observations, 1st Ed.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     10/10/95
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c       height   Height (meters) of beam above ground. 
c       sfcrng   Distance (meters) of point along ground from radar.
c
c     OUTPUT
c       elvang   Elevation angle (degrees) of radar beam
c       range    Distance (meters) along radar beam from radar
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      real height
      real sfcrng
      real elvang
      real range
c
      double precision eradius,frthrde,eighthre,fthsq,rad2deg
      parameter (eradius=6371000.,
     :           frthrde=(4.*eradius/3.),
     :           eighthre=(8.*eradius/3.),
     :           fthsq=(frthrde*frthrde),
     :           rad2deg=(180./3.14592654))
c
      double precision elvrad,hgtdb,rngdb,drange
c
      IF(sfcrng.gt.0.) THEN

        hgtdb=frthrde+dble(height)
        rngdb=dble(sfcrng)/frthrde

        elvrad = atan((hgtdb*cos(rngdb) - frthrde)/(hgtdb * sin(rngdb)))
        drange = (hgtdb*sin(rngdb))/cos(elvrad) 
        elvang=rad2deg*elvrad
        range=drange

      ELSE

        elvang=90.
        range=height

      END IF
      RETURN
      END
c
      SUBROUTINE dhdrange(elvang,range,dhdr)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the local change in height of the radar
c     beam with respect to a change in range.  Due to 
c     curvature of the beam and the earth's surface this is
c     generally different what would be calculated from the
c     elevation angle measured at the radar.  This derivative
c     is needed for finding 3-d velocities from radial winds
c     and accounting for terminal velocity of precipitation.
c
c     This formulation, consistent with subroutine beamhgt,
c     assumes a 4/3 earth radius beam curvature.  This formula
c     is obtained by differentiating Eq 2.28 of Doviak and
c     Zrnic', Doppler Radar and Weather Observations, 1st Ed.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     06/22/95
c
c     MODIFICATION HISTORY:
c
c
c#######################################################################
c
c     INPUT:
c
c       elvang   Elevation angle (degrees) of radar beam
c       range    Distance (meters) along radar beam from radar
c
c     OUTPUT:
c       dhdr     Change in height per change in range (non-dimensional)
c
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c

      implicit none
      real range
      real elvang
      real dhdr
c
      double precision eradius,frthrde,eighthre,fthsq,deg2rad,rad2deg
      parameter (eradius=6371000.,
     :           frthrde=(4.*eradius/3.),
     :           eighthre=(8.*eradius/3.),
     :           fthsq=(frthrde*frthrde),
     :           deg2rad=(3.14592654/180.))
c
      double precision sinelv,dhdrdb,drange
c
      drange=dble(range)
      sinelv=sin(deg2rad*dble(elvang))
      dhdrdb = (drange+frthrde*sinelv)/
     :       sqrt(drange*drange + fthsq + eighthre*drange*sinelv)
      dhdr = dhdrdb
c
      RETURN
      END
c
      SUBROUTINE disthead(lat1,lon1,lat2,lon2,headng,dist)
c
c#######################################################################
c
c     PURPOSE:
c
c     Given a pair of locations specified in lat,lon on the earth's
c     surface find the distance between them and the great circle
c     heading from the first point to the second point.  Spherical
c     geometry is used, which is more than adequate for radar
c     and local modelling applications.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     06/22/95
c
c     MODIFICATION HISTORY:
c
c
c#######################################################################
c
c     INPUT:
c
c       lat1   Latitude (degrees, north positive) of first point
c       lon1   Latitude (degrees, east positive) of first point
c       lat2   Latitude (degrees, north positive) of second point
c       lon2   Latitude (degrees, east positive) of second point
c
c     OUTPUT:
c
c       headng Heading (degrees, north zero) of great circle path
c              at first point.
c       dist   Distance (meters) between two points along great circle
c              great circle arc.
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
C
C     Arguments
C
      real lat1,lon1
      real lat2,lon2
      real headng
      real dist
C
C     Parameters
C
      double precision pi,deg2rad,rad2deg,eradius,one,mone
      parameter (pi=3.141592654,
     :           deg2rad=(pi/180.),
     :           rad2deg=(180./pi),
     :           eradius=6371000.,      ! Earth radius in meters
     :           one=1.,
     :           mone=-1.)
C
C     Misc internal variables
C
      double precision alat1,alat2,dlon,arcdst,cosdst,coshd,denom
C
C     Find arc length using law of cosines 
C
C     cos a = cos b cos c + sin b sin c cos A
C     cos (1 to 2) = sin(lat1) * sin (lat2)
C                  +(cos(lat1) * sin (lat2)
C                    * cos (lon1 - lon2)
C
      alat1=deg2rad * dble(lat1)
      alat2=deg2rad * dble(lat2)
      dlon=deg2rad*dble(lon2-lon1)
      cosdst = sin(alat1) * sin(alat2)  +
     +         cos(alat1) * cos(alat2) * cos(dlon)  
      arcdst = acos(cosdst)
      dist = eradius*arcdst
c
      denom=cos(alat1)*sin(arcdst)
      headng=0.
      IF(abs(denom).gt.1.E-06) THEN
        coshd=(sin(alat2) - sin(alat1)*cosdst) / denom
        coshd=dmax1(coshd,mone)
        coshd=dmin1(coshd,one)
        headng=rad2deg*acos(coshd)
        IF( sin(dlon).lt.0 ) headng = 360.-headng
      ELSE IF( abs(cos(alat1)).lt.1.E-06 .and. alat1.gt.0.) THEN
        headng=180.
      END IF
c
      RETURN
      END
c
      SUBROUTINE gcircle(lat1,lon1,head,dist,lat2,lon2)
c
c#######################################################################
c
c     PURPOSE:
c
c     Following a great circle path from a point specified as
c     lat,lon on the earth's surface leaving at heading given
c     by head for a distance given by dist, give the location
c     of the end point.  Useful for finding the lat,lon of a
c     radar gate.
c
c#######################################################################
c
c     AUTHOR: Keith Brewster
c     06/22/95
c
c     MODIFICATION HISTORY:
c
c
c#######################################################################
c
c     INPUT:
c       lat1   Latitude (degrees, north positive) of first point
c       lon1   Latitude (degrees, east positive) of first point
c       head   Heading (degrees, north zero) of great circle path
c              at first point.
c       dist   Distance (meters) between two points along great circle
c              great circle arc.
c
c     OUTPUT:
c
c       lat2   Latitude (degrees, north positive) of second point
c       lon2   Latitude (degrees, east positive) of second point
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c

      implicit none
C
C     Arguments
C
      real lat1,lon1
      real head
      real dist
      real lat2,lon2
C
C     Parameters
C
      double precision pi,deg2rad,rad2deg,eradius,one,mone
      parameter (pi=3.141592654,
     :           deg2rad=(pi/180.),
     :           rad2deg=(180./pi),
     :           eradius=6371000.,      ! Earth radius in meters
     :           one=1.,
     :           mone=-1.)
C
C     Misc internal variables
C
      double precision alat1,alat2,dlon,arcdst,cosdst,coshd
      double precision denom,sinlat2,cosdlon
c
      alat1=deg2rad*dble(lat1)
      arcdst=dble(dist)/eradius
      cosdst=cos(arcdst)
      coshd=cos(deg2rad*dble(head))
c
      sinlat2=coshd*cos(alat1)*sin(arcdst) + sin(alat1)*cosdst
      sinlat2=dmax1(sinlat2,mone)
      sinlat2=dmin1(sinlat2,one)
      alat2=asin(sinlat2)
      lat2=rad2deg*alat2
c
      denom=cos(alat1)*cos(alat2)
      IF(denom.ne.0.) THEN
        cosdlon=(cosdst - sin(alat1)*sinlat2)/(cos(alat1)*cos(alat2))
        cosdlon=dmax1(cosdlon,mone)
        cosdlon=dmin1(cosdlon,one)
        dlon=rad2deg*acos(cosdlon)
        IF(sin(deg2rad*head).lt.0.) dlon=-dlon
        lon2=lon1+dlon
      ELSE
        lon2=lon1
      END IF
      RETURN
      END
c
c
      SUBROUTINE beamrng(elvang,sfcrng,range,height)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the height of the radar beam and the along-
c     radar beam distance from the radar as a function
c     distance along ground and radar
c     elevation angle (elvang).
c
c     This method assumes dn/dh is constant such that the
c     beam curves with a radius of 4/3 of the earth's radius.
c     This is from Eq. 2.28 of Doviak and Zrnic', Doppler Radar
c     and Weather Observations, 1st Ed.
c
c#######################################################################
c
c     INPUT:
c
c       elvang   Elevation angle (degrees) of radar beam
c       sfcrng   Distance (meters) of point along ground from radar.
c
c     OUTPUT:
c       height   Height (meters) of beam above ground.
c       range    Distance (meters) along radar beam from radar
c
c#######################################################################
c
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
      real elvang
      real sfcrng
      real range
      real height
c
      double precision eradius,frthrde,eighthre,fthsq,deg2rad
      parameter (eradius=6371000.,
     :           frthrde=(4.*eradius/3.),
     :           eighthre=(8.*eradius/3.),
     :           fthsq=(frthrde*frthrde),
     :           deg2rad=(3.14592654/180.))
c
      double precision elvrad,hgtdb,rngdb,drange
c
      elvrad=deg2rad*dble(elvang)
      rngdb=dble(sfcrng)/frthrde
 
      drange=frthrde * (sin(rngdb)/cos(elvrad+rngdb)) 
      range =drange

      hgtdb = sqrt(drange*drange + fthsq +
     :              eighthre*drange*sin(elvrad)) -
     :              frthrde
      height=hgtdb

      RETURN
      END
c

