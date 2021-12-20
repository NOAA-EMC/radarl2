#include <cmath>

using namespace std;



// ##########################################################################
//
//  radarlib3d.cc:  The common library functions for radar beam geometry
//                  calculations
//
// ##########################################################################
//
//      Author: Jian Zhang (CIMMS/NSSL),  Wenwu Xia (CIMMS/NSSL)
//      November, 2000
//
//      Modification History:
//      12/12/2000     Jian Zhang (CIMMS/NSSL)
//      Added documentation.
//
// ##########################################################################




//######################################################################
//
//    Function gcircle_func:
//
//    Following a great circle path from a point specified as
//    lat,lon on the earth's surface leaving at heading given
//    by head for a distance given by dist, give the location
//    of the end point.  Useful for finding the lat,lon of a
//    radar gate.
//
//######################################################################



void gcircle_func(float lat1,float lon1,float head,
                 float dist,float& lat2,float& lon2){
  
      double pi,deg2rad,rad2deg,eradius,one,mone;
      pi=3.141592654;
      deg2rad=(pi/180.);
      rad2deg=(180./pi);
      eradius=6371000.;
      one=1.;
      mone=-1.;

      double alat1,alat2,dlon,arcdst,cosdst,coshd;
      double denom,sinlat2,cosdlon;

      alat1=deg2rad*(double)lat1;
      arcdst=(double)dist/eradius;
      cosdst=cos(arcdst);
      coshd=cos(deg2rad*(double)head);

      sinlat2=coshd*cos(alat1)*sin(arcdst) + sin(alat1)*cosdst;
      if(sinlat2<mone)sinlat2=mone;
      if(sinlat2>one)sinlat2=one;
      alat2=asin(sinlat2);
      lat2=rad2deg*alat2;

      denom=cos(alat1)*cos(alat2);
      if(denom!=0.){
        cosdlon=(cosdst - sin(alat1)*sinlat2)/(cos(alat1)*cos(alat2));
        if(cosdlon<mone)cosdlon=mone;
        if(cosdlon>one)cosdlon=one;
        dlon=rad2deg*acos(cosdlon);
      if(sin(deg2rad*head)<0.) dlon=-dlon;
        lon2=lon1+dlon;
      }
      else
        lon2=lon1;
};




//######################################################################
//
//    FUNCTION beamhgt_func:
//
//    Calculate the height of the radar beam and the along-
//    ground distance from the radar as a function
//    distance along the radar beam (range) and radar
//    elevation angle (elvang).
//
//    This method assumes dn/dh is constant such that the
//    beam curves with a radius of 4/3 of the earth's radius.
//    This is from Eq. 2.28 of Doviak and Zrnic', Doppler Radar
//    and Weather Observations, 1st Ed.
//
//######################################################################


void beamhgt_func(float elvang, float range,float &height){
  
      double eradius,frthrde,eighthre,fthsq,deg2rad;
      eradius=6371000.;
      frthrde=(4.*eradius/3.);
      eighthre=(8.*eradius/3.);
      fthsq=(frthrde*frthrde);
      deg2rad=(3.14592654/180.);

      double elvrad,hgtdb,drange;

      elvrad=deg2rad*(double)elvang;
      drange=(double)range;
      hgtdb = sqrt(drange*drange + fthsq +
                    eighthre*drange*sin(elvrad)) -
                    frthrde;
      height=hgtdb;
};


void beamhgt_func1(float elvang, float range,float &height,float &sfcrng){
  
      double eradius,frthrde,eighthre,fthsq,deg2rad;
      eradius=6371000.;
      frthrde=(4.*eradius/3.);
      eighthre=(8.*eradius/3.);
      fthsq=(frthrde*frthrde);
      deg2rad=(3.14592654/180.);

      double elvrad,hgtdb,rngdb,drange;

      elvrad=deg2rad*(double)elvang;
      drange=(double)range;
      hgtdb = sqrt(drange*drange + fthsq +
                    eighthre*drange*sin(elvrad)) -
                    frthrde;
      height=hgtdb;
      rngdb = frthrde * asin (drange*cos(elvrad)/(frthrde + hgtdb) );
      sfcrng=rngdb;
};




//######################################################################
//
//    FUNCTION beamelv_func:
//
//    Calculate the elevation angle (elvang) and the along
//    ray-path distance (range) of a radar beam
//    crossing through the given height and along-ground
//    distance.
//
//    This method assumes dn/dh is constant such that the
//    beam curves with a radius of 4/3 of the earth's radius.
//    This is dervied from Eq. 2.28 of Doviak and Zrnic',
//    Doppler Radar and Weather Observations, 1st Ed.
//
//######################################################################



void beamelv_func(float height,float sfcrng,float &elvang,float &range){
      
      double eradius,frthrde,eighthre,fthsq,rad2deg;
      eradius=6371000.;
      frthrde=(4.*eradius/3.);
      eighthre=(8.*eradius/3.);
      fthsq=(frthrde*frthrde);
      rad2deg=(180./3.14592654);

      double elvrad,hgtdb,rngdb,drange;

// ##########################################################################
//
//    Beginning of calculations.
//
// ##########################################################################

      if(sfcrng>0.){

        hgtdb=frthrde+(double)height;
        rngdb=(double)sfcrng/frthrde;

        elvrad = atan((hgtdb*cos(rngdb) - frthrde)/(hgtdb * sin(rngdb)));
        drange = (hgtdb*sin(rngdb))/cos(elvrad);
        elvang=rad2deg*elvrad;
        range=drange;
      }

      else {
        elvang=90.;
        range=height;
      }
};  // end of function beam_elv
 




//######################################################################
//
//    FUNCTION disthead_func:
//
//    Given a pair of locations specified in lat,lon on the earth's
//    surface find the distance between them and the great circle
//    heading from the first point to the second point.  Spherical
//    geometry is used, which is more than adequate for radar
//    and local modelling applications.
//
//######################################################################


void disthead_func(float lat1,float lon1,float lat2,
             float lon2,float &headng,float &dist){

      double pi,deg2rad,rad2deg,eradius,one,mone;
      pi=3.141592654;
      deg2rad=(pi/180.);
      rad2deg=(180./pi);
      eradius=6371000.;     
      one=1.;
      mone=-1.;

      double alat1,alat2,dlon,arcdst,cosdst,coshd,denom;

//#################################################################
//
//     Find arc length using law of cosines
//
//     cos a = cos b cos c + sin b sin c cos A
//     cos (1 to 2) = sin(lat1) * sin (lat2)
//                  +(cos(lat1) * sin (lat2)
//                    * cos (lon1 - lon2)
//
//#################################################################

      alat1=deg2rad * (double)lat1;
      alat2=deg2rad * (double)lat2;
      dlon=deg2rad*(double)(lon2-lon1);
      cosdst = sin(alat1) * sin(alat2)  +
              cos(alat1) * cos(alat2) * cos(dlon);
      arcdst = acos(cosdst);
      dist = eradius*arcdst;

      denom=cos(alat1)*sin(arcdst);
      headng=0.;


      if(fabs(denom)>1.E-06){
        coshd=(sin(alat2) - sin(alat1)*cosdst) / denom;
        if(coshd<mone)coshd=mone;
        if(coshd>one)coshd=one;
        headng=rad2deg*acos(coshd);
        if( sin(dlon)<0 ) headng = 360.-headng;
      }
      else if( fabs(cos(alat1))<1.E-06 && alat1>0.) headng=180.;
};





float hgt_to_range(float height,float elvang)
{
//
//#######################################################################
//
//     PURPOSE:
//
//     Calculate the range of a point w.r.t. a radar given
//     the height and elevetion of the point w.r.t. the radar.
//
//     This method assumes dn/dh is constant such that the
//     beam curves with a radius of 4/3 of the earth's radius.
//     This is from Eq. 2.28 of Doviak and Zrnic', Doppler Radar
//     and Weather Observations, 1st Ed.
//
//#######################################################################
//
//     AUTHOR: Jian Zhang
//     09/02/99
//
//#######################################################################
//
//     INPUT:
//       height   Height (meters) of beam above ground.
//
//     OUTPUT:
//       hgt_to_range    Distance (meters) along radar beam from radar
//
//#######################################################################

      double eradius,frthrde,eighthre,fthsq,deg2rad;
      eradius=6371000.;
      frthrde=(4.*eradius/3.);
      eighthre=(8.*eradius/3.);
      fthsq=(frthrde*frthrde);
      deg2rad=(3.14592654/180.);

      double elvrad,hgtdb,rngdb;
      double arg;
      elvrad=deg2rad*(double)elvang;
      hgtdb=(double)height;

      arg=eighthre*sin(elvrad);
      rngdb=sqrt(arg*arg
                 +4.0*(hgtdb*hgtdb+eighthre*hgtdb) );
      return( 0.5* (rngdb - arg) );
}

