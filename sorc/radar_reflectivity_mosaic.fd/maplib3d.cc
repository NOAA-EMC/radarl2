// ##########################################################################
//
//      maplib3d.cc: Implement class function of maplib3d
//
// ##########################################################################
//
//      Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
//      May 20, 2000
//
//      Modification History:
//
// ##########################################################################

#include "maplib3d.h"

using namespace std;

void setmapr::setmap_proj(CONFIG_PARS &mp)
{

//#######################################################################
//
//  Misc. local variables: 
//
//#######################################################################

    float denom1,denom2,denom3;
    int iproj;
    float scale;           // map scale factor
    float latnot1;         // true latitude (degrees N)
    float latnot2;         // true latitude (degrees N)
    float orient;          // orientation longitude (degrees E)

//#######################################################################

    iproj=mp.mapproj;

    latnot1 = mp.trulat1;
    latnot2 = mp.trulat2;

    if(map_scale_fct!= 1.0){ scale  = 1.0/(map_scale_fct); }
    else { scale  = 1.0; }
    orient=mp.trulon;

//#######################################################################

    xorig=0.;
    yorig=0.;
    jproj=abs(iproj);

    if(iproj<0) jpole=-1;
    else jpole=1;


//#######################################################################
//
//  No map projection
//
//#######################################################################

    if( jproj==0 ){ 
    }

//#######################################################################
//
//  Polar Stereographic projection
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is the numerator of emfact, the map image scale factor.
//    projc3 is projc2 times the scaled earth's radius.
//
//#######################################################################

    else if ( jproj==1 )
    {
      trulat1=latnot1;
      trulat2=latnot2;
      rota=orient;
      scmap=scale;
      projc1=scale*eradius;
      projc2=(1. + sin(d2rad*jpole*trulat1) );
      projc3=projc1*projc2;

      if(jpole>0 && mp.debug_flag){ 
        std::cout<<"  Map projection set to Polar Stereographic"<<std::endl;
        std::cout<<"  X origin, Y origin set to 0.,0. at the North Pole."<<std::endl;
      }
      else   {
        std::cout<<"  Map projection set to Polar Stereographic"<<std::endl;
        std::cout<<"  X origin, Y origin set to 0.,0. at the South Pole."<<std::endl;
      }

    } // end else if ( jproj==1 )

//#######################################################################
//
//  Lambert Conformal Conic Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius/n
//    projc2 is cos of trulat(1)
//    projc3 is tan (45. - trulat/2) a const for local map scale
//    projc4 is the cone constant, n
//
//#######################################################################
//
    else if( jproj==2 )
    {
      trulat1=latnot1;
      trulat2=latnot2;
      rota=orient;
      scmap=scale;
      projc2=cos(d2rad*trulat1);
      projc3=tan(d2rad*(45.-0.5*jpole*trulat1));
      denom1=cos(d2rad*trulat2);
      denom2=tan(d2rad*(45.-0.5*jpole*trulat2));
      if(denom2!=0.) denom3=log( projc3/denom2 );
      else   denom3=0.;

      if(denom1!=0.&&denom3!=0.)
      {
        projc4=log( projc2/denom1 ) / denom3;

        if( projc4<0.)
        {
          std::cout<< "  Warning in SETMAPR for Lambert Projection"
              << "  For the true latitudes provided, "
              << trulat1<<" and "<<trulat2
              << "  projection must be from opposite pole...changing pole."
              <<std::endl; 
          jpole=-jpole;
          projc3=tan(d2rad*(45.-0.5*jpole*trulat1) );
          denom2=tan(d2rad*(45.-0.5*jpole*trulat2));

          if(denom2!=0.) denom3=log( projc3/denom2 );
          else denom3=0.;

          if(denom1!=0.&& denom3!=0.){ 
            projc4=log( projc2/denom1 ) / denom3;
          }
          else {
            std::cout<<"  Error (1) in SETMAPR for Lambert Projection"
                <<"  Illegal combination of trulats one: "
                <<trulat1<<" and two: "<<trulat2<<std::endl;
            exit(1);
          }
        }  // end if projc4<0.

        projc1=scale*eradius/projc4;

      } // end if denom1!=0.&&denom3!=0.

      else if(denom3==0. &&denom2!=0.) 
      {                                     // tangent
        projc4=sin(d2rad*jpole*trulat1);

        if( projc4<0.)
        {
          std::cout<<"  Warning in SETMAPR for Lambert Projection"
              <<"  For the true latitudes provided, "
              <<trulat1<<" and "<<trulat2
              <<"projection must be from opposite pole...changing pole."
              <<std::endl;
          jpole=-jpole;
          projc4=sin(d2rad*jpole*trulat1);
        }

        projc1=scale*eradius/projc4;
      }

      else {
        std::cout<<"  Error (1) in SETMAPR for Lambert Projection"
            <<"  Illegal combination of trulats one: "
            <<trulat1<<" and two: "<<trulat2<<std::endl;
        exit(1); 
      }
 
    } // end else if( jproj==2 )

//#######################################################################
//
//  Mercator Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2
//
//#######################################################################

    else if( jproj==3 )
    {
      trulat1=latnot1;
      trulat2=latnot2;
      rota=orient;
      scmap=scale;
      projc1=scale*eradius;
      projc2=cos(d2rad*trulat1);
      projc3=projc1*projc2;
      if(projc2<=0.) { 
        std::cout<<"  Error (1) in SETMAPR for Mercator Projection"
            <<"  Illegal true latitude provided: "<<trulat1
            <<std::endl;
        exit(1);
      }
    }

//#######################################################################
//
//  Lat, Lon Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2 times 180/pi
//
//#######################################################################

    else if( jproj==4 )
    {
      trulat1=latnot1;
      trulat2=latnot2;
      rota=orient;
      scmap=scale;
      projc1=scale*eradius;
      projc2=cos(d2rad*trulat1);
      if(projc2<=0.) { 
        std::cout<<"  Error (1) in SETMAPR for Lat,Lon Projection"
            <<"  Illegal true latitude provided: "<<trulat1
            <<std::endl;
        exit(1);
      }
      projc3=projc1*projc2/d2rad;
    }
    else {
      std::cout<< iproj<<" projection is not supported"<<std::endl;
      exit(1);
    }
};       




 
//     ##################################################################
//     ##################################################################
//     ######                                                      ######
//     ######                FUNCTION   GETMAPR                    ######
//     ######                                                      ######
//     ##################################################################
//     ##################################################################
//
void getmapr(int &iproj,float &scale,float *latnot,float &orient,
                   float &x0,float &y0, setmapr &mp)
{
//
//#######################################################################
//
//     PURPOSE:
//
//     Get the constants for the current map projection, which are stored
//     in the common block named /projcst/.
//
//#######################################################################
//
//     OUTPUT:
//
//       iproj        Map projection number
//                    1=North Polar Stereographic   (-1 South Pole)
//                    2=Northern Lambert Conformal  (-2 Southern)
//                    3=Mercator
//                    4=Lat,Lon
//
//       scale        Map scale factor,  at latitude=latnot
//                    Distance on map = (Distance on earth) * scale
//                    For ARPS model runs, generally this is 1.0
//                    For ARPS plotting this will depend on window
//                    size and the area to be plotted.
//
//       latnot(2)    Real "True" latitude(s) of map projection
//                    (degrees, positive north)
//                    Except for iproj=2, only latnot(1) is used 
//
//       orient       Longitude line that runs vertically on the map.
//                    (degrees, negative west, positive east)
//
//       x0           x coordinate of origin
//       y0           y coordinate of origin
//
//#######################################################################
 
    iproj=mp.jproj*mp.jpole;
    scale=mp.scmap;
    latnot[0]=mp.trulat1;
    latnot[1]=mp.trulat2;
    orient=mp.rota;
    x0=mp.xorig;
    y0=mp.yorig;
};



void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a)
{

//#######################################################################
//
//  PURPOSE:
//
//  Find the x- and y-coordinates for given latitude and longitude
//  on a specific map projection.  Before calling this function, 
//  a proper map projection needs to be setup (e.g., by calling
//  "setmapr".
//
//#######################################################################

    float radius,denom,dlon,ratio;
    float rlat = rrlat;
    float rlon = rrlon;

//#######################################################################
//
//  No map projection
//
//#######################################################################

    if( a.jproj==0 )
    {
      ratio=d2rad*eradius;
      xloc = ratio*rlon - a.xorig;
      yloc= ratio*rlat - a.yorig;
    }

//#######################################################################
//
//  Polar Stereographic projection
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is the numerator of emfact, the map image scale factor.
//    projc3 is projc2 times the scaled earth's radius.
//
//#######################################################################

    else if( a.jproj==1 )
    {
      denom=(1. + sin(d2rad*a.jpole*rlat));
      if(denom==0.) denom=1.0E-10;
      radius=a.jpole*a.projc3*cos(d2rad*rlat)/denom;
      dlon=a.jpole*d2rad*(rlon-a.rota);
      xloc= radius*sin(dlon) - a.xorig;
      yloc=-radius*cos(dlon) - a.yorig;
    }

//#######################################################################
//
//  Lambert Conformal Conic Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius/n
//    projc2 is cos of trulat(1)
//    projc3 is tan (45. - trulat/2) a const for local map scale
//    projc4 is the cone constant, n
//
//#######################################################################

    else if(a.jproj==2 )
    {
      radius= a.projc1*a.projc2*(float)pow((float)(tan(d2rad*(45.0-0.5*a.jpole*rlat)))/a.projc3,
              (float)a.projc4 );                //wang  
      dlon=a.projc4*d2rad*(rlon-a.rota);
      xloc=       radius*sin(dlon) - a.xorig;
      yloc=-a.jpole*radius*cos(dlon) - a.yorig;
    }

//#######################################################################
//
//  Mercator Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2
//
//#######################################################################

    else if(a.jproj==3)
    {
      dlon=rlon-a.rota;
      if(dlon<-180.) dlon=dlon+360.;
      if(dlon> 180.) dlon=dlon-360.;
      xloc=a.projc3*d2rad*dlon - a.xorig;
      denom=tan(d2rad*(45. - 0.5*rlat));
      if( denom<=0. ) denom=1.0E-10;
      yloc=-a.projc3*log(denom) - a.yorig;
    }

//#######################################################################
//
//  Lat, Lon Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2 times 180/pi
//
//#######################################################################

    else if(a.jproj==4)
    {
      xloc=rlon-a.xorig;
      yloc=rlat-a.yorig;
    }
    else {
      std::cout<< a.jproj<<" projection is not supported"<<std::endl;
      exit(1);
    }
};




void xytoll_func(float **rlat,float **rlon,float *x,float *y,
                  int idim, int jdim,setmapr &m)
{
    float xabs,yabs,yjp;
    float radius,ratio,dlon;

//#######################################################################
//
//  No map projection
//
//#######################################################################

    if ( m.jproj==0 )
    {
      ratio=r2deg/eradius;

      for(int i=0;i< idim;i++){
        for(int j=0;j< jdim;j++){
          rlat[i][j] = ratio*(y[j]+m.yorig);
          rlon[i][j] = ratio*(x[i]+m.xorig);
        }  // end of j-loop
      }  // end of i-loop
    }  // end of if ( m.jproj==0 )

//#######################################################################
//
//  Polar Stereographic projection
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is the numerator of emfact, the map image scale factor.
//    projc3 is projc2 times the scaled earth's radius.
//
//#######################################################################

    else if ( m.jproj==1 )
    {
      for(int i=0;i< idim;i++){
        for(int j=0;j< jdim;j++){
          yabs=y[j]+m.yorig;
          xabs=x[i]+m.xorig;
          radius=sqrt( xabs*xabs + yabs*yabs )/m.projc3;
          rlat[i][j] = m.jpole*(90. - 2.*r2deg*atan(radius));
          if(rlat[i][j]>90.)rlat[i][j]=90.0;
          if(rlat[i][j]<-90.)rlat[i][j]=-90.0;
 
          if((m.jpole*yabs)>0.) 
            dlon=180. + r2deg*atan(-xabs/yabs);
          else if((m.jpole*yabs)<0.)
            dlon=r2deg*atan(-xabs/yabs);
          else if (xabs>0.)      // y=0.
            dlon=90.;
          else 
            dlon=-90.;
          rlon[i][j]= m.rota + m.jpole*dlon;
          if(rlon[i][j]> 180) rlon[i][j]=rlon[i][j]-360.;
          if(rlon[i][j]<-180) rlon[i][j]=rlon[i][j]+360.;
          if(rlon[i][j]>180.) rlon[i][j]=180.0;
          if(rlon[i][j]<-180.) rlon[i][j]=-180.0;
        }  //end of j-loop
      }  //end of i-loop
    }  // end of else if ( m.jproj==1 )

//#######################################################################
//
//  Lambert Conformal Conic Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius/n
//    projc2 is cos of trulat(1)
//    projc3 is tan (45. - trulat/2) a const for local map scale
//    projc4 is the cone constant, n
//
//#######################################################################

    else if ( m.jproj==2 )
    {
      for(int i=0;i< idim;i++){
        for(int j=0;j< jdim;j++){
          yabs=y[j]+m.yorig;
          xabs=x[i]+m.xorig;
          radius=sqrt( xabs*xabs+ yabs*yabs );
          ratio=m.projc3*(float)pow((float)(radius/(m.projc1*m.projc2)),(float)(1./m.projc4));
          rlat[i][j]=m.jpole*(90. -2.*r2deg*(atan(ratio)));
          if(rlat[i][j]>90.)rlat[i][j]=90.0;
          if(rlat[i][j]<-90.)rlat[i][j]=-90.0;
 
          yjp=m.jpole*yabs;
          if(yjp>0.)
            dlon=180. + r2deg*atan(-xabs/yabs)/m.projc4;
          else if(yjp<0.) 
            dlon=r2deg*atan(-xabs/yabs)/m.projc4;
          else if (xabs>0.)      // y=0.
            dlon=90./m.projc4;
          else
            dlon=-90./m.projc4;
          rlon[i][j]= m.rota + m.jpole*dlon;
          if(rlon[i][j]> 180) rlon[i][j]=rlon[i][j]-360.;
          if(rlon[i][j]<-180) rlon[i][j]=rlon[i][j]+360.;
          if(rlon[i][j]>180.) rlon[i][j]=180.0;
          if(rlon[i][j]<-180.) rlon[i][j]=-180.0;
        }  // end of j-loop
      }  // end of i-loop
    }  // end of else if( m.jproj==2)

//#######################################################################
//
//  Mercator Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2
//
//#######################################################################

    else if( m.jproj==3 )
    {
      for(int i=0;i< idim;i++){
        for(int j=0;j< jdim;j++){
          yabs=y[j]+m.yorig;
          xabs=x[i]+m.xorig;
          rlat[i][j]=(90. - 2.*r2deg*atan(exp(-yabs/m.projc3)));
          if(rlat[i][j]>90.)rlat[i][j]=90.0;
          if(rlat[i][j]<-90.)rlat[i][j]=-90.0;
          dlon=r2deg*(xabs/m.projc3);
          rlon[i][j]=m.rota + dlon;
          if(rlon[i][j]> 180) rlon[i][j]=rlon[i][j]-360.;
          if(rlon[i][j]<-180) rlon[i][j]=rlon[i][j]+360.;
        }  // end of j-loop
      }  // end of i-loop
    }  // end of else if( m.jproj==3 )

//#######################################################################
//
//  Lat, Lon Projection.
//  For this projection:
//    projc1 is the scaled earth's radius, scale times eradius
//    projc2 is cos of trulat(1)
//    projc3 is projc1 times projc2 times 180/pi
//
//#######################################################################

    else if( m.jproj==4 )
    {
      for(int i=0;i< idim;i++){
        for(int j=0;j< jdim;j++){
          rlon[i][j]=x[i]+m.xorig;
          rlat[i][j]=y[j]+m.yorig;
        }
      }
    }
    else
    {
      std::cout<<m.jproj<<" projection is not supported"<<std::endl;
      exit(1); 
    }
};




// ##########################################################################
//
//   setorig.cc: A function to set up the origin on a specific map projection
//
// ##########################################################################
//
// ##########################################################################
//
//   Author: Jian Zhang (CIMMS/NSSL), Wenwu Xia (CIMMS/NSSL)
//   May 20, 2000
//
//   Modification History:
//   09/10/2001  Jian Zhang (CIMMS/NSSL)
//   Code cleanup.  Documentation improvement.
//
// ##########################################################################


     
//void lltoxy_func(float rrlat,float rrlon,float &xloc,float &yloc,setmapr& a);
//
//void xytoll_func(float **rlat,float **rlon,float *x,float *y,
//                  int idim, int jdim,setmapr &m);
 
void setorig::set_orig(setmapr &m)
{


//#######################################################################
//
//  iopt=1 input origin is given in x,y in absolute coordinates.
//
//#######################################################################

    if( iopt==1 )
    { 
      m.xorig=x0;
      m.yorig=y0;

      float **rlat, **rlon,*cx,*cy;
      rlat = new float *[1];
      rlon = new float *[1];
      rlat[0] = new float[1];
      rlon[0] = new float[1]; 
      cx=new float[1];
      cy=new float[1];
      cx[0]=0.0;
      cy[0]=0.0;
 
      xytoll_func(rlat,rlon,cx,cy, 1, 1,m);

      std::cout<<"Coordinate origin set to absolute x,y ="<<m.xorig
          <<"  "<<m.yorig
          <<"  Latitude, longitude="<<rlat[0][0]<<"  "<<rlon[0][0]<<std::endl;
 
      delete [] cx;
      delete [] cy;
      delete [] rlat[0];
      delete [] rlon[0];
      delete [] rlat;
      delete [] rlon; 
    }

//#######################################################################
//
//  iopt=2 input origin is given in lat,lon on earth
//
//#######################################################################

    else if( iopt==2 )
    {
      m.xorig=0.;
      m.yorig=0.;

      lltoxy_func(x0,y0,m.xorig,m.yorig,m); 

      std::cout<<"Coordinate origin set to absolute x,y ="<<m.xorig
          <<"  "<<m.yorig
          <<"  Latitude, longitude="<<x0<<"  "<<y0<<std::endl;
        
    }
 
    else 
    {

      float **rlat, **rlon,*cx,*cy;
      rlat = new float *[1];
      rlon = new float *[1];
      rlat[0] = new float[1];
      rlon[0] = new float[1];
      cx=new float[1];
      cy=new float[1];
      cx[0]=0.0;
      cy[0]=0.0;

      xytoll_func(rlat,rlon,cx,cy, 1, 1,m);

      std::cout<<" ++WARNING  Setorig option "<<iopt<<" not supported."
          <<"  Coordinate origin unchanged at x,y ="<<m.xorig
          <<"  "<<m.yorig
          <<"   Latitude, longitude= "<<rlat[0][0]<<"  "<<rlon[0][0]<<std::endl;

      delete [] cx;
      delete [] cy;
      delete [] rlat[0];
      delete [] rlon[0];
      delete [] rlat;
      delete [] rlon;

    }
};
