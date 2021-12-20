#ifndef SRC_DATA_HEADER_H
#define SRC_DATA_HEADER_H

//////////////////////////////////////////////////////////////////////////
//     FILE:  SRC_DATA_HEADER.h  		         		//
//STRUCTURE:  SRC_DATA_HEADER						//
//  PURPOSE:  DEVELOP SRC RADAR DATA STRUCTURE FOR 3D MOSAIC    	//
//   AUTHOR:  XIAOYONG XU						//
//     DATE:  11/08/06							//
//////////////////////////////////////////////////////////////////////////

//#define NAZIM 760
//#define NGATE 1000
#define  NUM_LEVEL 31

struct SRC_DATA_HEADER
{
     int          dim_1;           //for nx
     int          dim_2;           //for ny
     int          dim_3;           //for nz
     float        dx;
     float        dy;
     float        nw_lat, nw_lon;
     float        zp[NUM_LEVEL];
     float        missing_value;

};

struct MOSAIC_DATA_HEADER

{
     int          nx;
     int          ny;
     int          nz;
     float        dx;
     float        dy;
     float        ctrl_lat, ctrl_lon;
     float        zp[NUM_LEVEL];
     float        missing_value;
     float        no_radar_cover;
    
};
#endif
