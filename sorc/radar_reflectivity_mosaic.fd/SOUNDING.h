#ifndef SOUNDING_H
#define SOUNDING_H

//////////////////////////////////////////////////////////////////////////
//    FILE:  SOUNDING.h							//
//   CLASS:  SOUNDING							//
// PURPOSE:  REPRESENT SOUNDING INFORMATION FOR A RADAR			//
//  AUTHOR:  CHRIS CALVERT                                              //
//    DATE:  4/19/00                                                    //
//////////////////////////////////////////////////////////////////////////


#include <fstream>
#include <string>
#include<iostream>
#include<stdlib.h>

#include "qpesums_utility.h"


#define YES			1 
#define NO			0
#define SOUNDING_DIR_HEADER	"SOUNDING_DIR:"
#define POLAR_NEW_FLAG_HEADER	"POLAR_NEW_FLAG:"
#define SOUNDING_FILE_TAG	".env_sounding.latest"
#define TWENTY_HEADER		"TWENTY_HEIGHT:"
#define TEN_HEADER		"TEN_HEIGHT:"
#define ZERO_HEADER		"ZERO_HEIGHT:"
#define MINUS_TEN_HEADER	"MINUS_TEN_HEIGHT:"
#define MINUS_TWENTY_HEADER	"MINUS_TWENTY_HEIGHT:"
#define MINUS_THIRTY_HEADER	"MINUS_THIRTY_HEIGHT:"
#define MINUS_FOURTY_HEADER	"MINUS_FOURTY_HEIGHT:"
#define MINUS_FIFTY_HEADER	"MINUS_FIFTY_HEIGHT:"
#define MINUS_SIXTY_HEADER	"MINUS_SIXTY_HEIGHT:"
#define DEFAULT_TWENTY_HEIGHT 0.0
#define DEFAULT_TEN_HEIGHT 750.0
#define DEFAULT_ZERO_HEIGHT 2300.0
#define DEFAULT_MINUS_TEN_HEIGHT 3850.0
#define DEFAULT_MINUS_TWENTY_HEIGHT 5400.0
#define DEFAULT_MINUS_THIRTY_HEIGHT 6950.0
#define DEFAULT_MINUS_FOURTY_HEIGHT 8450.0
#define DEFAULT_MINUS_FIFTY_HEIGHT 10000.0
#define DEFAULT_MINUS_SIXTY_HEIGHT 11000.0

class SOUNDING 
{


  private:


    //VARIABLES


      std::string   sounding_location;       // name of sounding location
      std::string   config_file_name;        // name of config file
      std::string   sounding_file_name;      // name of sounding file
      float         twenty_height;           // height of 20C isotherm
      float         ten_height;              // height of 10C isotherm
      float         zero_height;             // height of 0C isotherm
      float         minus_ten_height;        // height of -10C isotherm
      float         minus_twenty_height;     // height of -20C isotherm
      float         minus_thirty_height;     // height of -30C isotherm
      float         minus_fourty_height;     // height of -40C isotherm
      float         minus_fifty_height;      // height of -50C isotherm
      float         minus_sixty_height;      // height of -60C isotherm
      int           polar_new_sounding_flag; // is this a new sounding?


    //METHODS

    
      //non-accessory methods


        void make_sounding_file_name();


  public:


    //METHODS

 
      //constructors


        SOUNDING();
        SOUNDING( std::string, std::string );


      //non-accessory methods


        void    read_sounding_file();
        void    write_sounding_file();


      //set methods


        void    set_sounding_location( std::string );
        void    set_config_file_name( std::string );
        void    set_sounding_file_name( std::string );
        void    set_twenty_height( float );
        void    set_ten_height( float );
        void    set_zero_height( float );
        void    set_minus_ten_height( float );
        void    set_minus_twenty_height( float );
        void    set_minus_thirty_height( float );
        void    set_minus_fourty_height( float );
        void    set_minus_fifty_height( float );
        void    set_minus_sixty_height( float );
        void    set_polar_new_sounding_flag( int );


      //get methods      


        std::string  get_sounding_location() const;
        std::string  get_config_file_name() const;
        std::string  get_sounding_file_name() const;
        float        get_twenty_height()       const;
        float        get_ten_height()       const;
        float        get_zero_height()       const;
        float        get_minus_ten_height()   const;
        float        get_minus_twenty_height()   const;
        float        get_minus_thirty_height()   const;
        float        get_minus_fourty_height()   const;
        float        get_minus_fifty_height()   const;
        float        get_minus_sixty_height()   const;
        int          get_polar_new_sounding_flag()   const;

};


#endif
