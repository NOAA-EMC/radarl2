#ifndef NIDS_RASTER_HEADER_H     
#define NIDS_RASTER_HEADER_H     

//////////////////////////////////////////////////////////////////////////
//    FILE:  NIDS_RASTER_HEADER.h                                       //
//   CLASS:  NIDS_RASTER_HEADER                                         //
// PURPOSE:  REPRESENT HEADER INFORMATION FOR RASTER NIDS FILES.        //
//  AUTHOR:  CHRIS CALVERT                                              //
//    DATE:  09/25/01                                                   //
//////////////////////////////////////////////////////////////////////////


#include<string> 
#include<iostream>
#include<fstream>
#include<vector>
#include<time.h>


#include "NIDS_INFO.h"
#include "qpesums_utility.h"


#define NIDS_INFO_TAG                   "INPUT_NIDS_INFO:"
#define NIDS_COLORS_FILE_NAME           "NIDS.colors"
#define NIDS_FILE_NAME			"NIDS_RASTER_HEADER.h"
#define NIDS_NUM_LEVELS			16
#define NIDS_NUMBER_HEADER_ELEMENTS	79
#define NIDS_MESSAGE_BYTES              18
#define NIDS_PRODUCT_BYTES              102
#define NIDS_RASTER_DATA_PACKET_HEADER  22
#define NIDS_PRODUCT_SYMBOLOGY_LENGTH   16
#define NIDS_DEFAULT_MESSAGE_LENGTH	0
#define NIDS_DEFAULT_BLOCK_LENGTH	0
#define NIDS_DEFAULT_LAYER_LENGTH	0
#define NIDS_DEFAULT_MAX_VALUE		0
#define NIDS_RASTER_SOURCE_ID		20001
#define NIDS_BLOCK_DIVIDER		-1
#define NIDS_NOT_USED			0
#define NIDS_NUMBER_OF_BLOCKS		3
#define NIDS_LAT_LON_SCALE		1000
#define NIDS_OPERATIONAL_MODE		2
#define NIDS_VCP			0
#define NIDS_OFFSET_SYMBOLOGY		60
#define NIDS_OFFSET_GRAPHIC		0
#define NIDS_OFFSET_TABULAR		0
#define NIDS_BLOCK_ID 			1
#define NIDS_NUMBER_OF_LAYERS		1
#define NIDS_LAYER_DIVIDER		-1
#define NIDS_PACKET_HEADER		0xBA07
#define NIDS_HEADER_CODE1		0x8000
#define NIDS_HEADER_CODE2		0x00C0
#define NIDS_X_SCALE_INTEGER		1
#define NIDS_X_SCALE_FRACTION		0
#define NIDS_Y_SCALE_INTEGER		1
#define NIDS_Y_SCALE_FRACTION		0
#define NIDS_PACKING_DESCRIPTOR		2
#define S_C_S				static_cast< short >


template< class Data_Type >

class NIDS_RASTER_HEADER 
{

  private:

    //VARIABLES

    /*                                             */
    /* message_code - NIDS code for some product.  */
    /* message_date - date of data for NIDS file.  */
    /* message_time - time of data for NIDS file.  */
    /* message_length - number of bytes in this    */
    /*           NIDS file.                        */
    /* center_latitude - center latitude of domain.*/
    /* center_longitude - center longitude of      */
    /*           domain.                           */
    /* num_cols - number of data columns.          */
    /* num_rows - number of data rows.             */
    /* thresholds - vector of values used to find  */
    /*           the run-length-encode index a     */
    /*           particular data value falls into. */
    /* data_levels - vector of values corresponding*/
    /*           to a data level color.            */
    /* block_length - number of bytes from the     */
    /*           start of the block.               */
    /* layer_length - number of bytes containing a */
    /*           data packet.                      */
    /* max_data_value - max data value of a        */
    /*           product.                          */
    /*                                             */

       short          message_code;
       short          message_date;
       long           message_time;
       long           message_length;
       long           center_latitude;
       long           center_longitude;
       int            num_cols;
       int            num_rows;
       std::vector< Data_Type > thresholds;
       std::vector< unsigned short > data_levels;
       long           block_length;
       long           layer_length;
       short          max_value;
       int            swap_flag;


    //METHODS


       void  find_data_levels( std::string, 
                               std::string );
       void  find_thresholds( std::string,
                              std::string );

  public:

    //METHODS

     //constructor

       NIDS_RASTER_HEADER( std::string,
                           const NIDS_INFO & );

     //output method

       void output_header( std::ostream & );

     //set methods

       void set_message_length( long );
       void set_block_length( long );
       void set_layer_length( long );
       void set_max_value( float );

     //get methods

       const std::vector< Data_Type > & get_thresholds() const;
    
};
      
///////////////////////////////////////////////////////
// FUNCTION: NIDS_RASTER_HEADER 
// DESCRIPTION: Class NIDS_RASTER_HEADER constructor. It
//    initiates an object of the NIDS_RASTER_HEADER class.
///////////////////////////////////////////////////////

template< class Data_Type >

NIDS_RASTER_HEADER< Data_Type >::NIDS_RASTER_HEADER(
                                        string confile,
                                        const NIDS_INFO &nids_info )
{
  message_date = nids_info.get_epoch_days();
  message_time = nids_info.get_seconds_since_midnight();
  round( center_latitude,
         nids_info.get_center_latitude() * NIDS_LAT_LON_SCALE );
  round( center_longitude,
         nids_info.get_center_longitude() * NIDS_LAT_LON_SCALE );
  message_code = nids_info.get_product_code();
  num_cols = nids_info.get_num_cols();
  num_rows = nids_info.get_num_rows();
  swap_flag = nids_info.get_swap_flag();
  message_length = NIDS_DEFAULT_MESSAGE_LENGTH;
  block_length = NIDS_DEFAULT_BLOCK_LENGTH;
  layer_length = NIDS_DEFAULT_LAYER_LENGTH;
  max_value = NIDS_DEFAULT_MAX_VALUE;

  find_data_levels( confile, nids_info.get_colors_tag() );
  find_thresholds( confile, nids_info.get_values_tag() );
}

///////////////////////////////////////////////////////
// FUNCTION: output_header
// DESCRIPTION: Output header to a NIDS formatted file.
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::output_header( ostream &outFile )
{

  /* Use this temporary variable  */
  /* when printing actual header. */
 
    short *output_array = new short[ NIDS_NUMBER_HEADER_ELEMENTS ];
 
    output_array[ 0 ] = message_code;
    output_array[ 1 ] = message_date;
    output_array[ 2 ] = S_C_S( message_time>>16 );
    output_array[ 3 ] = S_C_S( message_time );
    output_array[ 4 ] = S_C_S( message_length>>16 );
    output_array[ 5 ] = S_C_S( message_length );
    output_array[ 6 ] = NIDS_RASTER_SOURCE_ID;
    output_array[ 7 ] = NIDS_NOT_USED;
    output_array[ 8 ] = NIDS_NUMBER_OF_BLOCKS;
    output_array[ 9 ] = NIDS_BLOCK_DIVIDER;
    output_array[ 10 ] = S_C_S( center_latitude>>16 );
    output_array[ 11 ] = S_C_S( center_latitude );
    output_array[ 12 ] = S_C_S( center_longitude>>16 );
    output_array[ 13 ] = S_C_S( center_longitude );
    output_array[ 14 ] = NIDS_NOT_USED;
    output_array[ 15 ] = message_code;
    output_array[ 16 ] = NIDS_OPERATIONAL_MODE;
    output_array[ 17 ] = NIDS_VCP;
    output_array[ 18 ] = NIDS_NOT_USED;
    output_array[ 19 ] = NIDS_NOT_USED;
    output_array[ 20 ] = message_date;
    output_array[ 21 ] = S_C_S( message_time>>16 );
    output_array[ 22 ] = S_C_S( message_time );
    output_array[ 23 ] = message_date;
    output_array[ 24 ] = S_C_S( message_time>>16 );
    output_array[ 25 ] = S_C_S( message_time );
    output_array[ 26 ] = NIDS_NOT_USED;
    output_array[ 27 ] = NIDS_NOT_USED;
    output_array[ 28 ] = NIDS_NOT_USED;
    output_array[ 29 ] = NIDS_NOT_USED;
    for(unsigned int i = 0; i < NIDS_NUM_LEVELS; i++)
    {
      output_array[ i + 30 ] = data_levels[ i ];
    }
    output_array[ 46 ] = max_value;
    output_array[ 47 ] = NIDS_NOT_USED;
    output_array[ 48 ] = NIDS_NOT_USED;
    output_array[ 49 ] = NIDS_NOT_USED;
    output_array[ 50 ] = NIDS_NOT_USED;
    output_array[ 51 ] = NIDS_NOT_USED;
    output_array[ 52 ] = num_cols;
    output_array[ 53 ] = NIDS_NOT_USED;
    output_array[ 54 ] = NIDS_NOT_USED;
    output_array[ 55 ] = NIDS_OFFSET_SYMBOLOGY;
    output_array[ 56 ] = NIDS_NOT_USED;
    output_array[ 57 ] = NIDS_OFFSET_GRAPHIC;
    output_array[ 58 ] = NIDS_NOT_USED;
    output_array[ 59 ] = NIDS_OFFSET_TABULAR;
    output_array[ 60 ] = NIDS_BLOCK_DIVIDER;
    output_array[ 61 ] = NIDS_BLOCK_ID;
    output_array[ 62 ] = S_C_S( block_length>>16 );
    output_array[ 63 ] = S_C_S( block_length );
    output_array[ 64 ] = NIDS_NUMBER_OF_LAYERS;
    output_array[ 65 ] = NIDS_LAYER_DIVIDER;
    output_array[ 66 ] = S_C_S( layer_length>>16 );
    output_array[ 67 ] = S_C_S( layer_length );
    output_array[ 68 ] = NIDS_PACKET_HEADER;
    output_array[ 69 ] = NIDS_HEADER_CODE1;
    output_array[ 70 ] = NIDS_HEADER_CODE2;
    output_array[ 71 ] = NIDS_NOT_USED;
    output_array[ 72 ] = NIDS_NOT_USED;
    output_array[ 73 ] = NIDS_X_SCALE_INTEGER;
    output_array[ 74 ] = NIDS_X_SCALE_FRACTION;
    output_array[ 75 ] = NIDS_Y_SCALE_INTEGER;
    output_array[ 76 ] = NIDS_Y_SCALE_FRACTION;
    output_array[ 77 ] = num_rows;
    output_array[ 78 ] = NIDS_PACKING_DESCRIPTOR;


  /* Since this is Linux, byteswap */

    if( swap_flag == YES )
    {
      byteswap( output_array, NIDS_NUMBER_HEADER_ELEMENTS );
    }


  /* Actually write out header. */

    outFile.write( reinterpret_cast< char * >( output_array ),
                   NIDS_NUMBER_HEADER_ELEMENTS * sizeof( short ) );


  /* Reclaim memory */

    delete [] output_array;

}

///////////////////////////////////////////////////////
// FUNCTION: find_data_levels
// DESCRIPTION: Reads in data_levels from file.
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::find_data_levels(
                                         string config_file_name,
                                         string colors_tag )
{

  string method_name =  "find_data_levels";


  /*                                             */
  /* Declare/initialize variables used in this   */
  /* method.                                     */
  /* inFile - file pointer to config file.       */
  /* nids_data -  directory containing NIDS info */
  /*           files                             */
  /* nidsFile - file pointer to NIDS colors file.*/
  /*                                             */


  /* Make sure config file name */
  /* is set, or exit.           */

    if( config_file_name.size() == 0 )
    {
      string error_string = "CONFIG FILE NAME NOT DEFINED";
      bail( NIDS_FILE_NAME, method_name, error_string );
    }


  /* Open config file. If it can't be */
  /* opened, or isn't there, exit.    */

    ifstream inFile( config_file_name.c_str(),
                     ios::in );

    if( !inFile )
    {
      string error_string = config_file_name;
      error_string.append( " FILE ISN'T THERE" );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }


  /* Find tag in config file for NIDS  */
  /* information. If the tag is there, */
  /* read in the value, and create the */
  /* NIDS colors file name. If the tag */
  /* is missing, exit.                 */

    string nids_data;

    if( search_file( inFile, NIDS_INFO_TAG ) )
    {
      inFile >> nids_data;
      inFile.close();
    }
    else
    {
      string error_string = "PROBLEM READING ";
      error_string.append( NIDS_INFO_TAG );
      error_string.append( " FROM CONFIG FILE " );
      error_string.append( config_file_name );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

    check_for_end_slash( nids_data );

    nids_data.append( NIDS_COLORS_FILE_NAME );


  /* Open the file, find the appropriate */
  /* colors tag, and read in the values. */
  /* If the tag isn't there, exit.       */

    ifstream nidsFile( nids_data.c_str(),
                       ios::in );

    if( !nidsFile )
    {
      string error_string = nids_data;
      error_string.append( " FILE ISN'T THERE" );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

    if( search_file( nidsFile, colors_tag ) )
    {
      data_levels.clear();
      data_levels.resize( NIDS_NUM_LEVELS );

      for( int i = 0; i < NIDS_NUM_LEVELS; i++ )
      {
        nidsFile >> data_levels[ i ];
      }

      nidsFile.close();
    }
    else
    {
      string error_string = "PROBLEMS READING ";
      error_string.append( colors_tag );
      error_string.append( " FROM: " );
      error_string.append( nids_data );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }   

}

///////////////////////////////////////////////////////
// FUNCTION: find_thresholds
// DESCRIPTION: Reads in find_thresholds from file. 
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::find_thresholds(
                                       string config_file_name,
                                       string values_tag )
{

  string method_name = "find_thresholds";


  /*                                             */
  /* Declare/initialize variables used in this   */
  /* method.                                     */
  /* inFile - file pointer to config file.       */
  /* nids_data -  directory containing NIDS info */
  /*           files                             */
  /* nidsFile - file pointer to NIDS ref file.   */
  /*                                             */


  /* Make sure config file name */
  /* is set, or exit.           */

    if( config_file_name.size() == 0 )
    {
      string error_string = "CONFIG FILE NAME NOT DEFINED";
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

  /* Open config file. If it can't be */
  /* opened, or isn't there, exit.    */

  ifstream inFile( config_file_name.c_str(),
                   ios::in );

  if( !inFile )
  {
    string error_string = config_file_name;
    error_string.append( " FILE ISN'T THERE" );
    bail( NIDS_FILE_NAME, method_name, error_string );
  }


  /* Find tag in config file for NIDS  */
  /* information. If the tag is there, */
  /* read in the value, and create the */
  /* NIDS colors file name. If the tag */
  /* is missing, exit.                 */

    string nids_data;

    if( search_file( inFile, NIDS_INFO_TAG ) )
    {
      inFile >> nids_data;
      inFile.close();
    }
    else
    {
      string error_string = "PROBLEM READING ";
      error_string.append( NIDS_INFO_TAG );
      error_string.append( " IN CONFIG FILE " );
      error_string.append( config_file_name );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

    check_for_end_slash( nids_data );

    nids_data.append( NIDS_COLORS_FILE_NAME );


  /* Open the file, find the appropriate */
  /* colors tag, and read in the values. */
  /* If the tag isn't there, exit.       */

    ifstream nidsFile( nids_data.c_str(),
                     ios::in );

    if( !nidsFile )
    {
      string error_string = nids_data;
      error_string.append( " FILE ISN'T THERE" );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

    if( search_file( nidsFile, values_tag ) ) 
    {
      thresholds.clear();
      thresholds.resize( NIDS_NUM_LEVELS );

      for( int i = 0; i < NIDS_NUM_LEVELS; i++ )
      {
        nidsFile >> thresholds[ i ];
      }

      nidsFile.close();
    }
    else
    {
      string error_string = "PROBLEMS READING ";
      error_string.append( values_tag );
      error_string.append( " FROM: " );
      error_string.append( nids_data );
      bail( NIDS_FILE_NAME, method_name, error_string );
    }

}

///////////////////////////////////////////////////////
// FUNCTION: set_message_length
// DESCRIPTION: Sets message_length variable to value
//    passed in.
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::set_message_length( long new_message_length )
{
  message_length = new_message_length;
}

///////////////////////////////////////////////////////
// FUNCTION: set_block_length
// DESCRIPTION: Sets block_length variable to value
//    passed in.
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::set_block_length( long new_block_length )
{
  block_length = new_block_length;
}

///////////////////////////////////////////////////////
// FUNCTION: set_layer_length
// DESCRIPTION: Sets layer_length variable to value
//    passed in.
///////////////////////////////////////////////////////

template< class Data_Type >

void NIDS_RASTER_HEADER< Data_Type >::set_layer_length( long new_layer_length )
{
  layer_length = new_layer_length;
}

///////////////////////////////////////////////////////
// FUNCTION: set_max_value
// DESCRIPTION: Sets max_value variable to value
//    passed in.
///////////////////////////////////////////////////////

template< class Data_Type >
 
void NIDS_RASTER_HEADER< Data_Type >::set_max_value( float data_value )
{
  round( max_value, data_value );
}

///////////////////////////////////////////////////////
// FUNCTION: get_thresholds
// DESCRIPTION:  Returns vector of thresholds. 
///////////////////////////////////////////////////////

template< class Data_Type >

const std::vector< Data_Type > & NIDS_RASTER_HEADER< Data_Type >::get_thresholds() const
{
  return thresholds;
}

#endif  
