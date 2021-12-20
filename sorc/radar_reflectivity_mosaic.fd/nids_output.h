/* ##########################################################################
 *      nids_output.h:  A function to write float 2D array into nids file
 *
 *      Author: Chris Calvert (CIMMS/NSSL),Wenwu Xia (CIMMS/NSSL)
 *      November 10, 2000
 *
 *      Modification History:
 *
 *      Chris Calvert 9/26/01 - re-did logic, made into a template
 *
 * ##########################################################################
 */



#include <fstream>
#include <string>
#include <vector>

#include "NIDS_RASTER_HEADER.h"
#include "NIDS_INFO.h"

#include "CONFIG_PARS.h"
#include "qpesums_utility.h"

#define REF_VALUES_TAG "REFLECTIVITY_VALUES:"
#define REF_COLORS_TAG "REFLECTIVITY_COLORS:"
#define HEIGHT_VALUES_TAG "HEIGHT_VALUES:"
#define HEIGHT_COLORS_TAG "HEIGHT_COLORS:"
#define VIL_VALUES_TAG "VIL_VALUES:"
#define VIL_COLORS_TAG "VIL_COLORS:"
#define LTG_TOTAL_VALUES_TAG "LTG_TOTAL_VALUES:"
#define LTG_TOTAL_COLORS_TAG "LTG_TOTAL_COLORS:"
#define LTG_POSITIVE_VALUES_TAG "LTG_POSITIVE_VALUES:"
#define LTG_POSITIVE_COLORS_TAG "LTG_POSITIVE_COLORS:"
#define LTG_PROG_VALUES_TAG "LTG_ALERT_VALUES:"
#define LTG_PROG_COLORS_TAG "LTG_ALERT_COLORS:"
#define RADCOVER_VALUES_TAG "RADCOVER_VALUES:"
#define RADCOVER_COLORS_TAG "RADCOVER_COLORS:"
#define RADCOV_PCNT_VALUES_TAG "RADCOV_PERCENTAGE_VALUES:"
#define RADCOV_PCNT_COLORS_TAG "RADCOV_PERCENTAGE_COLORS:"
#define RAIN_RATE_VALUES_TAG "RAINRATE_VALUES:"
#define RAIN_RATE_COLORS_TAG "RAINRATE_COLORS:"
#define RAIN_FLAG_VALUES_TAG "RAINFLAG_VALUES:"
#define RAIN_FLAG_COLORS_TAG "RAINFLAG_COLORS:"
#define PHASE_VALUES_TAG "PHASE_VALUES:"
#define PHASE_COLORS_TAG "PHASE_COLORS:"
#define RAIN_HOURLY_VALUES_TAG "RAIN_HRLY_ACCUMUL_VALUES:"
#define RAIN_HOURLY_COLORS_TAG "RAIN_HRLY_ACCUMUL_COLORS:"
#define RAIN_DAILY_VALUES_TAG "RAIN_DAILY_ACCUMUL_VALUES:"
#define RAIN_DAILY_COLORS_TAG "RAIN_DAILY_ACCUMUL_COLORS:"
#define TEMP_VALUES_TAG "TEMPERATURE_VALUES:"
#define TEMP_COLORS_TAG "TEMPERATURE_COLORS:"

#define NO			0
#define YES			1
#define NUM_SECONDS_IN_DAY	86400
#define EMPTY			0
#define NIDS_SNOW_OFFSET	50


///////////////////////////////////////////////////////
// FUNCTION: nids_output
// DESCRIPTION: Templated method to output a NIDS file.
///////////////////////////////////////////////////////

template< class Data_Type >

void nids_output(
	string config_file_name,
	char *fname,
	CONFIG_PARS &pp,
	Data_Type **t_inp,
	double dlon_n,
	double dlat_n,
	int nx_n,
	int ny_n,
	short QPEcode,
        string values_tag,
        string colors_tag,
	const Data_Type rmissing,
	float r_max_scale )
{

  /* Set file name and method name */

    string file_name = "nids_output.h";
    string method_name = "nids_output";


  /* Fill NIDS_INFO variable. This will pass   */
  /* all the necessary data to various objects */
  /* and methods without having a long list of */
  /* method arguments to deal with.            */

    NIDS_INFO nids_i;
    nids_i.set_product_code( static_cast< short >( QPEcode ) );
    nids_i.set_epoch_days( ( pp.epoch_seconds / NUM_SECONDS_IN_DAY ) + 1 );
    nids_i.set_seconds_since_midnight( pp.epoch_seconds % NUM_SECONDS_IN_DAY );
    nids_i.set_center_latitude( pp.ctrlat );
    nids_i.set_center_longitude( pp.ctrlon );
    nids_i.set_num_cols( nx_n );
    nids_i.set_num_rows( ny_n );
    nids_i.set_values_tag( values_tag );
    nids_i.set_colors_tag( colors_tag );
    nids_i.set_swap_flag( pp.swap_flag );


  /* Determine if this product has snow.  */
  /* Snow data values are offset by       */
  /* NIDS_SNOW_OFFSET, so if we know that */
  /* snow values are possible, then we    */
  /* can handle them correctly later on.  */

    int product_has_snow = NO;
 
    if(
        ( QPEcode == 211 || QPEcode == 221 || QPEcode == 231 ) ||
        ( QPEcode > 212 && QPEcode < 218 ) ||
        ( QPEcode > 222 && QPEcode < 228 ) ||
        ( QPEcode > 232 && QPEcode < 238 )
      )
    {
      product_has_snow = YES;
    }


  /* Re-project the radar coverage data. Find  */
  /* the max value of the data. Call it a      */
  /* temporary max value, since it could be    */
  /* any data type. Later, we'll set this to a */
  /* short, which is what NIDS expects. We'll  */
  /* do two cases, one where snow could be     */
  /* present, and the other where it couldn't. */
  /* This causes a repeat of some code, but    */
  /* we only check the "product_has_snow"      */
  /* variable once, instead of every loop      */
  /* iteration. In other words, this way       */
  /* takes more code, but takes less CPU time. */

    vector< Data_Type > data_vec( nx_n * ny_n );
    Data_Type temp_max_value = -9999;
    int row_position = 0;
    int data_position = 0;
    int i = 0;
    int j = 0;

    if( product_has_snow )
    {
      for( int j_n = 0; j_n < ny_n; j_n++ )
      {
        row_position = j_n * nx_n;
 
        j = static_cast< int >( ( ( static_cast< double >( j_n ) * dlat_n ) / static_cast< double >( pp.dy ) ) + 0.50 );
        if(j >= pp.ny)  j = pp.ny - 1;


        for( int i_n = 0; i_n < nx_n; i_n++ )
        {
          data_position = row_position + i_n;

          i = static_cast< int >( ( ( static_cast< double >( i_n ) * dlon_n ) / static_cast< double >( pp.dx ) ) + 0.50 );
          if(i >= pp.nx) i = pp.nx - 1;


          data_vec[ data_position ] = t_inp[ i ][ j ];

          if( ( data_vec[ data_position ] >= NIDS_SNOW_OFFSET ) )
          { 
            if( ( ( data_vec[ data_position ] - NIDS_SNOW_OFFSET ) > temp_max_value ) &&
                ( ( data_vec[ data_position ] - NIDS_SNOW_OFFSET ) > rmissing ) )
            {
              temp_max_value = data_vec[ data_position ];
            }
          }
          else
          {
            if( ( data_vec[ data_position ] > temp_max_value ) && 
                ( data_vec[ data_position ] > rmissing ) )
            {
              temp_max_value = data_vec[ data_position ];
            }
          }
        }
      }
    }
    else // no snow values possible
    {
      for( int j_n = 0; j_n < ny_n; j_n++ )
      {
        row_position = j_n * nx_n;
 
        j = static_cast< int >( ( ( static_cast< double >( j_n ) * dlat_n ) / static_cast< double >( pp.dy ) ) + 0.50 );
        if(j >= pp.ny)  j = pp.ny - 1;


        for( int i_n = 0; i_n < nx_n; i_n++ )
        {
          data_position = row_position + i_n;

          i = static_cast< int >( ( ( static_cast< double >( i_n ) * dlon_n ) / static_cast< double >( pp.dx ) ) + 0.50 );
          if(i >= pp.nx) i = pp.nx - 1;


          data_vec[ data_position ] = t_inp[ i ][ j ];

          if( ( data_vec[ data_position ] > temp_max_value ) && 
              ( data_vec[ data_position ] > rmissing ) )
          {
            temp_max_value = data_vec[ data_position ];
          }
        }
      }
    }


  /* Set max value for NIDS. */
  
  short max_value;
  round( max_value, ( temp_max_value * r_max_scale ) );


  /* Create nids_header object. This object */
  /* creates and output the header to the   */
  /* NIDS file. It also reads in the        */
  /* thresholds used for this product.      */

    NIDS_RASTER_HEADER< Data_Type > nids_header( config_file_name,
                                                 nids_i );

  /* Declare vector of unsigned shorts,    */
  /* which will eventually make up the     */
  /* array to be written to file.          */

    std::vector< unsigned short > output_vec;


  /* Call run_length_encode method, which will */
  /* run-length-encode the data, and fill the  */
  /* variable output_vec.                      */

    run_length_encode( data_vec,
                       output_vec, 
                       nids_header.get_thresholds(),
                       nids_i );


  /* Fill in necessary NIDS info. This information */
  /* isn't able to be calculated until after the   */
  /* data has been run-length-encoded.             */
 
    long total_size_bytes = static_cast< long >( output_vec.size() );
    total_size_bytes *= sizeof( unsigned short );
 
    long block_length = total_size_bytes;
    block_length += NIDS_RASTER_DATA_PACKET_HEADER;
    block_length += NIDS_PRODUCT_SYMBOLOGY_LENGTH;
 
    long data_layer_bytes = total_size_bytes;
    data_layer_bytes += NIDS_RASTER_DATA_PACKET_HEADER;
 
    long message_length = NIDS_MESSAGE_BYTES;
    message_length += NIDS_PRODUCT_BYTES;
    message_length += block_length;

    nids_header.set_block_length( block_length );
    nids_header.set_layer_length( data_layer_bytes );
    nids_header.set_message_length( message_length );
    nids_header.set_max_value( max_value );

 
  /* Write data to array, since an array can */
  /* be written to file with one I/O call.   */
  /* Since this is Linux, byteswap the data. */
 
    int vec_length = output_vec.size();
 
    unsigned short *output_array = new unsigned short[ vec_length ];
 
    for( i = 0; i < vec_length; i++ )
    {
      output_array[ i ] = output_vec[ i ];
    }

    if( pp.swap_flag == YES )
    {
      byteswap( output_array, vec_length );
    }
    

 
  /* Create output file, if it can't */
  /* be created, exit.               */

    string output_filename( fname );
 
    std::ofstream outFile( output_filename.c_str(),
                           std::ios::out | std::ios::binary );
 
    if( !outFile )
    {
      std::string  error_string = "CAN'T CREATE ";
      error_string.append( output_filename );
      bail( file_name, method_name, error_string );
    }


  /* Write out NIDS header. */
 
    nids_header.output_header( outFile );


  /* Write data array to file. */
 
    outFile.write( reinterpret_cast< char * >( output_array ),
                   vec_length * sizeof( unsigned short ) );
 

  /* Close file. */
 
    outFile.close();


  /* Reclaim memory. */
 
    delete [] output_array;
 
}

///////////////////////////////////////////////////////
// FUNCTION: run_length_encode
// DESCRIPTION: Given a data array, run-length-encode
//    the data, and fill the output vector.
///////////////////////////////////////////////////////

template< class Data_Type >

void run_length_encode(  const vector< Data_Type > &data_vec,
                         vector< unsigned short > &output_vec, 
                         const vector< Data_Type > &thresholds,
                         const NIDS_INFO &nids_i )
{
 
    std::vector< unsigned char > rle_vec;
    int rle_vec_size          = 0;
    unsigned char index       = 255;
    unsigned char new_index   = 255;
    unsigned char temp_char   = 255;
    unsigned short temp_short = 255;
    unsigned char count       = 255;
    int pos                   = 0;
    int data_pos              = 0;
    int num_cols              = nids_i.get_num_cols();
    int num_rows              = nids_i.get_num_rows();
    int stop_here             = num_cols - 2;

 
  /* Loop through each row of data */
  /* and run length encode it.     */

    for( int row_num = num_rows - 1; row_num >= 0; row_num-- )
    {

      data_pos = row_num * num_cols;
      pos = 0;
      find_index( index,
                  data_vec[ data_pos + pos ],
                  thresholds );
 
      count = 1;
 
      while( pos < stop_here )
      {
        pos++;
        find_index( new_index,
                    data_vec[ data_pos + pos ],
                    thresholds );
 
        if( ( new_index == index ) &&
            ( count < 15 ) )
        {
          count++;
        }
        else
        {
          temp_char = count;
          temp_char <<= 4;
          temp_char += index;
          rle_vec.push_back( temp_char );
          count = 1;
          index = new_index;
        }
      }
 
      /* Take care of next to last one. */
 
        temp_char = count;
        temp_char <<= 4;
        temp_char += index;
        rle_vec.push_back( temp_char );
 
      /* Take care of last one. */
 
        pos++;
        count = 1;
        find_index( new_index,
                    data_vec[ data_pos + pos ],
                    thresholds );
        temp_char = count;
        temp_char <<= 4;
        temp_char += index;
        rle_vec.push_back( temp_char );

        rle_vec_size = rle_vec.size();


        /* Make sure there are even number of */
        /* bytes (run/value pairs).           */

        if( rle_vec_size % 2 )
        {
          temp_char = 0;
          rle_vec.push_back( temp_char );
          rle_vec_size++;
        }

        /* For each run-length-encoded row, push the */
        /* number of bytes for this row and the      */
        /* run/value pairs onto the output vector.   */

        output_vec.push_back( static_cast< unsigned short >( rle_vec_size ) );

        for( int i = 0; i < rle_vec_size; i += 2 )
        {
          temp_short = static_cast< unsigned short >( rle_vec[ i ] );
          temp_short <<= 8;
          temp_short += static_cast< unsigned short >( rle_vec[ i + 1 ] );
          output_vec.push_back( temp_short );
        }

        rle_vec.resize( EMPTY );
 
    } // for each row

}

///////////////////////////////////////////////////////
// FUNCTION: find_index
// DESCRIPTION: Given a data value, this sets the index
//    to the appropriate position in the thresholds array
///////////////////////////////////////////////////////

template< class Data_Type >

void find_index(  unsigned char &index,
                  const Data_Type &value,
                  const vector< Data_Type > &thresholds )
{
 
  if( value < thresholds [ 0 ] )
  {
    index = 0;
  }
  else if( value >= thresholds[ 15 ] )
  {
    index = 15;
  }
  else
  {
    for( int pos = 1; pos < 15; pos++ )
    {
      if( ( value >= thresholds[ pos ] ) && 
          ( value < thresholds[ pos + 1 ] ) )
      {
        index = pos;
      }
    }
  }

  return;

}

