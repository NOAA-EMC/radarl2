#ifndef QPESUMS_UTILITY_H
#define QPESUMS_UTILITY_H

//////////////////////////////////////////////////////////////////////////
//    FILE:  qpesums_utility.h						//
//   CLASS:  								//
// PURPOSE:  IMPLEMENT UTILTIY METHODS					//
//  AUTHOR:  CHRIS CALVERT						//
//    DATE:  4/19/00							//
//////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <fstream>
#include <string>
#include <iostream>
#include <vector>


#ifndef KM_TO_M
#define KM_TO_M		1000.0
#endif

#ifndef PI
#define PI		3.141592654
#endif

#ifndef DEG_TO_RAD
#define DEG_TO_RAD	PI / 180.0
#endif

#ifndef YES
#define YES		1
#endif

#ifndef NO
#define NO		0
#endif


template < class Return_Type, class Round_Type >

inline void round( Return_Type &num, const Round_Type &roundme )
{
 
  /* If the fractional part is less than 0.5,  */
  /* then "chop off" the fractional part, else */
  /* round the value up.                       */

    if( ( roundme - static_cast< Return_Type >( roundme ) ) < 0.5 ) 
    {
      num = static_cast< Return_Type >( roundme );
    }
    else
    {
      num = ( ( static_cast< Return_Type >( roundme ) ) + 1 );
    }

}

template < class Data_Type >

inline void byteswap( Data_Type &data )
{
  unsigned int num_bytes = sizeof( Data_Type );
  char *char_data = reinterpret_cast< char * >( &data );
  char *temp = new char[ num_bytes ];

  for( unsigned int i = 0; i < num_bytes; i++ )
  {
    temp[ i ] = char_data[ num_bytes - i - 1 ];
  }

  for( unsigned int i = 0; i < num_bytes; i++ )
  {
    char_data[ i ] = temp[ i ];
  }

  delete [] temp;
}

template < class Data_Type >

inline void byteswap( std::vector< Data_Type > &data_vec )
{
  unsigned int num_bytes = sizeof( Data_Type );
  unsigned int vec_size = data_vec.size();
  char *temp = new char[ num_bytes ];
  char *char_data;

  for( unsigned int i = 0; i < vec_size; i++ )
  {
    char_data = reinterpret_cast< char * >( &data_vec[ i ] );

    for( unsigned int i = 0; i < num_bytes; i++ )
    {
      temp[ i ] = char_data[ num_bytes - i - 1 ];
    }

    for( unsigned int i = 0; i < num_bytes; i++ )
    {
      char_data[ i ] = temp[ i ];
    }
  }

  delete [] temp;
}

template < class Data_Type >

inline void byteswap( Data_Type *data_array, int num_elements )
{
  int num_bytes = sizeof( Data_Type );
  char *temp = new char[ num_bytes ];
  char *char_data;

  for( int i = 0; i < num_elements; i++ )
  {
    char_data = reinterpret_cast< char * >( &data_array[ i ] );

    for( int i = 0; i < num_bytes; i++ )
    {
      temp[ i ] = char_data[ num_bytes - i - 1 ];
    }

    for( int i = 0; i < num_bytes; i++ )
    {
      char_data[ i ] = temp[ i ];
    }
  }

  delete [] temp;
}

void beamheight( float &, const float &, const float & );

int search_file( std::ifstream &, const std::string & );

long get_file_size( std::ifstream & );

long get_file_size( const std::string & );

void bail( const std::string &, const std::string &, const std::string & );

void check_for_end_slash( std::string & );


#endif
