//////////////////////////////////////////////////////////////////////////
//    FILE:  qpesums_utility.cc						//
// PURPOSE:  IMPLEMENT UTILITY METHODS					//
//  AUTHOR:  CHRIS CALVERT						//	
//    DATE:  4/19/00							//
//////////////////////////////////////////////////////////////////////////
//
#include <cstdlib>

#include "qpesums_utility.h"

using namespace std;

///////////////////////////////////////////////////////
// FUNCTION: beamheight
// DESCRIPTION: Compute the height above the radar of
//    of a radar beam given the range and radar angle.
// INPUTS: A reference to 3 floats, two of which are
//    constant. The other reference is the value to
//    be computed.
// OUTPUTS: None
// RETURN VALUES: None
///////////////////////////////////////////////////////

void beamheight( float &height, const float &angle, const float &range )
{

  /* Make sure range is positive. */

    if( range < 0.0 )
    {
      const_cast< float & >( range ) = range * -1.0; 
    }


  /* Convert elevation angle to radians. */

    double elvrad( DEG_TO_RAD * angle );


  /* Compute height. */

    height = sqrt( ( range * range ) + ( 8494666.66 * 8494666.66 ) + ( ( 16989333.33 ) * ( range * sin( elvrad ) ) ) ) - 8494666.66;

}

///////////////////////////////////////////////////////
// FUNCTION: search_file
// DESCRIPTION: Determine if the string passed in is
//    in the file passed in.
// INPUTS: A reference to a file stream and a string.
// OUTPUTS: None
// RETURN VALUES: Integer
///////////////////////////////////////////////////////
int search_file( ifstream &inFile, const string &find_this )
{
    string current_string; // current string read from file

    //search from current location
    while( !inFile.eof() )
    {
      inFile >> current_string;
      if(current_string == find_this)
          return YES;
    }

    cout<<"You can't find the parameter "<<find_this<<"In the config file."<<endl
        <<"Or You didn't set the parameter "<<find_this
        <<" in get_config function as same order as config file."<<endl;

    return NO;
}


///////////////////////////////////////////////////////
// FUNCTION: get_file_size
// DESCRIPTION: Determine the size of the file 
//    corresponding to the ifstream reference passed in.
// INPUTS: A reference to a file stream.
// OUTPUTS: None
// RETURN VALUES: Long
///////////////////////////////////////////////////////

long get_file_size( ifstream &inFile )
{

  /* Determine current position of   */
  /* the file pointer. Then put the  */
  /* pointer at the end of the file. */
  /* This gives the file's size.     */
  /* Finally, return the pointer to  */
  /* the original position and       */
  /* the file size.                  */
 
    long current_pos = inFile.tellg();
    inFile.seekg( 0, ios::end );
    long file_size = inFile.tellg();
    inFile.seekg( current_pos );

    return file_size;

}

///////////////////////////////////////////////////////
// FUNCTION: get_file_size
// DESCRIPTION: Determine the size of the file
//    corresponding to the file name passed in.
// INPUTS: A string.
// OUTPUTS: None
// RETURN VALUES: Long
///////////////////////////////////////////////////////

long get_file_size( const string &file_name )
{

  /* Open the file corresponding to the */
  /* passed in string. If it can't be   */
  /* opened, or isn't there, bail.      */

    ifstream inFile( file_name.c_str(), ios::in );

    if( !inFile )
    {
      string error_string = "FILE: ";
      error_string.append( file_name );
      error_string.append( " ISN'T THERE" );
      bail( "qpesums_utility.cc", "get_file_size", error_string );
    }


  /* Put file pointer at the end of the */
  /* file and get the file's size.      */
  /* Return this value.                 */

    long file_size = get_file_size( inFile );
    inFile.close();

    return file_size;

}

///////////////////////////////////////////////////////
// FUNCTION: bail
// DESCRIPTION: Outputs an error message, then exits
//    the program.
// INPUTS: Three references to strings.
// OUTPUTS: Error string
// RETURN VALUES: None 
///////////////////////////////////////////////////////

void bail( const string &file, const string &method, const string &message )
{
  cout << endl << endl;
  cout << "IN FILE:   " << file << endl;
  cout << "IN METHOD: " << method << endl;
  cout << message << endl;
  cout << "EXITING" << endl;
  cout << endl << endl;
  exit( 0 );
}

///////////////////////////////////////////////////////
// FUNCTION: check_for_end_slash
// DESCRIPTION: Makes sure the passed in string ends
//    in a slash. 
// INPUTS: Reference to a string.
// OUTPUTS: None
// RETURN VALUES: None
///////////////////////////////////////////////////////

void check_for_end_slash( string &check_it )
{

  /* Make sure the passed in string  */
  /* ends in a slash. If it doesn't, */
  /* add a slash.                    */

    if( check_it.at( ( check_it.length() - 1 ) ) != '/' )
    {
      check_it.append( "/" ); // check for trailing slash
    }

}
