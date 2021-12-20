//////////////////////////////////////////////////////////////////////////
//    FILE:  SOUNDING.cc						//
// PURPOSE:  IMPLEMENT SOUNDING CLASS FROM SOUNDING.h FILE		//
//  AUTHOR:  CHRIS CALVERT                                              //
//    DATE:  4/19/00                                                    //
//////////////////////////////////////////////////////////////////////////

#include "SOUNDING.h"

using namespace std;

///////////////////////////////////////////////////////
// FUNCTION: SOUNDING
// DESCRIPTION: Default class SOUNDING constructor. It
//    initiates an object of the SOUNDING class. 
// INPUTS: String
// OUTPUTS: None
// RETURN VALUES: None
///////////////////////////////////////////////////////

SOUNDING::SOUNDING(){}

///////////////////////////////////////////////////////
// FUNCTION: SOUNDING 
// DESCRIPTION: Class SOUNDING constructor. It initiates 
//    an object of the SOUNDING class.  A string is passed
//    in that becomes the name of the sounding location.
//    This method "fills" the sounding variables by  
//    calling the read_sounding_file method.
// INPUTS: String
// OUTPUTS: None
// RETURN VALUES: None
///////////////////////////////////////////////////////

SOUNDING::SOUNDING( string name, string confile )
{
  sounding_location = name;
  config_file_name = confile;

  read_sounding_file();
}

///////////////////////////////////////////////////////
// FUNCTION: read_sounding_file
// DESCRIPTION: Assigns values to the zero_height and
//    minusten_height variables by reading appropriate
//    sounding files.
// INPUTS: None
// OUTPUTS: None
// RETURN VALUES: None
///////////////////////////////////////////////////////

void SOUNDING::read_sounding_file()
{

  make_sounding_file_name();

  /* Open sounding file. If it won't open, or */
  /* isn't there, bail.                       */

    ifstream soundFile( sounding_file_name.c_str(), ios::in );

    if( !soundFile )
    {
      string error_string = sounding_file_name;
      error_string.append( " FILE ISN'T THERE" );
      bail( "SOUNDING.cc", "read_sounding_file", error_string );
    }
    else
    {

      /* Search for the appropriate header tags.  */
      /* If they are in the sounding file, then   */
      /* read in the value, if they aren't, bail  */
      /* Close the file when finished.            */

        int rc = search_file( soundFile, TWENTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> twenty_height;
        }
        else
        {
          twenty_height = DEFAULT_TWENTY_HEIGHT;
        }

        rc = search_file( soundFile, TEN_HEADER );

        if( rc > 0 )
        {
          soundFile >> ten_height;
        }
        else
        {
          ten_height = DEFAULT_TEN_HEIGHT;
        }

        rc = search_file( soundFile, ZERO_HEADER );

        if( rc > 0 )
        {
          soundFile >> zero_height;
        }
        else
        {
          zero_height = DEFAULT_ZERO_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_TEN_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_ten_height;
        }
        else
        {
          minus_ten_height = DEFAULT_MINUS_TEN_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_TWENTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_twenty_height;
        }
        else
        {
          minus_twenty_height = DEFAULT_MINUS_TWENTY_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_THIRTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_thirty_height;
        }
        else
        {
          minus_thirty_height = DEFAULT_MINUS_THIRTY_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_FOURTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_fourty_height;
        }
        else
        {
          minus_fourty_height = DEFAULT_MINUS_FOURTY_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_FIFTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_fifty_height;
        }
        else
        {
          minus_fifty_height = DEFAULT_MINUS_FIFTY_HEIGHT;
        }

        rc = search_file( soundFile, MINUS_SIXTY_HEADER );

        if( rc > 0 )
        {
          soundFile >> minus_sixty_height;
        }
        else
        {
          minus_sixty_height = DEFAULT_MINUS_SIXTY_HEIGHT;
        }

        rc = search_file( soundFile, POLAR_NEW_FLAG_HEADER );

        if( rc > 0 )
        {
          soundFile >> polar_new_sounding_flag;
        }
        else
        {
          polar_new_sounding_flag = NO;
        }  

        soundFile.close();

    }

}

///////////////////////////////////////////////////////
// FUNCTION: write_sounding_file
// DESCRIPTION: Outputs sounding information to a file.
///////////////////////////////////////////////////////

void SOUNDING::write_sounding_file()
{

  make_sounding_file_name();

  /* Open sounding file and write out values */

    ofstream soundFile( sounding_file_name.c_str(), ios::out );

    if( !soundFile )
    {
      string error_string = sounding_file_name;
      error_string.append( " CAN'T BE CREATED" );
      bail( "SOUNDING.cc", "write_sounding_file", error_string );
    }
    else
    {
      soundFile << TWENTY_HEADER << " " << twenty_height << endl;
      soundFile << TEN_HEADER << " " << ten_height << endl;
      soundFile << ZERO_HEADER << " " << zero_height << endl;
      soundFile << MINUS_TEN_HEADER << " " << minus_ten_height << endl;
      soundFile << MINUS_TWENTY_HEADER << " " << minus_twenty_height << endl;
      soundFile << MINUS_THIRTY_HEADER << " " << minus_thirty_height << endl;
      soundFile << MINUS_FOURTY_HEADER << " " << minus_fourty_height << endl;
      soundFile << MINUS_FIFTY_HEADER << " " << minus_fifty_height << endl;
      soundFile << MINUS_SIXTY_HEADER << " " << minus_sixty_height << endl;
      soundFile << POLAR_NEW_FLAG_HEADER << " " << polar_new_sounding_flag << endl;
      soundFile.close();
    }

}

///////////////////////////////////////////////////////
// FUNCTION: make_sounding_file_name
// DESCRIPTION: Reads the config file and creates the
//    name of the sounding file to read.
///////////////////////////////////////////////////////

void SOUNDING::make_sounding_file_name()
{
  /* If the sounding location or   */
  /* config_file_name variables    */
  /* aren't set, we won't get far, */
  /* so bail.                      */

    if( sounding_location.size() == 0 )
    {
      bail( "SOUNDING.cc", "make_sounding_file", "SOUNDING LOCATION NOT SET" );
    }

    if( config_file_name.size() == 0 )
    {
      bail( "SOUNDING.cc", "make_sounding_file", "CONFIG FILE NAME NOT SET" );
    }


  /* Open config file, if it won't open, or */
  /* isn't there, bail.                     */

    ifstream inFile( config_file_name.c_str(), ios::in );

    if( !inFile )
    {
      string error_string = config_file_name;
      error_string.append( " FILE ISN'T THERE" );
      bail( "SOUNDING.cc", "make_sounding_file", error_string );
    }


  /* Search config file for tag that has  */
  /* the sounding directory value. If the */
  /* tag isn't in the config file, bail.  */

    int rc = search_file( inFile, SOUNDING_DIR_HEADER );

    if( rc > 0 )
    {
      inFile >> sounding_file_name;
      inFile.close();
    }
    else
    {
      bail( "SOUNDING.cc", "make_sounding_file", "PROBLEM READING QPESUMS_SOUNDING_DIRECTORY: FROM CONFIG FILE" );
    }


  /* Make the sounding file name by making sure   */
  /* the sounding directory has a trailing slash, */
  /* append the sounding location name, then      */
  /* append the sounding file file tag.           */

    check_for_end_slash( sounding_file_name );
    sounding_file_name.append( sounding_location );
    sounding_file_name.append( SOUNDING_FILE_TAG );

}


///////////////////////////////////////////////////////
// FUNCTION: Set_sounding_location
// DESCRIPTION: Sets value of the sounding_location
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_sounding_location( string new_name ) 
{
  sounding_location = new_name;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_config_file_name
// DESCRIPTION: Sets value of the config_file_name
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_config_file_name( string new_name ) 
{
  config_file_name = new_name;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_sounding_file_name
// DESCRIPTION: Sets value of the sounding_file_name
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_sounding_file_name( string new_name )
{
  sounding_file_name = new_name;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_twenty_height
// DESCRIPTION: Sets value of the twenty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_twenty_height( float new_height )
{
  twenty_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_ten_height
// DESCRIPTION: Sets value of the ten_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_ten_height( float new_height )
{
  ten_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_zero_height
// DESCRIPTION: Sets value of the zero_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_zero_height( float new_height )
{
  zero_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_ten_height
// DESCRIPTION: Sets value of the minus_ten_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_ten_height( float new_height )
{
  minus_ten_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_twenty_height
// DESCRIPTION: Sets value of the minus_twenty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_twenty_height( float new_height )
{
  minus_twenty_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_thirty_height
// DESCRIPTION: Sets value of the minus_thirty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_thirty_height( float new_height )
{
  minus_thirty_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_fourty_height
// DESCRIPTION: Sets value of the minus_fourty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_fourty_height( float new_height )
{
  minus_fourty_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_fifty_height
// DESCRIPTION: Sets value of the minus_fifty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_fifty_height( float new_height )
{
  minus_fifty_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_minus_sixty_height
// DESCRIPTION: Sets value of the minus_sixty_height
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_minus_sixty_height( float new_height )
{
  minus_ten_height = new_height;
}

///////////////////////////////////////////////////////
// FUNCTION: Set_polar_new_sounding_flag
// DESCRIPTION: Sets value of the polar_new_sounding_flag
//    variable to the passed in value.
///////////////////////////////////////////////////////

void SOUNDING::set_polar_new_sounding_flag( int new_flag )
{
  polar_new_sounding_flag = new_flag;
}

///////////////////////////////////////////////////////
// FUNCTION: get_sounding_location
// DESCRIPTION: Returns the value of the sounding_location
//    variable.
///////////////////////////////////////////////////////

string SOUNDING::get_sounding_location() const 
{
  return sounding_location;
}

///////////////////////////////////////////////////////
// FUNCTION: get_config_file_name
// DESCRIPTION: Returns the value of the config_file_name
//    variable.
///////////////////////////////////////////////////////

string SOUNDING::get_config_file_name() const
{
  return config_file_name;
}

///////////////////////////////////////////////////////
// FUNCTION: get_sounding_file_name
// DESCRIPTION: Returns the value of the sounding_file_name
//    variable.
///////////////////////////////////////////////////////

string SOUNDING::get_sounding_file_name() const
{
  return sounding_file_name;
}

///////////////////////////////////////////////////////
// FUNCTION: get_twenty_height
// DESCRIPTION: Returns the value of the twenty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_twenty_height() const
{
  return twenty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_ten_height
// DESCRIPTION: Returns the value of the ten_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_ten_height() const
{
  return ten_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_zero_height
// DESCRIPTION: Returns the value of the zero_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_zero_height() const
{
  return zero_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_ten_height
// DESCRIPTION: Returns the value of the minus_ten_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_ten_height() const
{
  return minus_ten_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_twenty_height
// DESCRIPTION: Returns the value of the minus_twenty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_twenty_height() const
{
  return minus_twenty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_thirty_height
// DESCRIPTION: Returns the value of the minus_thirty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_thirty_height() const
{
  return minus_thirty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_fourty_height
// DESCRIPTION: Returns the value of the minus_fourty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_fourty_height() const
{
  return minus_fourty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_fifty_height
// DESCRIPTION: Returns the value of the minus_fifty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_fifty_height() const
{
  return minus_fifty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_minus_sixty_height
// DESCRIPTION: Returns the value of the minus_sixty_height
//    variable.
///////////////////////////////////////////////////////

float SOUNDING::get_minus_sixty_height() const
{
  return minus_sixty_height;
}

///////////////////////////////////////////////////////
// FUNCTION: get_polar_new_sounding_flag
// DESCRIPTION: Returns the value of the polar_new_sounding_flag
//    variable.
///////////////////////////////////////////////////////

int SOUNDING::get_polar_new_sounding_flag() const 
{
  return polar_new_sounding_flag;
}
