//////////////////////////////////////////////////////////////////////////
//    FILE:  NIDS_INFO.cc						//
// PURPOSE:  IMPLEMENT NIDS_INFO CLASS FROM NIDS_INFO.h			//
//  AUTHOR:  CHRIS CALVERT                                              //
//    DATE:  9/25/01                                                    //
//////////////////////////////////////////////////////////////////////////

#include "NIDS_INFO.h"

using namespace std;

///////////////////////////////////////////////////////
// FUNCTION: NIDS_INFO 
// DESCRIPTION: Class NIDS_INFO constructor.  It initiates an
//    object of the NIDS_INFO class.
///////////////////////////////////////////////////////

NIDS_INFO::NIDS_INFO()
{
  product_code = NIDS_INFO_DEFAULT_PRODUCT_CODE;
  epoch_days = NIDS_INFO_DEFAULT_EPOCH_DAYS;
  seconds_since_midnight = NIDS_INFO_DEFAULT_SECONDS_SINCE_MIDNIGHT;
  center_latitude = NIDS_INFO_DEFAULT_CENTER_LATITUDE;
  center_longitude = NIDS_INFO_DEFAULT_CENTER_LONGITUDE;
  num_cols = NIDS_INFO_DEFAULT_NUM_COLS;
  num_rows = NIDS_INFO_DEFAULT_NUM_ROWS;
  values_tag = NIDS_INFO_DEFAULT_VALUES_TAG;
  colors_tag = NIDS_INFO_DEFAULT_COLORS_TAG;
}


///////////////////////////////////////////////////////
// FUNCTION: set_product
// DESCRIPTION: Sets variable product_code to the
//    passed in value.
///////////////////////////////////////////////////////

void NIDS_INFO::set_product_code( short new_product_code )
{
  product_code = new_product_code;
}

///////////////////////////////////////////////////////
// FUNCTION: set_epoch_days
// DESCRIPTION: Sets variable epoch_days to the passed
//    in value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_epoch_days( short new_epoch_days )
{
  epoch_days = new_epoch_days;
}

///////////////////////////////////////////////////////
// FUNCTION: set_seconds_since_midnight
// DESCRIPTION: Sets variable seconds_since_midnight
//    to the passed in value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_seconds_since_midnight( long new_value )
{
  seconds_since_midnight = new_value;
}

///////////////////////////////////////////////////////
// FUNCTION: set_center_latitude
// DESCRIPTION: Sets variable center_latitude to the
//    passed in value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_center_latitude( float new_latitude )
{
  center_latitude = new_latitude;
}

///////////////////////////////////////////////////////
// FUNCTION: set_center_longitude
// DESCRIPTION: Sets variable center_longitude to the
//    passed in value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_center_longitude( float new_longitude )
{
  center_longitude = new_longitude;
}

///////////////////////////////////////////////////////
// FUNCTION: set_num_cols
// DESCRIPTION: Sets variable num_cols to the passed in
//    value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_num_cols( int new_num_cols )
{
  num_cols = new_num_cols;
}

///////////////////////////////////////////////////////
// FUNCTION: set_num_rows
// DESCRIPTION: Sets variable num_rows to the passed in
//    value.
///////////////////////////////////////////////////////
 
void NIDS_INFO::set_num_rows( int new_num_rows )
{
  num_rows = new_num_rows;
}

///////////////////////////////////////////////////////
// FUNCTION: set_values_tag
// DESCRIPTION: Sets variable values_tag to the passed
//    in value.
///////////////////////////////////////////////////////

void NIDS_INFO::set_values_tag( string new_values_tag )
{
  values_tag = new_values_tag;
}

///////////////////////////////////////////////////////
// FUNCTION: set_colors_tag
// DESCRIPTION: Sets variable colors_tag to the passed
//    in value.
///////////////////////////////////////////////////////

void NIDS_INFO::set_colors_tag( string new_colors_tag )
{
  colors_tag = new_colors_tag;
}

///////////////////////////////////////////////////////
// FUNCTION: set_swap_flag
// DESCRIPTION: Sets variable swap_flag to the passed
//    in value.
///////////////////////////////////////////////////////

void NIDS_INFO::set_swap_flag( int new_swap_flag )
{
  swap_flag = new_swap_flag;
}

///////////////////////////////////////////////////////
// FUNCTION: get_product_code
// DESCRIPTION: Returns value of product_code variable
///////////////////////////////////////////////////////

short NIDS_INFO::get_product_code() const
{
  return product_code;
}

///////////////////////////////////////////////////////
// FUNCTION: get_epoch_days
// DESCRIPTION: Returns value of epoch_days variable
///////////////////////////////////////////////////////

short NIDS_INFO::get_epoch_days() const
{
  return epoch_days;
}

///////////////////////////////////////////////////////
// FUNCTION: get_seconds_since_midnight
// DESCRIPTION: Returns value of seconds_since_midnight
//      variable
///////////////////////////////////////////////////////

long NIDS_INFO::get_seconds_since_midnight() const
{
  return seconds_since_midnight;
}

///////////////////////////////////////////////////////
// FUNCTION: get_center_latitude
// DESCRIPTION: Returns value of center_latitude variable
///////////////////////////////////////////////////////

float NIDS_INFO::get_center_latitude() const
{
  return center_latitude;
}

///////////////////////////////////////////////////////
// FUNCTION: get_center_longitude
// DESCRIPTION: Returns value of center_longitude variable
///////////////////////////////////////////////////////

float NIDS_INFO::get_center_longitude() const
{
  return center_longitude;
}

///////////////////////////////////////////////////////
// FUNCTION: get_num_cols
// DESCRIPTION: Returns value of num_cols variable
///////////////////////////////////////////////////////

int NIDS_INFO::get_num_cols() const
{
  return num_cols;
}

///////////////////////////////////////////////////////
// FUNCTION: get_num_rows
// DESCRIPTION: Returns value of num_rows variable
///////////////////////////////////////////////////////

int NIDS_INFO::get_num_rows() const
{
  return num_rows;
}

///////////////////////////////////////////////////////
// FUNCTION: get_values_tag
// DESCRIPTION: Returns value of values_tag variable
///////////////////////////////////////////////////////

string NIDS_INFO::get_values_tag() const
{
  return values_tag;
}

///////////////////////////////////////////////////////
// FUNCTION: get_colors_tag
// DESCRIPTION: Returns value of colors_tag variable
///////////////////////////////////////////////////////

string NIDS_INFO::get_colors_tag() const
{
  return colors_tag;
}

///////////////////////////////////////////////////////
// FUNCTION: get_swap_flag
// DESCRIPTION: Returns value of swap_flag variable
///////////////////////////////////////////////////////

int NIDS_INFO::get_swap_flag() const
{
  return swap_flag;
}

