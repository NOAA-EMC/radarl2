#ifndef NIDS_INFO_H
#define NIDS_INFO_H

//////////////////////////////////////////////////////////////////////////
//    FILE:  NIDS_INFO.h						//
//   CLASS:  NIDS_INFO							//
// PURPOSE:  REPRESENT NIDS INFORMATION					//
//  AUTHOR:  CHRIS CALVERT                                              //
//    DATE:  9/25/01                                                    //
//////////////////////////////////////////////////////////////////////////


#include<string>


#define NIDS_INFO_DEFAULT_PRODUCT_CODE			0
#define NIDS_INFO_DEFAULT_EPOCH_DAYS			0
#define NIDS_INFO_DEFAULT_SECONDS_SINCE_MIDNIGHT	0
#define NIDS_INFO_DEFAULT_CENTER_LATITUDE		0.0
#define NIDS_INFO_DEFAULT_CENTER_LONGITUDE		0.0
#define NIDS_INFO_DEFAULT_NUM_COLS			0 
#define NIDS_INFO_DEFAULT_NUM_ROWS			0 
#define NIDS_INFO_DEFAULT_VALUES_TAG			"/tmp"
#define NIDS_INFO_DEFAULT_COLORS_TAG			"/tmp"


class NIDS_INFO
{

  private:

    //VARIABLES

    /*                                             */
    /* product_code - NIDS code for some product.  */
    /* epoch_days - days since 1/1/70.             */
    /* seconds_since_midnight - just what it says. */
    /* center_latitude - latitude of center of     */
    /*           center of domain.                 */
    /* center_longitude - longitude of center of   */
    /*           center of domain.                 */
    /* num_cols - number of data columns.          */
    /* num_rows - number of data rows.             */
    /* values_tag - string to search for in the    */
    /*           config file, after which the      */
    /*           product's threshold follow.       */
    /* colors_tag - string to search for in the    */
    /*           config file, after which the      */
    /*           product's color codes follow.     */
    /*                                             */

      short          product_code;
      short          epoch_days;
      long           seconds_since_midnight;
      float          center_latitude;
      float          center_longitude;
      int            num_cols;
      int            num_rows;
      std::string    values_tag;
      std::string    colors_tag;
      int            swap_flag;


  public:
  
    //METHODS
  
     //constructor

       NIDS_INFO();

     //set methods

       void set_product_code( short );
       void set_epoch_days( short );
       void set_seconds_since_midnight( long );
       void set_center_latitude( float );
       void set_center_longitude( float );
       void set_num_cols( int );
       void set_num_rows( int );
       void set_values_tag( std::string );
       void set_colors_tag( std::string );
       void set_swap_flag( int );

     //get methods

       short get_product_code() const;
       short get_epoch_days() const;
       long           get_seconds_since_midnight() const;
       float          get_center_latitude() const;
       float          get_center_longitude() const;
       int            get_num_cols() const;
       int            get_num_rows() const;
       std::string    get_values_tag() const;
       std::string    get_colors_tag() const;
       int            get_swap_flag() const;

};
  

#endif
