/*12345678912345678912345678912345678912345678912345678912345678*/
/*								*/
/* read_superob.h --- header file for read_superob.c		*/
/* author: Yukuan Song						*/
/* version: 1.0							*/
/* time: May 31, 2002						*/
/*								*/
/****************************************************************/
#ifndef READ_SUPEROB_H
#define READ_SUPEROB_H
typedef struct {   /* message header block 			*/
    short msg_code;/* message code: -131 - -16; 0 - 211 	*/
    short date;    /* date of the message; modified Julian date */
    int time;      /* time of the message; GMT; seconds after
                      midnight, 1/1/1970 			*/
    int length;    /* message length; number of bytes including
                      this header; Note that this is the length
                      in the C structure format. 		*/
    short src_id;  /* ID of the sender: 0 - 999 		*/
    short dest_id; /* ID of the receiver: 0 - 999 		*/
    short n_blocks;/* header block plus the product description
                      blocks in the message 			*/
    short divider;	/* value of -1 used to delineate the header from 
			   the above message header; DIV1OFF 10 */
    int latitude;	/* latitude of radar in .001 degrees, -90. - 90.; 
			   LTMSWOFF LTLSWOFF */
    int longitude;	/* longitude of radar in .001 degrees, 
			   -180. - 180. East (+) and West (-); LNMSWOFF 
			   LNLSWOFF */
    short height;	/* above mean sea level height of radar in feet, 
			   -100 - 11000 RADHGTOFF */
    short prod_code;	/* Internel product code; 16 - 131; negative 
			   number is used sometimes; the same as msg_code?;
			   PRDCODOFF 16 */
    short op_mode;	/* operational (weather) mode; 0 = maintenance; 
			   1 = clear air; 2 = precipitation (severe weather);
			   WTMODOFF */
    short vcp_num;	/* VCP number; 1 - 767; VCPOFF */
    short seq_num;	/* sequence number of the request that generated 
			   the product (refer to figure 3-3, RPG/PUP ICD); 
			   for products generated by an alert condition, 
			   = -13; SQNUMOFF */
    short vol_num;	/* counter, recycles to one (1) every 80 volume 
			   scans; 1 - 80, VSNUMOFF */
    short vol_date;	/* Modified Julian date; VSDATOFF */
    short vol_time_s;	/* number of seconds after midnight GMT		*/ 
    unsigned short vol_time_l;/* number of seconds after midnight GMT		*/ 
    short gen_date;	/* product generation data; modified Julian date; 
			   GDPRDOFF */
    int gen_time;	/* product generation time; seconds after 
			   midnight GMT; GTMSWOFF GTLSWOFF */

    short base_time;	/* base time -- center of time window    	*
 			 * minutes from midnight 			*/ 
    short time_radius;	/* time radius of time window			*/ 
    short elev_ind;	/* RPG elevation index in a volume (not cut number); 
			   1 - 20; ELINDOFF */

    short param_3;	/* product dependent parameter.  For products 
			   16 - 21, 28 - 30, 22 - 27, 49, 88, 87, 43, 46, 
			   45, 44, 55, 56, elevation angle; in .1 degrees;
			   -1.0 - 45.0; for product 84; horizontal wind 
			   altitude in 1000 feet, ; 0 to 70; EAZALOFF 30 */    

    short level_1;	/* data lev. thre. (see p3-63, PRG/PUP ICD);
			   DL1OFF 31 */
    short level_2;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL2OFF */
    short level_3;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL3OFF */
    short level_4;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL4OFF */
    short level_5;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL5OFF */
    short level_6;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL6OFF */
    short level_7;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL7OFF */
    short level_8;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL8OFF */
    short level_9;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL9OFF */
    short level_10;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL10OFF */
    short level_11;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL11OFF */
    short level_12;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL12OFF */
    short level_13;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL13OFF */
    short level_14;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL14OFF  */
    short level_15;	/* data lev. thre. (see p3-63, PRG/PUP ICD) DL15OFF*/
    short level_16;	/* data lev. thre. (see p3-63, PRG/PUP ICD);
			   DL16OFF 46 */

    short cell_range_size;/* size of each cell range in km		*/ 
			    
    short cell_azimuth_size;/* size of each cell azimuth in degree	*/  

    short maximum_range;/* maximum range from the radar km		*/ 

    short min_num_points;/* minimum number of points in each cell	*/ 

    short param_8;	
    short param_9;	/* product dependent parameters.
			   product: 16 - 21, 39 - 40, 35 - 38, 50, 85, 63, 
				    65, 64, 66, 89, 90, calibration constant; 
				    in dB; -50. - 50.; CALCONMSW 51 CALCONLSW
			   product: 81; cast to short; rainfall endtime; 
				    minutes; 0 - 1439;
			   product: 74; cast to short; edited indicator; 
				    not 0 for edited;
			   product: 43, 46, 45, 44, ; cast to short; 
				    alert category; see table IV;
			   product: 55; cast to two shorts; 
				storm speed; .1 Knots; 0 - 99.9;  
				storm direction; .1 degrees; 0 - 359.9;
			   product: 56; cast to two shorts; 
				average storm speed; .1 Knots; 0 - 99.9;  
				average storm direction; .1 degrees; 0 - 359.9;
			   product: 80; cast to two shorts;
				end time rainfall; minutes; 0 - 1439;
				rate bias; in .01; 0 - 99.99;
			   product: 78, 79; cast to short; rainfall end time;
				    in minutes; 0 - 1439;
			   product: 84; cast to short; RMS error; Knots;
				    0 - 29; STSPDOFF 51 STDIROFF 52 */

    short param_10;	/* product dependent parameter.
			   product: 39, 40, ; contour interval; dBZ; 5 - 25;
			   product: 42; contour interval; feet; 2000 - 30000;
			   product: 53; alert catagory; see table IV;
			   product: 80; error var of bias; in .01; 0 - 99.99;
			   CNTINTOFF 53 */

    short n_maps;	/* if the message is a map data, this is the number
			   of map pieces, otherwise it is 0; 0 - 17; 
			   NMAPSOFF 54 */

    int sym_off;	/* number of shorts from the top of message (message 
			   code field in header) to the -1 divider of each 
			   block listed. If the offset is zero, the block is
			   not part of the product in question;
			   OPRMSWOFF 55 OPRLSWOFF 56 */
    int gra_off;	/* same as above to graphic block 
			   OGMSWOFF 57 OGLSWOFF 58 */
    int tab_off;	/* same as above to tabular block 
			   OTADMSWOFF 59 OTADLSWOFF 60 */

} Msg_header_pdb;

typedef struct {

    short divider;	/* value of -1 used to delineate the following from 
			   the above product description block; DIV2OFF 61 */
    short block_id;	/* always 1 */
    int block_len;	/* length of this block in bytes including the 
			   preceding devider and block id; 1 - 80000 */
    short n_layers;	/* number of data layers obtained in this block; 
			   1 - 15 */

    short layer_divider;/* value of -1 used to delineate one data layer 
			   from another; */
    int data_len;	/* length of data layer (in bytes) starting from the 
			   bytes after this int and ending at the last data
			   of this layer; 1 - 80000; */
} Symbology_block_header;

#endif
