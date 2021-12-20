/* 
 * RCS info
 * $Author: ccalvert $
 * $Locker:  $
 * $Date: 2010/05/24 19:59:34 $
 * $Id: pbd_process_data.c,v 1.86 2010/05/24 19:59:34 ccalvert Exp $
 * $Revision: 1.86 $
 * $State: Exp $
 */  

#include <pbd.h>

#define MAX_ADD_MOMENTS   	5	/* Maximum additional moments in an RPG 
                                	   radial. */

#define MAX_AZI_DIFF		1.0f 	/* Max match error on azimuth (split cuts) */
#define MAX_ELE_DIFF    	0.5f	/* Max match error on elevation (split cuts) */

/* Constants for accelerating azimuth match */
#define TEST_ANGLE		15.0f	/* For large angle difference test in 
				           Search_for_saved_radial */ 

#define N_JUMP			8	/* Number of radials to skip; We assume that
				           the azimuth differences among a contiguous 
				           N_JUMP radials are always less than 
				           TEST_ANGLE degrees */
/* Static Globals. */
static short RPG_elev_cut_num;		/* RPG elevation cut # for saved radial. */	

/* Local Function Prototypes */
static int Move_additional_data( char *rda_msg, char *rpg_msg );
static int Add_DP_data( char *gbd, char *rpg_radial, int index,
                        int fields_added );
static int Insert_data( char *rpg_radial, int add_mom, int offset,
                        int max_bins, Generic_moment_t *mom );
/*******************************************************************

   Description:
      This function moves the three data fields from the RDA 
      radial data message to the RPG base data structure. In
      RPG data array, the first data corresponds to the 
      measurement at the range of the bin size.
      The data arrays are terminated with BASEDATA_INVALID on
      both near and far range sides if data is missing.

   Inputs:
      gdb - Generic basedata radial message.

   Outputs:
      rpg_radial - RPG basedata buffer.

   Returns:
      This function returns 0 on success or -1 on failure. So far
      there is no failure condition.

********************************************************************/
int PD_move_data( Generic_basedata_t *gbd, char *rpg_radial ){

    unsigned char *ref_src_ptr = NULL, *vel_src_ptr = NULL;
    unsigned char *wid_src_ptr = NULL;
    int radial_index, fields_added, msg_type, i;

    Base_data_header *rpg_hd = (Base_data_header *) rpg_radial;
    Generic_moment_t *mom = NULL;
    Moment_t *ref_dest_ptr = NULL, *vel_dest_ptr = NULL;
    Moment_t *wid_dest_ptr = NULL;

    msg_type = (int) gbd->msg_hdr.type;

    PBD_saved_ind = -1;
    /* Make a copy of the radial index. */
    radial_index = PBD_saved_ind;

    /* Process reflectivity data. */
    if( (Info.num_surv_bins + Info.rpg_surv_bin_off) > (int) MAX_BASEDATA_REF_SIZE ){
/*
	LE_send_msg ( GL_STATUS | LE_RPG_WARN_STATUS, 
                      "Too Many Reflectivity Bins (%d > %d For RDA Cut %d)\n",
		      Info.num_surv_bins, MAX_BASEDATA_REF_SIZE, gbd->base.elev_num );
*/
	Info.num_surv_bins = MAX_BASEDATA_REF_SIZE - Info.rpg_surv_bin_off;

    }

    /* Where we store the reflectivity data. */
    ref_dest_ptr = (Moment_t *) ((short *) rpg_radial + BASEDATA_REF_OFF);

    /* Where we get the reflectivity data. */
    if( PBD_saved_ind >= 0 ){		

       /* Data was part of split cut. */
       ref_src_ptr = (unsigned char *) &PBD_saved_ref[PBD_saved_ind].ref[0];

       /* Indicate that reflectivity data is enabled and is inserted into this 
          radial. */
       rpg_hd->msg_type |= (REF_INSERT_BIT | REF_ENABLED_BIT);

       /* Transfer the saved reflectivity data to RPG radial. */
       for( i = 0; i < Info.num_surv_bins; i++ )	
          ref_dest_ptr[i + Info.rpg_surv_bin_off] = 
                           (Moment_t) ref_src_ptr[i + Info.rda_surv_bin_off];

    }
    else{

       mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DREF );
       if( (mom != NULL) && (mom->data_word_size == BYTE_MOMENT_DATA) ){

          ref_src_ptr = (unsigned char *) &mom->gate.b[0];  
          for( i = 0; i < Info.num_surv_bins; i++ )	
             ref_dest_ptr[i + Info.rpg_surv_bin_off] = 
                                 (Moment_t) ref_src_ptr[i + Info.rda_surv_bin_off];

          /* Indicate that reflectivity is enabled. */
          rpg_hd->msg_type |= REF_ENABLED_BIT;

       }
       else{

          /* The number of data bins is non-zero, the data is not in the 
             expected format or the data is not available so set all bins 
             to below threshold. */
          for( i = 0; i < Info.num_surv_bins; i++ )	
             ref_dest_ptr[i + Info.rpg_surv_bin_off] = 0;

       }

    }

    /* Terminate the array with invalid data. */
    rpg_hd->ref_offset = 0;
    if( Info.num_surv_bins > 0 ){

       if( Info.rpg_surv_bin_off > 0 )
          memset( ref_dest_ptr, 0, Info.rpg_surv_bin_off*sizeof(Moment_t) );

       if( (Info.rpg_surv_bin_off + Info.num_surv_bins) < (MAX_BASEDATA_REF_SIZE-1) )
          ref_dest_ptr[Info.rpg_surv_bin_off + Info.num_surv_bins] = BASEDATA_INVALID;

       /* Set offset, in number of bytes, to reflectivity data in the radial header. */
       rpg_hd->ref_offset = (unsigned short) ((char *) ref_dest_ptr - rpg_radial);

       /* Set the message size dependent on the availability of Reflectivity moment. */
       rpg_hd->msg_len =
           (rpg_hd->ref_offset + (rpg_hd->n_surv_bins*sizeof(Moment_t)) + 1)/sizeof(short);

    }

    /* Process Doppler data (radial velocity and spectrum width). */
    if( (Info.num_dop_bins + Info.rpg_dop_bin_off) > (int) BASEDATA_DOP_SIZE ){
/*
        printf("Info.num_dop_bins %d    %d\n", Info.num_dop_bins, Info.rpg_dop_bin_off);
	LE_send_msg ( GL_STATUS | LE_RPG_WARN_STATUS, 
                      "Too Many Doppler Bins (%d > %d For RDA Cut %d)\n",
		      Info.num_dop_bins, BASEDATA_DOP_SIZE, gbd->base.elev_num );
*/
	Info.num_dop_bins = (int) BASEDATA_DOP_SIZE - Info.rpg_dop_bin_off;

    }

    vel_dest_ptr = (Moment_t *) ((short *) rpg_radial + BASEDATA_VEL_OFF);
    wid_dest_ptr = (Moment_t *) ((short *) rpg_radial + BASEDATA_SPW_OFF);

    mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DVEL );
    if( (mom != NULL) && (mom->data_word_size == BYTE_MOMENT_DATA) ){

       vel_src_ptr = (unsigned char *) &mom->gate.b[0];  
       for (i = 0; i < Info.num_dop_bins; i++)
          vel_dest_ptr[i + Info.rpg_dop_bin_off] = 
                              (Moment_t) vel_src_ptr[i + Info.rda_dop_bin_off];	

       /* Indicate velocity is enabled. */
       rpg_hd->msg_type |= VEL_ENABLED_BIT;

    }
    else{

       /* The number of data bins is non-zero, the data is not in the expected 
          format or the data is not available so set all bins to below threshold. */
       for( i = 0; i < Info.num_dop_bins; i++ )	
          vel_dest_ptr[i + Info.rpg_dop_bin_off] = 0;

    }

    mom = (Generic_moment_t *) ORPGGDR_get_data_block( (char *) gbd, ORPGGDR_DSW );
    if( (mom != NULL) && (mom->data_word_size == BYTE_MOMENT_DATA) ){

       wid_src_ptr = (unsigned char*) &mom->gate.b[0];  
       for (i = 0; i < Info.num_dop_bins; i++)
          wid_dest_ptr[i + Info.rpg_dop_bin_off] = 
                              (Moment_t) wid_src_ptr[i + Info.rda_dop_bin_off];	

       /* Indicate velocity is enabled. */
       rpg_hd->msg_type |= VEL_ENABLED_BIT;

    }
    else{

       /* The number of data bins is non-zero, the data is not in the expected 
          format or the data is not available so set all bins to below threshold. */
       for( i = 0; i < Info.num_dop_bins; i++ )	
          wid_dest_ptr[i + Info.rpg_dop_bin_off] = 0;

    }

    /* Terminate the arrays with invalid data. */
    rpg_hd->vel_offset = 0;
    rpg_hd->spw_offset = 0;
    if( Info.num_dop_bins > 0 ){

       if( Info.rpg_dop_bin_off > 0 ){

          memset( vel_dest_ptr, 0, Info.rpg_dop_bin_off*sizeof(Moment_t) );
          memset( wid_dest_ptr, 0, Info.rpg_dop_bin_off*sizeof(Moment_t) );

       }

       if( (Info.rpg_dop_bin_off + Info.num_dop_bins) < (BASEDATA_DOP_SIZE - 1) ){

	  vel_dest_ptr[Info.rpg_dop_bin_off + Info.num_dop_bins] = BASEDATA_INVALID;
	  wid_dest_ptr[Info.rpg_dop_bin_off + Info.num_dop_bins] = BASEDATA_INVALID;

       }

       /* Set offsets to velocity and spectrum width data in the radial header. */
       rpg_hd->vel_offset = (unsigned short) ((char *) vel_dest_ptr - rpg_radial);    
       rpg_hd->spw_offset = (unsigned short) ((char *) wid_dest_ptr - rpg_radial);    

       /* Set the size of the radial message dependent on the availability of 
          Velocity and/or Spectrum Width data. */
       if( (rpg_hd->vel_offset > rpg_hd->ref_offset)
                              ||
           (rpg_hd->spw_offset > rpg_hd->ref_offset) ){

          if( rpg_hd->vel_offset > rpg_hd->spw_offset )
             rpg_hd->msg_len = (rpg_hd->vel_offset + 
                               (rpg_hd->n_dop_bins*sizeof(Moment_t)) + 1)/sizeof(short);

          else
             rpg_hd->msg_len = (rpg_hd->spw_offset + 
                               (rpg_hd->n_dop_bins*sizeof(Moment_t)) + 1)/sizeof(short);

       }

    }

    /* Move additional data to RPG radial. */
    fields_added = Move_additional_data( (char *) gbd, rpg_radial );

    /* Do we need to add in the DP data? */
    if( radial_index >= 0 )
       Add_DP_data( (char *) gbd, rpg_radial, radial_index, fields_added );

    return (0);

/* End of PD_move_data */
}

/*******************************************************************

   Description:
      With generic type radial format, additional moments other
      than the standard issue (reflectivity, velocity, spectrum
      width) may be included with this radial.  These moments need
      to be copied to the rpg radial. 

      This function checks if the radial has DP data already residing
      in the radial and moves the data if it does.

   Inputs:
      rda_msg - Pointer to Generic type (Message 1) message.

   Outputs:
      rpg_radial - Pointer to RPG radial mesage.

   Returns:
      On error returns -1, 0 if DP data not moved, or 1 if DP data 
      moved.   If DP data moved, this means there is DP data already
      in this radial.   For Doppler split cuts, it doesn't need to 
      be moved from the surveillance scan.

*******************************************************************/
static int Move_additional_data( char *rda_msg, char *rpg_radial ){

   Generic_basedata_t *generic_hd = (Generic_basedata_t *) rda_msg;
   Base_data_header *rpg_hd = (Base_data_header *) rpg_radial;
   Generic_moment_t *mom_block, *mom;
   char str_type[5];
   int dualpol_added, offset, add_mom, max_bins, field_added, i;
   int size = 0, bin_offset = 0;
   float range;
    
   offset = rpg_hd->msg_len*sizeof(short);
   offset += (offset % sizeof(int));

   /* Go through all additional moments (up to MAX_ADD_MOMENTS), and append each
      to rpg_msg. */
   add_mom = 0;
   dualpol_added = 0;
   field_added = 0;
   for( i = 0; i < generic_hd->base.no_of_datum; i++ ){

      Generic_any_t *data_block = (Generic_any_t *)
                 (rda_msg + sizeof(RDA_RPG_message_header_t) + generic_hd->base.data[i]);

      /* Convert the name to a string so we can do string compares. */
      memset( str_type, 0, 5 );
      memcpy( str_type, data_block->name, sizeof(data_block->name) );

      /* If not a moment data block, ignore it. */
      if( str_type[0] != 'D' )
         continue;

      /* Ignore if one of the standard 3 moments.  These moments have
         already been copied to the RPG radial message. */
      if( (strstr( str_type, "DREF" ) != NULL)
                      ||
          (strstr( str_type, "DVEL" ) != NULL)
                      ||
          (strstr( str_type, "DSW" ) != NULL) )
         continue;

      /* Check to see if any of the Dual Pol data items are added. */
      if( strstr( str_type, "DRHO" ) != NULL ){

         field_added = PBD_DRHO_MOVED;
         max_bins = BASEDATA_RHO_SIZE;

      }      
      else if( strstr( str_type, "DPHI" ) != NULL ){

         field_added = PBD_DPHI_MOVED;
         max_bins = BASEDATA_PHI_SIZE;

      }
      else if( strstr( str_type, "DZDR" ) != NULL ){

         field_added = PBD_DZDR_MOVED;
         max_bins = BASEDATA_ZDR_SIZE;
                      
      }
      else if( strstr( str_type, "DSNR" ) != NULL ){

         field_added = 0;
         max_bins = BASEDATA_SNR_SIZE;

      }
      else if( strstr( str_type, "DRFR" ) != NULL ){

         field_added = 0;
         max_bins = BASEDATA_RFR_SIZE;
                      
      }
      else{

         /* ????? */
         field_added = 0;
         continue;

      }

      mom = (Generic_moment_t *) data_block;

      /* Is the data in the expected resolution? */
      if( (mom->data_word_size != BYTE_MOMENT_DATA)
                          &&
          (mom->data_word_size != SHORT_MOMENT_DATA) ){

         LE_send_msg( GL_INFO, "%d-bit Moment Data, %d or %d-bit Data Expected\n",
                      mom->data_word_size, BYTE_MOMENT_DATA, SHORT_MOMENT_DATA );
         LE_send_msg( GL_INFO, " ... Skipping Moment\n" );

         field_added = 0; 
         continue;

      }

      /* The data field is recognized ... let's move it. */
      if( field_added )
         dualpol_added |= field_added;

      /* Set the offset in the RPG radial header. */
      rpg_hd->offsets[rpg_hd->no_moments + add_mom] = offset;

      /* Move the data block header ..... */
      mom_block = (Generic_moment_t *) (rpg_radial + offset);
      memcpy( (char *) mom_block, mom, sizeof(Generic_moment_t) );
      offset += sizeof(Generic_moment_t);

      /* Calculate the bin offset value.   This is used to determine the number
         of pad values (0's) to prepend to the data. */
      range = (float) (mom->first_gate_range - mom->bin_size/2);
      bin_offset = Round( range / (float) mom->bin_size );
                      
      /* Ensure the number of gates is correct and not more than the max. */
      if( (mom->no_of_gates + bin_offset) > max_bins ){

         int diff = (mom->no_of_gates + bin_offset) - max_bins;
         mom_block->no_of_gates -= diff;

      }

      if( mom->data_word_size == BYTE_MOMENT_DATA ){

         memcpy( rpg_radial + offset + bin_offset, 
                 &mom->gate.b[0], mom_block->no_of_gates*sizeof(char) ); 
         size = mom_block->no_of_gates*sizeof(char);

      }
      else if( mom->data_word_size == SHORT_MOMENT_DATA ){

         memcpy( rpg_radial + offset + bin_offset*sizeof(short), 
                 &mom->gate.u_s[0], mom_block->no_of_gates*sizeof(short) ); 
         size = mom_block->no_of_gates*sizeof(short);

      }

      /* Adjust the offset based on number of bins.  The number of bins is affected
         by the bin_offset value. */
      if( mom_block->no_of_gates > 0 ){

         int pad_size = 0;

         /* Set any lending data before actual start of radial to below 
            threshold. */
         if( bin_offset > 0 ){

            if( mom->data_word_size == BYTE_MOMENT_DATA ){

               pad_size = bin_offset*sizeof(char);
               memset( rpg_radial + offset, 0, pad_size );

            }
            else if( mom->data_word_size == SHORT_MOMENT_DATA ){

               pad_size = bin_offset*sizeof(short);
               memset( rpg_radial + offset, 0, pad_size );

            }

            /* Adjust the first gate range ... */
            mom_block->first_gate_range -= bin_offset*mom->bin_size;

            /* Adjust the number of bins. */
            mom_block->no_of_gates += bin_offset;

         }

         /* Adjust the size of the radial, in bytes. */
         offset += size + pad_size;

      }

      /* Ensure the offset is defined on a word boundary. */
      offset += (offset % sizeof(int));

      add_mom++;
      if( (rpg_hd->no_moments + add_mom) >= MAX_ADD_MOMENTS )
         break;
      
   }

   /* Increase the size of RPG radial to accomodate the additional data.
      Make sure the message length in word aligned. */
   offset += (offset % sizeof(int)); 
   rpg_hd->msg_len = offset/sizeof(short);

   /* Set the number of additional moments. */
   rpg_hd->no_moments += add_mom;

   /* Set the DUALPOL_TYPE flag if dual pol data provided in this
      radial. */
   if( dualpol_added )
      rpg_hd->msg_type |= DUALPOL_TYPE; 

   return dualpol_added;

/* End of Move_additional_data() */
}

/*******************************************************************

   Description:
      Adds DP data to Doppler split cut radial from data from 
      Surveillance split cut radial.

   Inputs:
      rda_msg - Pointer to Generic type (Message 1) message.
      index - index of radial from Surveillance split cut.
      fields_added - bit map of DP fields already in radial.

   Outputs:
      rpg_radial - Pointer to RPG radial mesage.

   Returns:
      On error returns -1, 0 otherwise.

*******************************************************************/
static int Add_DP_data( char *rda_msg, char *rpg_radial, int index,
                        int fields_added ){

   Base_data_header *rpg_hd = (Base_data_header *) rpg_radial;
   int dualpol_added, offset, add_mom, max_bins;
    
   offset = rpg_hd->msg_len*sizeof(short);
   offset += (offset % sizeof(int));

   add_mom = 0;
   dualpol_added = 0;

   /* Add ZDR if needed. */
   if( !(fields_added & PBD_DZDR_MOVED) 
                     && 
       (PBD_saved_ref[index].has_zdr)
                     &&
       ((rpg_hd->no_moments + add_mom) < MAX_ADD_MOMENTS ) ){

      /* Set flag to indicate this radial has DP data. */
      dualpol_added = 1;

      /* Set the offset in the RPG radial header. */
      rpg_hd->offsets[rpg_hd->no_moments + add_mom] = offset;

      /* Determine the maximum number of bins allowed. */
      max_bins = BASEDATA_ZDR_SIZE;

      /* Insert the ZDR data. */
      offset = Insert_data( rpg_radial, add_mom, offset, max_bins, 
                            &PBD_saved_ref[index].zdr_mom );

      /* Increment the number of added data fields. */
      add_mom++;

   }

   /* Add PHI if needed. */
   if( !(fields_added & PBD_DPHI_MOVED) 
                     && 
       (PBD_saved_ref[index].has_phi) 
                     &&
       ((rpg_hd->no_moments + add_mom) < MAX_ADD_MOMENTS ) ){

      /* Set flag to indicate this radial has DP data. */
      dualpol_added = 1;

      /* Set the offset in the RPG radial header. */
      rpg_hd->offsets[rpg_hd->no_moments + add_mom] = offset;

      /* Determine the maximum number of bins allowed. */
      max_bins = BASEDATA_PHI_SIZE;

      /* Insert the PHI data. */
      offset = Insert_data( rpg_radial, add_mom, offset, max_bins,
                            &PBD_saved_ref[index].phi_mom );

      /* Increment the number of added data fields. */
      add_mom++;

   }

   /* Add RHO if needed. */
   if( !(fields_added & PBD_DRHO_MOVED) 
                     && 
       (PBD_saved_ref[index].has_rho) 
                     &&
       ((rpg_hd->no_moments + add_mom) < MAX_ADD_MOMENTS ) ){

      /* Set flag to indicate this radial has DP data. */
      dualpol_added = 1;

      /* Set the offset in the RPG radial header. */
      rpg_hd->offsets[rpg_hd->no_moments + add_mom] = offset;

      /* Determine the maximum number of bins allowed. */
      max_bins = BASEDATA_RHO_SIZE;

      /* Insert the RHO data. */
      offset = Insert_data( rpg_radial, add_mom, offset, max_bins,
                            &PBD_saved_ref[index].rho_mom );

      /* Increment the number of added data fields. */
      add_mom++;

   }
      
   /* Increase the size of RPG radial to accomodate the additional data.
      Make sure the message length in word aligned. */
   offset += (offset % sizeof(int)); 
   rpg_hd->msg_len = offset/sizeof(short);

   /* Set the number of additional moments. */
   rpg_hd->no_moments += add_mom;

   /* Set the DUALPOL_TYPE flag if dual pol data provided in this
      radial. */
   if( dualpol_added )
      rpg_hd->msg_type |= DUALPOL_TYPE; 

   return 0;

/* End of Add_DP_data() */
}

/******************************************************************

   Description:
      Inserts DP data into a radial. 

   Inputs:
      rpg_radial - RPG radial. 
      add_mom - moment index.
      offset - radial offset to place the DP data.
      max_bins - maximum number of bins in a radial.
      mom - DP data.

   Returns:
      Offset to the end of the radial.
      
******************************************************************/
static int Insert_data( char *rpg_radial, int add_mom, int offset, 
                        int max_bins, Generic_moment_t *mom ){

   Base_data_header *rpg_hd = (Base_data_header *) rpg_radial;
   Generic_moment_t *mom_block = (Generic_moment_t *) (rpg_radial + offset);
   int size = 0, bin_offset = 0;
   float range;

   /* Set the offset in the RPG radial header. */
   rpg_hd->offsets[rpg_hd->no_moments + add_mom] = offset;

   /* Move the data block header ..... */
   memcpy( mom_block, mom, sizeof(Generic_moment_t) );
   offset += sizeof(Generic_moment_t);

   /* Calculate the bin offset.   This value is used to determine the 
      number of pad values (0's) to prepend to the data. */
   range = (float) (mom->first_gate_range - mom->bin_size/2);
   bin_offset = Round( range / (float) mom->bin_size );

   /* Ensure the number of gates is correct and not more than the max. */
   if( (mom->no_of_gates + bin_offset) > max_bins ){

      int diff = (mom->no_of_gates + bin_offset) - max_bins;
      mom_block->no_of_gates -= diff;

   }

   if( mom->data_word_size == BYTE_MOMENT_DATA ){

      memcpy( rpg_radial + offset + bin_offset, 
              &mom->gate.b[0], mom_block->no_of_gates*sizeof(char) );
      size = mom_block->no_of_gates*sizeof(char);

   }
   else if( mom->data_word_size == SHORT_MOMENT_DATA ){

      memcpy( rpg_radial + offset + bin_offset*sizeof(short), 
              &mom->gate.u_s[0], mom_block->no_of_gates*sizeof(short) );
      size = mom_block->no_of_gates*sizeof(short);

   }

   /* Set any lending data before actual start of radial to below
         threshold. */
   if( mom_block->no_of_gates > 0 ){

      int pad_size = 0;

      if( bin_offset > 0 ){

         if( mom->data_word_size == BYTE_MOMENT_DATA ){

            pad_size = bin_offset*sizeof(char);
            memset( rpg_radial + offset, 0, pad_size );

         }
         else if( mom->data_word_size == SHORT_MOMENT_DATA ){

            pad_size = bin_offset*sizeof(short);
            memset( rpg_radial + offset, 0, pad_size );

         }

         /* Adjust the first gate range ... */
         mom_block->first_gate_range -= bin_offset*mom->bin_size;

         /* Adjust the number of bins. */
         mom_block->no_of_gates += bin_offset;

      }

      /* Adjust the size of the radial, in bytes. */
      offset += size + pad_size;

   }

   /* Ensure the offset is defined on a word boundary. */
   offset += (offset % sizeof(int));

   return offset;

/* End of Insert_data(). */
}
