// ##########################################################################
//
//  CONFIG_PARS.cc: inplement class CONFIG_PARS
//
// ##########################################################################
//
//  Author: Shunxin Wang (CIMMS/NSSL)
//  MAY. 22, 2005
//
//  Modification History:
//
// ##########################################################################

#include "CONFIG_PARS.h"

//#######################################################################
// FUNCTION: get_parameter
// DESCRIPTION: get single parameter from config file
//#######################################################################

template< class Data_Type >
void get_parameter(ifstream &File,Data_Type &par,char* tag,string configfile)
{
    int rc; // a local status flag for "search_file" function
    rc = search_file(File,tag);
    if(rc>0)
       File>>par;
    else
    {
       cout<<"+++ERROR Reading "<<tag<<" from: "<<configfile.c_str()<<endl;
       cout<<"Exiting from get_parameter()."<<endl;
       exit(0);
    }
}

//#######################################################################
//  constructor
//#######################################################################
CONFIG_PARS::CONFIG_PARS(string configfile)
{
    config_file = configfile;
}

//#######################################################################
//  GET_CONFIG
//#######################################################################
int CONFIG_PARS::get_config()
{
    int rc;                // return flag from function: search_file

//#######################################################################
//  Check if the config file exists
//#######################################################################

    ifstream  inFile(config_file.c_str(),ios::in);

    if(!inFile)
    {
      cout<<"+++ERROR+++ Can not open "<<config_file<<endl;
      cout<<"Exiting from get_config"<<endl;
      exit(0);
    }

    //WDSS-II related
    get_parameter(inFile, input_flag, "INPUT_FLAG:", config_file);
    get_parameter(inFile, rad_product, "RAD_PRODUCT:", config_file);
    get_parameter(inFile, vcp_product, "VCP_PRODUCT:", config_file);
    
    
    //Read depictor name:
    get_parameter(inFile,depictorName,"DEPICTORNAME:",config_file);

    //Read radar number
    get_parameter(inFile,radar_number,"RADAR_NUMBER:",config_file);

    //Read variable number
    get_parameter(inFile,variable_number,"VARIABLE_NUMBER:",config_file);

    //Read variable name
    rc = 0;
    rc = search_file(inFile,"VARIABLE_NAME:");
    if(rc>0)
    {
       for(int i = 0; i < variable_number; i++)
          inFile>>variable_name[i];
    }
    else
    {
       cout<<"No data from VARIABLE_NAME:"<<endl;
       exit(0);
    }

    //Read parameters about time windows and frequences
    get_parameter(inFile,mosaic_window,"MOSAIC_WINDOW:",config_file);
    get_parameter(inFile,mosaic_freq,"MOSAIC_FREQ:",config_file);
    get_parameter(inFile,rucwind_window,"RUCWIND_WINDOW:",config_file);
    get_parameter(inFile,rucwind_freq,"RUCWIND_FREQ:",config_file);
    get_parameter(inFile,ruc_tsfc_window,"RUC_TSFC_WINDOW:",config_file);
    get_parameter(inFile,ir_mask_window,"IR_MASK_WINDOW:",config_file);
    get_parameter(inFile,ir_mask_freq,"IR_MASK_FREQ:",config_file);
    get_parameter(inFile,eca_mask_window,"ECA_MASK_WINDOW:",config_file);
    get_parameter(inFile,eca_mask_freq,"ECA_MASK_FREQ:",config_file);
    get_parameter(inFile,ctp_mask_window,"CTP_MASK_WINDOW:",config_file);
    get_parameter(inFile,ctp_mask_freq,"CTP_MASK_FREQ:",config_file);

    prod_timestamp_opt = 0;

    get_parameter(inFile,grid_ref_dir,"GRID_REF_DIR:",config_file);

    //second data option etc
    get_parameter(inFile,second_data_opt,"SECOND_DATA_OPT:",config_file);
    get_parameter(inFile,node_str,"NODE_STR:",config_file);
    get_parameter(inFile,prod_app,"PROD_APP:",config_file);
    get_parameter(inFile,input_dir,"INPUT_DIR:",config_file);
    //get_parameter(inFile,sounding_dir,"SOUNDING_DIR:",config_file);
    //get_parameter(inFile,model_dat_dir,"MODEL_DAT_DIR:",config_file);
    //get_parameter(inFile,ruc_tsfc_dir,"RUC_TSFC_DIR:",config_file);
    //get_parameter(inFile,rucwind_u_dir,"RUCWIND_U_DIR:",config_file);
    //get_parameter(inFile,rucwind_v_dir,"RUCWIND_V_DIR:",config_file);
    get_parameter(inFile,sat_mask_qc_opt,"SAT_MASK_QC_OPT:",config_file);
    get_parameter(inFile,ir_mask_opt,"IR_MASK_OPT:",config_file);
    //get_parameter(inFile,ir_mask_dir,"IR_MASK_DIR:",config_file);
    get_parameter(inFile,eca_mask_opt,"ECA_MASK_OPT:",config_file);
    //get_parameter(inFile,eca_mask_dir,"ECA_MASK_DIR:",config_file);
    get_parameter(inFile,ctp_mask_opt,"CTP_MASK_OPT:",config_file);
    //get_parameter(inFile,ctp_mask_dir,"CTP_MASK_DIR:",config_file);

    //precip flag directories and thresholds
    get_parameter(inFile,strat_thresh,"STRAT_THRESHOLD:",config_file);
    get_parameter(inFile,conv_thresh,"CONV_THRESHOLD:",config_file);
    get_parameter(inFile,conv_thresh_m10c,"CONV_THRESHOLD_-10:",config_file);
    
    //Parameter related to bloom QC logic
    get_parameter(inFile,tsfc_bloom_threshold,"TSFC_BLOOM_THRESHOLD:",config_file);
        
    //Parameters related to conus grid and conus reference data
    get_parameter(inFile,conus_ref_opt,"CONUS_REF_OPT:",config_file);
    get_parameter(inFile,conus_nx,"CONUS_NX:",config_file);
    get_parameter(inFile,conus_ny,"CONUS_NY:",config_file);
    get_parameter(inFile,conus_nz,"CONUS_NZ:",config_file);

    get_parameter(inFile,conus_dx,"CONUS_DX:",config_file);
    get_parameter(inFile,conus_dy,"CONUS_DY:",config_file);
    get_parameter(inFile,conus_ctrlat,"CONUS_CTRLAT:",config_file);
    get_parameter(inFile,conus_ctrlon,"CONUS_CTRLON:",config_file);
    get_parameter(inFile,sat_conus_nx,"SAT_CONUS_NX:",config_file);
    get_parameter(inFile,sat_conus_ny,"SAT_CONUS_NY:",config_file);
    get_parameter(inFile,sat_conus_nz,"SAT_CONUS_NZ:",config_file);
    get_parameter(inFile,sat_conus_dx,"SAT_CONUS_DX:",config_file);
    get_parameter(inFile,sat_conus_dy,"SAT_CONUS_DY:",config_file);
 
    startlat = conus_ctrlat - conus_dy*(float)(conus_ny/2);
    startlon = conus_ctrlon - conus_dx*(float)(conus_nx/2);
 

    //nids output options and directories.
    get_parameter(inFile,lref_opt,"LREF_OPT:",config_file);
    get_parameter(inFile,nids_lref_flag,"NIDS_LREF_FLAG:",config_file);
    //get_parameter(inFile,nids_lref_dir,"NIDS_LREF_DIR:",config_file);

    get_parameter(inFile,tref_opt,"TREF_OPT:",config_file);
    get_parameter(inFile,nids_tref_flag,"NIDS_TREF_FLAG:",config_file);
    //get_parameter(inFile,nids_tref_dir,"NIDS_TREF_DIR:",config_file);

    get_parameter(inFile,nids_cref_flag,"NIDS_CREF_FLAG:",config_file);
    //get_parameter(inFile,nids_cref_dir,"NIDS_CREF_DIR:",config_file);

    get_parameter(inFile,nids_chgt_flag,"NIDS_CHGT_FLAG:",config_file);
    //get_parameter(inFile,nids_chgt_dir,"NIDS_CHGT_DIR:",config_file);

    //options and directories for multivariable netcdf files output
    get_parameter(inFile,mosaic2d_netcdf_remote_output_opt,"MOSAIC2D_NETCDF_REMOTE_OUTPUT_OPT:",config_file);
    get_parameter(inFile,mosaic2d_netcdf_flag,"MOSAIC2D_NETCDF_FLAG:",config_file);
    get_parameter(inFile,mosaic2d_wdssii_netcdf_flag,"MOSAIC2D_WDSSII_NETCDF_FLAG:",config_file);
    get_parameter(inFile,mosaic2d_netcdf_dir,"MOSAIC2D_NETCDF_DIR:",config_file);
    //get_parameter(inFile,mosaic2d_netcdf_dir_remote,"MOSAIC2D_NETCDF_DIR_REMOTE:",config_file);

    get_parameter(inFile,mosaic3d_netcdf_remote_output_opt,"MOSAIC3D_NETCDF_REMOTE_OUTPUT_OPT:",config_file);
    get_parameter(inFile,mosaic3d_netcdf_flag,"MOSAIC3D_NETCDF_FLAG:",config_file);
    get_parameter(inFile,mosaic3d_wdssii_netcdf_flag,"MOSAIC3D_WDSSII_NETCDF_FLAG:",config_file);
    get_parameter(inFile,mosaic3d_netcdf_dir,"MOSAIC3D_NETCDF_DIR:",config_file);
    //get_parameter(inFile,mosaic3d_netcdf_dir_remote,"MOSAIC3D_NETCDF_DIR_REMOTE:",config_file);

    get_parameter(inFile,tref3d_netcdf_flag,"TREF3D_NETCDF_FLAG:",config_file);
    get_parameter(inFile,tref3d_wdssii_netcdf_flag,"TREF3D_WDSSII_NETCDF_FLAG:",config_file);
    //get_parameter(inFile,tref3d_netcdf_dir,"TREF3D_NETCDF_DIR:",config_file);

    //options and directories for binary products
    get_parameter(inFile,bi_mref_flag,"BI_MREF_FLAG:",config_file);
    //get_parameter(inFile,bi_mref_dir,"BI_MREF_DIR:",config_file);

    get_parameter(inFile,bi_tref_flag,"BI_TREF_FLAG:",config_file);
    //get_parameter(inFile,bi_tref_dir,"BI_TREF_DIR:",config_file);

    get_parameter(inFile,bi_cref_flag,"BI_CREF_FLAG:",config_file);
    //get_parameter(inFile,bi_cref_dir,"BI_CREF_DIR:",config_file);

    get_parameter(inFile,bi_chgt_flag,"BI_CHGT_FLAG:",config_file);
    //get_parameter(inFile,bi_chgt_dir,"BI_CHGT_DIR:",config_file);

    get_parameter(inFile,bi_vil_flag,"BI_VIL_FLAG:",config_file);
    //get_parameter(inFile,bi_vil_dir,"BI_VIL_DIR:",config_file);

    get_parameter(inFile,bi_vilD_flag,"BI_VILD_FLAG:",config_file);
    //get_parameter(inFile,bi_vilD_dir,"BI_VILD_DIR:",config_file);

    get_parameter(inFile,bi_shi_flag,"BI_SHI_FLAG:",config_file);
    //get_parameter(inFile,bi_shi_dir,"BI_SHI_DIR:",config_file);

    get_parameter(inFile,bi_posh_flag,"BI_POSH_FLAG:",config_file);
    //get_parameter(inFile,bi_posh_dir,"BI_POSH_DIR:",config_file);

    get_parameter(inFile,bi_mehs_flag,"BI_MEHS_FLAG:",config_file);
    //get_parameter(inFile,bi_mehs_dir,"BI_MEHS_DIR:",config_file);

    get_parameter(inFile,bi_etp_flag,"BI_ETP_FLAG:",config_file);
    //get_parameter(inFile,bi_etp_dir,"BI_ETP_DIR:",config_file);

    get_parameter(inFile,bi_hsr_flag,"BI_HSR_FLAG:",config_file);
    //get_parameter(inFile,bi_hsr_dir,"BI_HSR_DIR:",config_file);

    get_parameter(inFile,bi_hsrh_flag,"BI_HSRH_FLAG:",config_file);
    //get_parameter(inFile,bi_hsrh_dir,"BI_HSRH_DIR:",config_file);

    get_parameter(inFile,bi_lcref_flag,"BI_LCREF_FLAG:",config_file);
    //get_parameter(inFile,bi_lcref_dir,"BI_LCREF_DIR:",config_file);

    get_parameter(inFile,bi_lcrefh_flag,"BI_LCREFH_FLAG:",config_file);
    //get_parameter(inFile,bi_lcrefh_dir,"BI_LCREFH_DIR:",config_file);

    get_parameter(inFile,bi_pcpflag_flag,"BI_PCPFLAG_FLAG:",config_file);
    //get_parameter(inFile,bi_pcpflag_dir,"BI_PCPFLAG_DIR:",config_file);

    get_parameter(inFile,lcref_max_hgt,"LCREF_MAX_HGT:",config_file);

    //  Read DX,DY
    get_parameter(inFile,nx,"NX:",config_file);
    get_parameter(inFile,ny,"NY:",config_file);
    get_parameter(inFile,nz,"NZ:",config_file);
    get_parameter(inFile,dx,"DX:",config_file);
    get_parameter(inFile,dy,"DY:",config_file);
    get_parameter(inFile,ctrlat,"CTRLAT:",config_file);
    get_parameter(inFile,ctrlon,"CTRLON:",config_file);

    rc = search_file(inFile,"Z_LEVELS_MSL:");
    if(rc>0)
    {
      if (nz < 1)
      {
         cout<<"++ERROR NZ="<<nz<<endl;
         exit (0);
      }
      if(debug_flag)
         cout<<"nz="<<nz<<endl;
      for(int k=0; k<nz; k++)
      {
        inFile>>zp[k];

        if(debug_flag)
           cout<<"zp["<<k<<"]:  "<<zp[k]<<endl;

        if(zp[k]<0 || zp[k]>30000)
        {
           cout<<"++ERROR1 incorrect zp definition.  Exit."<<endl;
           exit(0);
        }

        if(k>0 && zp[k]<zp[k-1])
        {
          cout<<"++ERROR2 incorrect zp definition.  Exit."<<endl;
          cout<<"zp["<<k<<"]="<<zp[k]<<"  zp["<<k-1<<"]="<<zp[k-1]<<endl;
          exit(0);
        }
      }
    }
    else
    {
      cout<<"+++ERROR Reading zp from "<<config_file<<endl;
      cout<<"Exiting from get_config_moslut."<<endl;
      exit(0);
    }

    //Map projection option
    get_parameter(inFile,mapproj,"MAPPROJ:",config_file);
    if( mapproj != 4 )
    {
        cout<<"Inconsistent map projection. Exiting from get_config."<<endl;
        exit(0);
    }
    get_parameter(inFile,trulat1,"TRULAT1:",config_file);
    get_parameter(inFile,trulat2,"TRULAT2:",config_file);
    get_parameter(inFile,trulon,"TRULON:",config_file);

    get_parameter(inFile,ternopt,"TERNOPT:",config_file);
    get_parameter(inFile,terndta,"TERNDTA:",config_file);

    get_parameter(inFile,debug_flag,"DEBUG_FLAG:",config_file);
    get_parameter(inFile,swap_flag,"SWAP_FLAG:",config_file);

    //Parameters related to transfering mosaic nodes to nmqserver
    get_parameter(inFile,ldm_netcdf_opt,"LDM_NETCDF_OPT:",config_file);
    get_parameter(inFile,ldm_nids_opt,"LDM_NIDS_OPT:",config_file);
    get_parameter(inFile,ldm_binary_opt,"LDM_BINARY_OPT:",config_file);
    //get_parameter(inFile,pqinsert_file,"PQINSERT_FILE:",config_file);
    //get_parameter(inFile,ldmd_log_file,"LDMD_LOG_FILE:",config_file);
    //get_parameter(inFile,ldm_pq_file,"LDM_PQ_FILE:",config_file);

    get_parameter(inFile,mosaic_info_flag,"MOSAIC_INFO_FLAG:",config_file);
    get_parameter(inFile,mosaic_info_dir,"MOSAIC_INFO_DIR:",config_file);

    //Read parameters of each radar
    char temp[50];
    if (radar_number < 1)
    {
        cout<<"++ERROR radar_number="<<radar_number<<endl;
        exit (0);
    }
    if(debug_flag)
        cout<<"radar_number="<<radar_number<<endl;

    for(int i = 0; i < radar_number; i++)
    {
         rc = 0;
         sprintf(temp,"%s%03d","RADAR",i+1);
         rc = search_file(inFile,temp);

         if(rc>0)
         {
            inFile>>radar_pars_list[i].radar_name;
            inFile>>radar_pars_list[i].nx;
            inFile>>radar_pars_list[i].ny;
            inFile>>radar_pars_list[i].nz;
            inFile>>radar_pars_list[i].dx;
            inFile>>radar_pars_list[i].dy;
            inFile>>radar_pars_list[i].ctrlat;
            inFile>>radar_pars_list[i].ctrlon;
            inFile>>radar_pars_list[i].node_no1;
            inFile>>radar_pars_list[i].node_no2;

            if( radar_pars_list[i].nx < 0 || radar_pars_list[i].ny < 0 ||
                radar_pars_list[i].nz < 0 || radar_pars_list[i].dx < 0 ||
                radar_pars_list[i].dy < 0 )
            {
                cout<<"++incorrect radar parameters input.  Exiting from get_config."<<endl;
                exit(0);
            }
         }
         else
         {
             cout<<"+++ERROR Reading Parameters from "<<temp<<" from "<<config_file<<endl;
             cout<<"Exiting from get_config."<<endl;
             exit(0);
         }
    }

// #######################################################################
//  Parameters related to archived operation
// ######################################################################
    get_parameter(inFile,arc_opt,"ARC_OPT:",config_file);

    if( arc_opt )
    {
       get_parameter(inFile,yr,"YEAR_ARC:",config_file);
       get_parameter(inFile,mo,"MONTH_ARC:",config_file);
       get_parameter(inFile,day,"DAY_ARC:",config_file);
       get_parameter(inFile,hr,"HOUR_ARC:",config_file);
       get_parameter(inFile,min,"MINUTE_ARC:",config_file);
    }

    //Close config file
    inFile.close();

    return 1;
}

//#######################################################################
//  GET_CORNER_COORDINATE
//#######################################################################
int CONFIG_PARS::get_corner_coordinate()
{
//startlat = SE lat of tile domain
//startlon = SE lon of tile domain
    //get corner coordinate for specific domain
    xmin = (int)((ctrlon - startlon)/dx) - nx/2; //should be zero
    xmax = xmin + nx - 1;                        //should be nx
    ymin = (int)((ctrlat - startlat)/dy) - ny/2; //should be zero
    ymax = ymin + ny - 1;                        //should be ny

    //get corner coordinate for all radars
    for( int i = 0; i < radar_number; i++ )
    {
        radar_pars_list[i].xmin = (int)((radar_pars_list[i].ctrlon - startlon)/radar_pars_list[i].dx) -
                                 radar_pars_list[i].nx/2;
        radar_pars_list[i].xmax = radar_pars_list[i].xmin + radar_pars_list[i].nx - 1;
        radar_pars_list[i].ymin = (int)((radar_pars_list[i].ctrlat - startlat)/radar_pars_list[i].dy) -
                                 radar_pars_list[i].ny/2;
        radar_pars_list[i].ymax = radar_pars_list[i].ymin + radar_pars_list[i].ny - 1;
        radar_pars_list[i].ctrx = (int)((radar_pars_list[i].ctrlon - startlon)/radar_pars_list[i].dx);
        radar_pars_list[i].ctry = (int)((radar_pars_list[i].ctrlat - startlat)/radar_pars_list[i].dy);
   }
   return 1;
}

///////////////////////////////////////////////////////////////////////////

