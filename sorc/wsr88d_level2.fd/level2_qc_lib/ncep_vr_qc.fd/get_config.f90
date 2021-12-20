 SUBROUTINE get_config(pp,fix)

        use CONFIG_PARS
        implicit none

        type(CCOUT_CONFIG_PARS)   pp
	character(len=80)         LOCAL_DIR
        character(len=*)          fix
	integer                   i
!kumar
        LOCAL_DIR=fix

        !* LOCAL_DIR='/meso/noscrub/wx22hl/xiaoyong/config_c++'

!///////////////////////////////////////////////////////////////////////                 
!// raw data_type= 1   for level2 format                              //
!//                2   for gematrnik level2 format                    //                                
!//                3   for FAA data format                            //
!//                4   for tdwr data format                           //
!//                5   for WDT data format                            //
!//                6   ......                                         //
!// radial_gap_filling_opt= fill radial gap and remove ground clutter //
!// DBZ_correction_opt= add additional dbz within partially blocked   //
!//                     regins                                        //
!// Unqc_src_opt=  Do src before qc operation                         //
!// Qc_src_opt=  Do src after qc operation                            //
!// Sync_src_opt=  Do src after sync operation                        //
!// Sync_qc_src_opt=  Do src after qc and sync operation              //
!///////////////////////////////////////////////////////////////////////
    pp.DATA_TYPE                                   = 1
    pp.RADIAL_GAP_FILLING_OPT                      = 1
    pp.DBZ_CORRECTION_OPT                          = 1
    pp.UNQC_SRC_OPT                                = 1
    pp.QC_SRC_OPT                                  = 1
    pp.SYNC_SRC_OPT                                = 1
    pp.SYNC_QC_SRC_OPT                             = 1 
    pp.ALIGNMENT_MAX_OPT                           = 1

!///////////////////////////////////////////////////////////////////////
!//  parameters for hardware test check                              //
!///////////////////////////////////////////////////////////////////////
!    HARDWARE_TEST_CHECK_OPT= 1
!    HARDWARE_TEST_CHECK_RANGE= 300
!    HARDWARE_TEST_CHECK_DIR= /usr/local/qpesumsdev/data/QPESUMS/polar/monitor/
!    DIFF_THRESHOLD_OF_ECHO_COVERAGE= 0.45
!    THRESHOLD_OF_ECHO_VALUE=  5.0
!    HARDWARE_TEST_CHECK_FREQ= 300
!    HARDWARE_TEST_CHECK_WINDOW= 1800
!
!///////////////////////////////////////////////////////////////////////
!//  parameters about the algorithm                                   //
!///////////////////////////////////////////////////////////////////////
    pp.RUN_QPESUMS_ALGORITHMS                      = 1
    pp.QPESUMS_GATE_RANGE                          = 460
    pp.QPESUMS_AZIMUTH_RANGE= 360
    pp.QPESUMS_GATE_SPACING=     1000
    pp.QPESUMS_AZIMUTH_SPACING=  1
    pp.MIN_NUMBER_OF_TILTS=      2
    pp.SHORT_PULSE= 0
    pp.DEBUG_FLAG=  1
    pp.BYTESWAP_FLAG= 1
    pp.REMAP_FREQ=    300

!///////////////////////////////////////////////////////////////////////
!//  parameters relared to raw level2 data                            //
!///////////////////////////////////////////////////////////////////////
    pp.NUM_AZIMUTHS=            360
    pp.MAX_NUM_AZIMUTHS=        400
    pp.MAX_NUM_REF_GATES=       500
    pp.MAX_NUM_DOPPLER_GATES=   1000
    pp.PRECIP_TIME_LAG=         360
    pp.CLEAR_AIR_TIME_LAG=      720
    pp.START_TIME_LAG=          180
    pp.VRP_LOW_RES=             2
    pp.LOW_RES_VELOCITY=        63.5
    pp.HIGH_RES_VELOCITY=       127.0
    pp.LEVEL2_PAUSE_TIME=       5
    pp.FILE_HEADER_ROOT_NAME=   "ARCHIVE2"
    pp.FILE_HEADER_ROOT_NAME_2= "AR2V0001"

!////////////////////////////////////////////////////////////////////////////
!//  parameters related to gematronik data(ones not the same with level2   //
!////////////////////////////////////////////////////////////////////////////
    pp.MAX_NUM_LONG_TILTS=      2
    pp.SIZE_SHORT_PULSE_GATE=   500
    pp.SIZE_LONG_PULSE_GATE=    1000

!///////////////////////////////////////////////////////////////////////
!//  parameters relared to hybrid tilt file                           //
!//  hybrid_tilt_scale = 1.0 for hybtilt tilt file                    //
!//                    = 100.0 for hybtilt elevation file             //
!//  hybrid_filename_tag = .hybtilts for hybrid tilt file             //
!//                      = .hybelev for hybrid elevation file         // 
!//  hybrid_file_opt   = 1 for hybrid tilt file                       //
!//                    = 2 for hybrid elevation file                  //
!///////////////////////////////////////////////////////////////////////
    pp.HYBRID_TILT_SCALE=               100.0
    pp.HYBRID_TILT_CODE=                92
    pp.HYBRID_TILT_UNITS_CODE=          2
    pp.HYBRID_FILENAME_TAG=             ".hybelev"
    pp.HYBRID_FILE_OPT=                 2
    pp.BLOCKERGE_FILENAME_TAG=          ".2Docc"

!///////////////////////////////////////////////////////////////////////
!//  directories for parameters related to input reference data       //
!///////////////////////////////////////////////////////////////////////
!    INPUT_BLOCK_LUTS= /usr/local/qpesumsdev/ref_data/SRC_BLOCK_LUT/
!    INPUT_HYBRID_TILTS= /usr/local/qpesumsdev/ref_data/
!    RADAR_INPUT_FILE=  /usr/local/qpesumsdev/ref_data/radarinfo.dat
!    INPUT_NIDS_INFO=  /usr/local/qpesumsdev/ref_data/

!///////////////////////////////////////////////////////////////////////
!//  parameters related to sounding data and model remaped surface    //
!//  temperture data and wind data                                    //
!///////////////////////////////////////////////////////////////////////
!    ENV_SOUNDING_DIRECTORY=   /usr/local/qpesumsdev/data/QPESUMS/soundings/
!    ENV_SOUNDING_FILE_TAG=    .env_sounding.latest
!    ENV_TAG=                  env
!    MODEL_SOUNDING_DIRECTORY= /usr/local/qpesumsdev/data/QPESUMS/grid/model_sounding/
!    MODEL_SOUNDING_FILE_TAG=  .model_sounding.latest
!    MODEL_TAG=                 model
!    SOUNDING_USAGE_DIRECTORY= /usr/local/qpesumsdev/data/QPESUMS/polar/sounding_usage/
!    SOUNDING_USAGE_FILE_TAG=  .sounding_usage.latest
!    CLIMATE_SOUNDING_FILE=    /mnt2/yqin/3d_polar/ref_data/climate_sounding.dat
!    TSFC_MODEL_DIR=      /usr/local/qpesumsdev/data/QPESUMS/grid/tsfc_model
!    RADAR_CTR_TSFC_THRESHOLD= 4.4

!// ######################################################################
!//  Parameters related to conus grid and conus reference data
!// ######################################################################
    pp.CONUS_REF_OPT= 1

    pp.CONUS_NX=  701
    pp.CONUS_NY=  351
    pp.CONUS_NZ=  21

    pp.CONUS_DX=   0.1
    pp.CONUS_DY=   0.1
    pp.CONUS_CTRLAT=  37.5
    pp.CONUS_CTRLON=  -95.0

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output binary file                         //
!///////////////////////////////////////////////////////////////////////
    write(pp.BINARY_OUTPUT_DIRECTORY,'(a,a)') trim(LOCAL_DIR),'/binary/'
    pp.OUTPUT_BREF_RAW_BINARY=	0
    pp.OUTPUT_BREF_BINARY=		0
    pp.OUTPUT_BREF_QC_BINARY=	1
    pp.OUTPUT_DA_BVEL_BINARY=      1
    pp.OUTPUT_BVEL_BINARY=		1
    pp.OUTPUT_BVEL_RAW_BINARY=	0
    pp.OUTPUT_SPWI_BINARY=		0
    pp.OUTPUT_SPWI_RAW_BINARY=	0
    pp.OUTPUT_CREF_BINARY=		1
    pp.OUTPUT_CHGT_BINARY=         0
    pp.OUTPUT_CREF_QC_BINARY=	1
    pp.OUTPUT_CHGT_QC_BINARY=      0
    pp.OUTPUT_LCREF_BINARY=	1
    pp.OUTPUT_LCHGT_BINARY=        0
    pp.OUTPUT_LCREF_QC_BINARY=     0
    pp.OUTPUT_LCHGT_QC_BINARY=     0
    pp.OUTPUT_HREF_BINARY=		1
    pp.OUTPUT_HHGT_BINARY=         1
    pp.OUTPUT_HREF_QC_BINARY=      0
    pp.OUTPUT_WARM_RATE_BINARY=	1
    pp.OUTPUT_WARM_FLAG_BINARY=	1
    pp.OUTPUT_COOL_RATE_BINARY=	1
    pp.OUTPUT_COOL_FLAG_BINARY=	1

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output nids file                           //
!///////////////////////////////////////////////////////////////////////
    write(pp.NIDS_OUTPUT_DIRECTORY,'(a,a)') trim(LOCAL_DIR),'/nids/'
    pp.OUTPUT_BREF_RAW_NIDS=	1
    pp.OUTPUT_BREF_NIDS=		1
    pp.OUTPUT_BREF_QC_NIDS=	1
    pp.OUTPUT_DA_BVEL_NIDS=        1
    pp.OUTPUT_BVEL_RAW_NIDS=	1
    pp.OUTPUT_BVEL_NIDS=		1           
    pp.OUTPUT_SPWI_RAW_NIDS=	1
    pp.OUTPUT_SPWI_NIDS=		1
    pp.OUTPUT_CREF_NIDS=		1
    pp.OUTPUT_CHGT_NIDS=           1
    pp.OUTPUT_CREF_QC_NIDS=	1
    pp.OUTPUT_CHGT_QC_NIDS=	0
    pp.OUTPUT_LCREF_NIDS=		1
    pp.OUTPUT_LCHGT_NIDS=          1
    pp.OUTPUT_LCREF_QC_NIDS=	0
    pp.OUTPUT_LCHGT_QC_NIDS=       0
    pp.OUTPUT_HREF_NIDS=		1
    pp.OUTPUT_HHGT_NIDS=           1
    pp.OUTPUT_HREF_QC_NIDS=	0
    pp.OUTPUT_WARM_RATE_NIDS=	1
    pp.OUTPUT_WARM_FLAG_NIDS=	1
    pp.OUTPUT_COOL_RATE_NIDS=	1
    pp.OUTPUT_COOL_FLAG_NIDS=	1

!///////////////////////////////////////////////////////////////////////
!//  parameters related to base reflectivity                          //
!///////////////////////////////////////////////////////////////////////
    pp.BREF_QPESUMS_PRODUCT_CODE=  100
    pp.BREF_UNITS_CODE=            1
    pp.BREF_SCALE=                 10.0
    pp.BREF_NIDS_PRODUCT_CODE=     19
    pp.BREF_NIDS_HI_INDEX=         15
    pp.BREF_NIDS_LO_INDEX=         1
    pp.BREF_NIDS_MISSING_INDEX=    0
!    pp.BREF_NIDS_VALUES_TAG=       REFLECTIVITY_VALUES=
!    pp.BREF_NIDS_COLORS_TAG=       REFLECTIVITY_COLORS=
!    pp.BREF_FILE_NAME=             BASE_REFLECT.cc
!    pp.BREF_PRODUCT_LABEL=         Reflectivity

!///////////////////////////////////////////////////////////////////////
!//  parameters related to base velocity                              //
!///////////////////////////////////////////////////////////////////////


!///////////////////////////////////////////////////////////////////////
!//  parameters related to output SPEC_WIDTH info                     //
!///////////////////////////////////////////////////////////////////////
 

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output brightband info                     //
!///////////////////////////////////////////////////////////////////////
    write(pp.BRIGHTBAND_DIRECTORY,'(a,a)')trim(LOCAL_DIR),'/brightband/'
    pp.BRIGHTBAND_DROPOFF_RATIO_THRESHOLD=	0.8
    pp.BRIGHTBAND_DBZ_THRESHOLD=		30.0
    pp.BRIGHTBAND_STD_DEV_THRESHOLD=	500.0
    pp.BRIGHTBAND_DBZ_RATIO_THRESHOLD=	0.25
    pp.BRIGHTBAND_NUMBER_OF_TREND_RECORDS=	300
    pp.BRIGHTBAND_VCP11_START_POINT=	10000.0
    pp.BRIGHTBAND_OTHER_START_POINT=	20000.0
    pp.BRIGHTBAND_END_POINT=		30000.0
    pp.BRIGHTBAND_DEFAULT_DEPTH=		1000.0
    pp.BRIGHTBAND_PAST_MINUTES_TO_AVERAGE=	30
    pp.VCP11=                              11
    pp.NUM_SECTORS=                        4
    pp.HYBRID_TILT_THRESHOLD=              3
    pp.VALID_NUM_TILTS=                    5
    pp.MISSING_BB=                         -1.0
!   MISSING_BB_TIME=                    999999999999
!    TIME_SERIES_TAG=                    .time_series
!    pp.DEFAULT_BB_DEPTH=                   1000.0
    pp.DEFAULT_VOLS_TO_AVG=                5
    pp.NUM_BB_INFO_IN_HEADER=              4
    pp.GROUND_HEIGHT=                      0.0

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output Composite Reflectivity info         //
!///////////////////////////////////////////////////////////////////////
    pp.CREF_QPESUMS_PRODUCT_CODE=       10
    pp.CREF_UNITS_CODE=                 1
    pp.CREF_SCALE=                      10.0
    pp.CREF_TILT_NUMBER=                0
    pp.CREF_TILT_ELEVATION=             0.0
    pp.CREF_NIDS_PRODUCT_CODE=          295
    pp.CREF_NIDS_HI_INDEX=              15
    pp.CREF_NIDS_LO_INDEX=              1
    pp.CREF_NIDS_MISSING_INDEX=         0
    pp.CREF_NIDS_VALUES_TAG=            'REFLECTIVITY_VALUES:'
    pp.CREF_NIDS_COLORS_TAG=            'REFLECTIVITY_COLORS:'
    pp.CREF_PRODUCT_LABEL=              'CompositeReflectivity'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output Composite HEIGHT info               //
!///////////////////////////////////////////////////////////////////////
    pp.COMP_HEIGHT_GATE_RANGE=        230
    pp.CHGT_QPESUMS_PRODUCT_CODE=     10
    pp.CHGT_UNITS_CODE=               1
    pp.CHGT_SCALE=                    1.0
    pp.CHGT_TILT_NUMBER=              0
    pp.CHGT_TILT_ELEVATION=           0.0
    pp.CHGT_NIDS_PRODUCT_CODE=        295
    pp.CHGT_NIDS_HI_INDEX=            15
    pp.CHGT_NIDS_LO_INDEX=            1
    pp.CHGT_NIDS_MISSING_INDEX=       0
    pp.CHGT_NIDS_VALUES_TAG=          'HEIGHT_VALUES:'
    pp.CHGT_NIDS_COLORS_TAG=          'HEIGHT_COLORS:'
    pp.CHGT_PRODUCT_LABEL=            'CMPHeight'


!///////////////////////////////////////////////////////////////////////////
!//  parameters related to output lower level Composite Reflectivity info //
!///////////////////////////////////////////////////////////////////////////
    pp.LCREF_MAX_TILT_NUM=               4
    pp.LCREF_QPESUMS_PRODUCT_CODE=       10
    pp.LCREF_UNITS_CODE=                 1
    pp.LCREF_SCALE=                      10.0
    pp.LCREF_TILT_NUMBER=                0
    pp.LCREF_TILT_ELEVATION=             0.0
    pp.LCREF_NIDS_PRODUCT_CODE=          295
    pp.LCREF_NIDS_HI_INDEX=              15
    pp.LCREF_NIDS_LO_INDEX=              1
    pp.LCREF_NIDS_MISSING_INDEX=         0
    pp.LCREF_NIDS_VALUES_TAG=            'REFLECTIVITY_VALUES:'
    pp.LCREF_NIDS_COLORS_TAG=            'REFLECTIVITY_COLORS:'
    pp.LCREF_PRODUCT_LABEL=              'LCompositeReflectivity'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output Lower Composite HEIGHT info         //
!///////////////////////////////////////////////////////////////////////
    pp.LCOMP_HEIGHT_GATE_RANGE=        230
    pp.LCHGT_QPESUMS_PRODUCT_CODE=     10
    pp.LCHGT_UNITS_CODE=               1
    pp.LCHGT_SCALE=                    1.0
    pp.LCHGT_TILT_NUMBER=              0
    pp.LCHGT_TILT_ELEVATION=           0.0
    pp.LCHGT_NIDS_PRODUCT_CODE=        295
    pp.LCHGT_NIDS_HI_INDEX=            15
    pp.LCHGT_NIDS_LO_INDEX=            1
    pp.LCHGT_NIDS_MISSING_INDEX=       0
    pp.LCHGT_NIDS_VALUES_TAG=          'HEIGHT_VALUES:'
    pp.LCHGT_NIDS_COLORS_TAG=          'HEIGHT_COLORS:'
    pp.LCHGT_PRODUCT_LABEL=            'CMPHeight'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output HYBRID_REFLECT info                 //
!///////////////////////////////////////////////////////////////////////
    pp.HREF_QPESUMS_PRODUCT_CODE=       11
    pp.HREF_UNITS_CODE=                 1
    pp.HREF_SCALE=                      10.0
    pp.HREF_TILT_NUMBER=                0
    pp.HREF_TILT_ELEVATION=             0.0
    pp.HREF_NIDS_PRODUCT_CODE=          297
    pp.HREF_NIDS_HI_INDEX=              15
    pp.HREF_NIDS_LO_INDEX=              1
    pp.HREF_NIDS_MISSING_INDEX=         0
    pp.HREF_NIDS_VALUES_TAG=            'REFLECTIVITY_VALUES:'
    pp.HREF_NIDS_COLORS_TAG=            'REFLECTIVITY_COLORS:'
    pp.HREF_PRODUCT_LABEL=              'HybridReflectivity'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to output HYBRID_HEIGHT info                  //
!///////////////////////////////////////////////////////////////////////
    pp.HYBRID_HEIGHT_GATE_RANGE=        230  
    pp.HHGT_QPESUMS_PRODUCT_CODE=       11
    pp.HHGT_UNITS_CODE=                 1
    pp.HHGT_SCALE=                      10.0
    pp.HHGT_TILT_NUMBER=                0
    pp.HHGT_TILT_ELEVATION=             0.0
    pp.HHGT_NIDS_PRODUCT_CODE=          297
    pp.HHGT_NIDS_HI_INDEX=              15
    pp.HHGT_NIDS_LO_INDEX=              1
    pp.HHGT_NIDS_MISSING_INDEX=         0
    pp.HHGT_NIDS_VALUES_TAG=            'HYBHGT_VALUES:'
    pp.HHGT_NIDS_COLORS_TAG=            'HYBHGT_COLORS:'
    pp.HHGT_PRODUCT_LABEL=              'HybridHeight'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to subdirectories and file name tag           //
!///////////////////////////////////////////////////////////////////////
    pp.BREF_RAW_NIDS_SUB_DIR=  'BREF_RAW'
    pp.BREF_RAW_TAG=           'bref_raw'
    pp.BREF_NIDS_SUB_DIR=      'BREF'
    pp.BREF_TAG=               'bref'
    pp.BREF_NF_NIDS_SUB_DIR=   'BREF_FLT'
    pp.BREF_NF_TAG=            'bref_flt'
    pp.BVEL_DA_NIDS_SUB_DIR=   'BVEL_DA'
    pp.BVEL_DA_TAG=            'bvel_da'
    pp.BREF_QC_NIDS_SUB_DIR=   'BREF_QC'
    pp.BREF_QC_TAG=            'bref_qc'
    pp.CREF_NIDS_SUB_DIR=      'CREF'
    pp.CREF_TAG=               'cref'
    pp.CHGT_NIDS_SUB_DIR=      'CREF_HGT'
    pp.CHGT_TAG=               'cref_hgt'
    pp.CREF_QC_NIDS_SUB_DIR=   'CREF_QC'
    pp.CREF_QC_TAG=            'cref_qc'
    pp.CHGT_QC_NIDS_SUB_DIR=   'CREF_QC_HGT'
    pp.CHGT_QC_TAG=            'cref_qc_hgt'
    pp.LCREF_NIDS_SUB_DIR=     'LCREF'
    pp.LCREF_TAG=              'lcref'
    pp.LCHGT_NIDS_SUB_DIR=     'LCREF_HGT'
    pp.LCHGT_TAG=              'lcref_hgt'
    pp.LCREF_QC_NIDS_SUB_DIR=  'LCREF_QC'
    pp.LCREF_QC_TAG=           'lcref_QC'
    pp.LCHGT_QC_NIDS_SUB_DIR=  'LCREF_QC_HGT'
    pp.LCHGT_QC_TAG=           'lcref_QC_HGT'
    pp.HREF_NIDS_SUB_DIR=      'HREF'
    pp.HREF_TAG=               'href'
    pp.HHGT_NIDS_SUB_DIR=      'HREF_HGT'
    pp.HHGT_TAG=               'href_hgt'
    pp.HREF_QC_NIDS_SUB_DIR=   'HREF_QC'
    pp.HREF_QC_TAG=            'href_QC'
    pp.WFLAG_NIDS_SUB_DIR=     'WFLAG'
    pp.WFLAG_TAG=              'warmflag'
    pp.WRATE_NIDS_SUB_DIR=     'WRATE'
    pp.WRATE_TAG=              'warmrate'
    pp.FLAG_COOL_NIDS_SUB_DIR= 'FLAG'
    pp.FLAG_COOL_TAG=          'coolflag'
    pp.RATE_COOL_NIDS_SUB_DIR= 'RATE'
    pp.RATE_COOL_TAG=          'coolrate'
    pp.PCP_FLAG_NIDS_SUB_DIR=  'PCP_FLAG'
    pp.PCP_FLAG_TAG=           'flg'
    pp.PCP_RATE_NIDS_SUB_DIR=  'PCP_RATE'
    pp.PCP_RATE_TAG=           'rate'
    pp.BVEL_NIDS_SUB_DIR=      'BVEL'
    pp.BVEL_TAG=               'bvel'
    pp.BVEL_RAW_NIDS_SUB_DIR=  'BVEL_RAW'
    pp.BVEL_RAW_TAG=           'bvel_raw'
    pp.SPWI_NIDS_SUB_DIR=      'SPWI'
    pp.SPWI_TAG=               'spwi'
    pp.SPWI_RAW_NIDS_SUB_DIR=  'SPWI_RAW'
    pp.SPWI_RAW_TAG=           'spwi_raw'

!///////////////////////////////////////////////////////////////////////
!//  parameters related to AP and noise quality control               //
!///////////////////////////////////////////////////////////////////////
    pp.QC_OPT=     1
    pp.ZERO_OUT_OPT=     1
    pp.START_HGT=   2.5
    pp.WINTER_START_HGT=   3.0
    pp.START_HGT1=   3.0
    pp.WINTER_START_HGT1=   3.0
    pp.END_HGT=   0.5
    pp.WINTER_END_HGT=   0.5
    pp.GDZ_THRESHOLD= 15.0
    pp.TEXTURE_LOW_THRESHOLD= 15.0
    pp.TEXTURE_HIGH_THRESHOLD= 35.0
    pp.REF_THRESHOLD= 35.0
    pp.NOISE_FILTER_RATIO=   0.3    
    pp.NOISE_FILTER_SIZE=   5    
    pp.STRATIFORM_SEARCH_RADIUS= 20
    pp.DONUT_REF_THRESHOLD= 7.5
    pp.DONUT_NUM_THRESHOLD= 10

!///////////////////////////////////////////////////////////////////////
!//  parameters related to velocity dealiasing                        //
!///////////////////////////////////////////////////////////////////////
    pp.VELOCITY_DEALIASING_MODE=           1

!///////////////////////////////////////////////////////////////////////
!//  parameters related to synchronization                            //
!///////////////////////////////////////////////////////////////////////
    write(pp.RUCWINDFIELD_DIR,'(a,a)') trim(LOCAL_DIR),'/rucwind/'
    pp.sync_opt       = 0
    pp.RUCWINDFIELD_NX= 651
    pp.RUCWINDFIELD_NY= 301
    pp.RUCWINDFIELD_FREQ= 3600 
    pp.RUCWINDFIELD_WINDOW= 10800 

!///////////////////////////////////////////////////////////////////////
!//  parametrs for PRECIP_FLAG or convective_strtiform                //
!///////////////////////////////////////////////////////////////////////
    pp.HR_TYPE= 1
    pp.CON_STRAT_LOW_DBZ=  	 30.0
    pp.CON_STRAT_HIGH_DBZ=		 50.0
    pp.DEPTH_OF_GOOD_SNOW=		 1500.0
    pp.FLAG_QPESUMS_PRODUCT_CODE=   20
    pp.FLAG_UNITS_CODE=             0
    pp.FLAG_SCALE=                  1.0
    pp.FLAG_TILT_NUMBER=            0
    pp.FLAG_TILT_ELEVATION=         0.0
    pp.FLAG_NIDS_PRODUCT_CODE=      298
    pp.FLAG_NIDS_HI_INDEX=          15
    pp.FLAG_NIDS_LO_INDEX=          1
    pp.FLAG_NIDS_MISSING_INDEX=     0
    pp.FLAG_NIDS_VALUES_TAG=        'RAINFLAG_VALUES:'
    pp.FLAG_NIDS_COLORS_TAG=        'RAINFLAG_COLORS:'
    pp.GOOD_RAIN_FLAG=              1
    pp.BAD_RAIN_FLAG=               2
    pp.GOOD_SNOW_FLAG=              3
    pp.BAD_SNOW_FLAG=               4
    pp.CONVECTIVE_FLAG=             6
    pp.FLAG_PRODUCT_LABEL=          'PrecipFlag'

!///////////////////////////////////////////////////////////////////////
!//  parametrs for RAIN_RATE or different rain types                  //
!///////////////////////////////////////////////////////////////////////
    pp.RATE_ACONV_R=    		 300.0
    pp.RATE_BCONV_R=		 1.4
    pp.RATE_ASTRAT_R=		 200.0
    pp.RATE_BSTRAT_R=		 1.6
    pp.RATE_ASNOW_R=		 75.0
    pp.RATE_BSNOW_R=		 2.0
    pp.RATE_QPESUMS_PRODUCT_CODE=   21
    pp.RATE_UNITS_CODE=             3
    pp.RATE_SCALE=                  10.0
    pp.RATE_TILT_NUMBER=            0
    pp.RATE_TILT_ELEVATION=         0.0
    pp.RATE_NIDS_PRODUCT_CODE=      299
    pp.RATE_NIDS_HI_INDEX=          12
    pp.RATE_NIDS_LO_INDEX=          1
    pp.RATE_NIDS_MISSING_INDEX=     0
    pp.RATE_NIDS_VALUES_TAG=        'RAINRATE_VALUES:'
    pp.RATE_NIDS_COLORS_TAG=        'RAINRATE_COLORS:'
    pp.RATE_MISSING=                0.0
    pp.RATE_BASE=                   10
    pp.RATE_FRACTION=               0.1
    pp.RATE_PRODUCT_LABEL=          'PrecipRate'

!///////////////////////////////////////////////////////////////////////
!//  parameters for climatology                                       //
!/////////////////////////////////////////////////////////////////////// 
!    SEA_CLUTTER_PAR_FILE=          /mnt2/yqin/3d_polar/ref_data/
!    SEA_CLUTTER_PAR_FILE_TAG=      .seaclutter_par.dat
!    CLIMAT_FLAG=                   1
!    CLIMAT_INPUT_DIRECTORY=        /usr/local/qpesumsdev/data/QPESUMS/polar/binary/climatology_input/
!    CLIMAT_OUTPUT_DIRECTORY=       /usr/local/qpesumsdev/data/QPESUMS/polar/binary/climatology/
!    CLIMAT_NIDS_DIRECTORY=         /usr/local/qpesumsdev/data/QPESUMS/nids/
!    CLIMAT_BINARY_DIRECTORY=       /usr/local/qpesumsdev/data/QPESUMS/polar/binary/climatology/

!///////////////////////////////////////////////////////////////////////
!//  parameters for polar monitor file                                //
!///////////////////////////////////////////////////////////////////////
!    MONITOR_DIRECTORY=     /usr/local/qpesumsdev/data/QPESUMS/nids/    
!    POLAR_MONITOR_FILE_FLAG=   0

!//     ##################################################################
!//     ######       CONFIG PARAMETERS FOR REMAP                    ######
!//     ##################################################################

!///////////////////////////////////////////////////////////////////////
!//  Parameters related to input reference data
!//  input_nids_info=   NIDS products code and color tables
!///////////////////////////////////////////////////////////////////////
   write(pp.LUT_DIR,'(a,a)')trim(LOCAL_DIR),'/SRC_LUT/'
   write(pp.G_REFDATA_DIR,'(a,a)')trim(LOCAL_DIR), '/ref_data/'
   write(pp.P_REFDATA_DIR,'(a,a)')trim(LOCAL_DIR), '/ref_data/'

!///////////////////////////////////////////////////////////////////////
!// Options and directories for nids files output
!///////////////////////////////////////////////////////////////////////
!   NIDS_LREF_FLAG=      0
!   NIDS_UNQC_LREF_NAME_TAG=  src
!   NIDS_QC_LREF_NAME_TAG=  src
!   NIDS_SYNC_LREF_NAME_TAG=  src
!   NIDS_SYNC_QC_LREF_NAME_TAG=  src
!   NIDS_UNQC_LREF_DIR=       /usr/local/qpesumsdev/data/QPESUMS/nids/SRC3D
!   NIDS_QC_LREF_DIR=       /usr/local/qpesumsdev/data/QPESUMS/nids/SRC3D_QC
!   NIDS_SYNC_LREF_DIR=       /usr/local/qpesumsdev/data/QPESUMS/nids/SRC3D_SYNC
!   NIDS_SYNC_QC_LREF_DIR=       /usr/local/qpesumsdev/data/QPESUMS/nids/SRC3D_SYNC_QC

!///////////////////////////////////////////////////////////////////////
!//  options and directories for binariy files output
!///////////////////////////////////////////////////////////////////////
!   MOSAIC3D_BINARY_FLAG=   0
!   MOSAIC3D_UNQC_BINARY_NAME_TAG=  ps3d
!   MOSAIC3D_QC_BINARY_NAME_TAG=  ps3d
!   MOSAIC3D_SYNC_BINARY_NAME_TAG=  ps3d_sync
!   MOSAIC3D_SYNC_QC_BINARY_NAME_TAG=  ps3d_sync
!   MOSAIC3D_UNQC_BINARY_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D
!   MOSAIC3D_QC_BINARY_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_QC
!   MOSAIC3D_SYNC_BINARY_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_SYNC
!   MOSAIC3D_SYNC_QC_BINARY_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_SYNC_QC

!///////////////////////////////////////////////////////////////////////
!//  options and directories for multivariable netcdf files output
!///////////////////////////////////////////////////////////////////////
!   MOSAIC3D_NETCDF_FLAG=   1
!   MOSAIC3D_UNQC_NETCDF_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D
!   MOSAIC3D_QC_NETCDF_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_QC
!   MOSAIC3D_SYNC_NETCDF_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_SYNC
!   MOSAIC3D_SYNC_QC_NETCDF_DIR=    /usr/local/qpesumsdev/data/QPESUMS/polar/SRC3D_SYNC_QC

!///////////////////////////////////////////////////////////////////////
!//  Grid configuration parameters.
!//  dx       Grid spacing in x-direction (meters).
!//           E-W length of the analysis grid domain is dx*(nx-1)
!//  dy       Grid spacing in y-direction (meters).
!//           N-S length of the analysis grid domain is dy*(ny-1)
!//  ctrlat   Latitude of the model physical domain center (deg. N).
!//  ctrlon   Longitude of the model physical domain center (deg. E).
!///////////////////////////////////////////////////////////////////////
   pp.NX=  601
   pp.NY=  601
   pp.NZ=  31
   pp.DX=   0.01
   pp.DY=   0.01
   do i= 1, 11
   pp.zp(i) = 500+(i-1)*250
   enddo
   do i= 12, 23 
   pp.zp(i) = 3500 +(i-12)*500
   enddo
   do i= 24, 30
   pp.zp(i) = 10000+(i-24)*1000
   enddo
   pp.zp(31) = 18000.  


!///////////////////////////////////////////////////////////////////////
!//  Map projection parameters=
!//  mapproj   Map projection option.
!//          = 0, no map projection;
!//          = 1, polar projection;
!//          = 2, Lambert projection;
!//          = 3, Mercator projection.
!//          = 4, LAT/LON projection.
!///////////////////////////////////////////////////////////////////////
  pp.MAPPROJ=    4

!//////////////////////////////////////////////////////////////////////
!//  Options and parameters related to terrain specification.
!//
!//  ternopt  Model terrain option.
!//         = 0, no terrain, flat ground;
!//         = 1, terrain data read in from file terndta (defined later)
!//  terndta  Name of the terrain data file for ternopt=1.
!//           (Note= The terrain data is currently being produced
!//           using the 'arpstern' software, and it is read in by
!//           subroutine "READTRN".)
!///////////////////////////////////////////////////////////////////////
   pp.TERNOPT= 0
   pp.TERNDTA= 'ok1km_600x400_llmap.tern'

!///////////////////////////////////////////////////////////////////////
!// remap_opt = 0  nearest neighbor remapping - not recommended except for testing
!//                purposes.  Using dBZ for remapping.
!//             1  Using Z (mm6/m3) for interpolation
!//             2  Using dBZ for interpolation
!// delv_vcp_margin=  Tolarence criteria to difference between the nominal elevation
!//                   angles and the actual elevation angles for any VCPs.
!//                   If the difference exceeds "delv_vcp_margin1", a warning message will
!//                   be given; if the difference exceeds "delv_vcp_margin2", an error
!//                   message will be given and the program will skip the tilt.
!///////////////////////////////////////////////////////////////////////
   pp.REMAP_OPT=  1
   pp.DELV_VCP_MARGIN1=  0.1
   pp.DELV_VCP_MARGIN2=  0.25

   pp.LENGTH_SCALE_KM=  50
   pp.RADIUS_INFL_KM=  460
   pp.WGT_CUTOFF=  1.0e-30

!///////////////////////////////////////////////////////////////////////
!// Swap option for NIDS output
!//   swap_flag = 0,   will not do byteswap
!//               1,   will do byteswap
!///////////////////////////////////////////////////////////////////////
   pp.SWAP_FLAG=  1

!///////////////////////////////////////////////////////////////////////
!//  Minutes back for rerun executable
!///////////////////////////////////////////////////////////////////////
!    RERUN_BACK_MIN=  1200

!///////////////////////////////////////////////////////////////////////
!//  Parameters related to transfering mosaic nodes to nmqserver
!//  LDM_OPT=  1 for doing transfering
!//            0 for not doing transfering
!///////////////////////////////////////////////////////////////////////
!    LDM_NETCDF_OPT= 0
!    LDM_NIDS_OPT=   0
!    PQINSERT_FILE=  /home/ldm/bin/pqinsert
!    LDMD_LOG_FILE=  /home/ldm/logs/ldmd.log
!    LDM_PQ_FILE=    /home/ldm/data/ldm.pq

!///////////////////////////////////////////////////////////////////////
!//  parameters for polar monitor file                                //
!///////////////////////////////////////////////////////////////////////
!    MONITOR_FILE_OPT=          1
!    MONITOR_FILE_DIRECTORY=    /usr/local/qpesumsdev/data/QPESUMS/polar/monitor/

!//////////////////////////////////////////////////////////////////////
!//  END_OF_FILE                                                      //
!///////////////////////////////////////////////////////////////////////


    
       RETURN
       END SUBROUTINE get_config

