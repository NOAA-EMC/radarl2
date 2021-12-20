
LOCAL_LIBRARIES =  -L ./include
INCLUDES = -I ./include  
SYS_LIBRARIES = -lm -lz 

.SUFFIXES : .cc .h


              #CXX = CC
              CXX = icc
              # CXX =  /usr/vacpp/bin/xlC
      CXXINCLUDES = $(INCLUDES)
    CXXDEBUGFLAGS =  
       #CXXOPTIONS =  -LANG:std -O2 
       #CXXOPTIONS = -g   -q64 -O3 -bmaxdata:0x80000000
       CXXOPTIONS =  -O3 
         CXXFLAGS = $(CXXDEBUGFLAGS) $(CXXOPTIONS) $(CXXINCLUDES)

               MV = mv
               CP = cp
               RM = rm -f

SHARED_SRCS=\
 read_RUC_tsfc.cc\
 get_current_vcp.cc\
 CONFIG_PARS.cc\
 setgrd.cc\
 maplib3d.cc\
 radarlib.cc\
 readtrn.cc\
 chktime.cc\
 get_time_realtime.cc\
 get_attr_realtime.cc\
 radar_sum.cc\
 grid_vars.cc\
 get_t_z_snd.cc\
 i2wrt_cart3d.cc\
 i2read_cart3d_conus.cc\
 read_sat_qc_mask.cc\
 qpesums_utility.cc\
 produce_CREF_CREFH_new.cc\
 produce_ETP_new.cc\
 produce_HDA_new.cc\
 produce_HSR_and_HSRH_new.cc\
 produce_VIL_and_VILD_new.cc\
 produce_LCREF_and_LCREFH_new.cc\
 produce_PCPFLAG_new.cc\
 produce_CREF_CREFH.cc\
 produce_ETP.cc\
 produce_HDA.cc\
 produce_HSR_and_HSRH.cc\
 produce_VIL_and_VILD.cc\
 produce_LCREF_and_LCREFH.cc\
 produce_PCPFLAG.cc\
 get_index_temp.cc\
 produce_2D_products.cc\
 output_mosaic_info.cc\
 FuncSet.cc\
 SOUNDING.cc\
 NIDS_INFO.cc
 


MOS_R_SRCS=\
 src_mosaic_main.cc

SHARED_OBJS=${SHARED_SRCS:.cc=.o}

MOS_R_OBJS=${MOS_R_SRCS:.cc=.o} $(SHARED_OBJS)

PROGRAMS=radar_reflectivity_mosaic

all:: $(PROGRAMS)

radar_reflectivity_mosaic: $(MOS_R_OBJS)
	$(RM) $@
	$(CXX) -o $@ $(CXXFLAGS) $(MOS_R_OBJS) $(LOCAL_LIBRARIES) $(SYS_LIBRARIES)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<


clean::
	$(RM) *.o core
