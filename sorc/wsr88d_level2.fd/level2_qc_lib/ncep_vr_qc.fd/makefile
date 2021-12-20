
OBJS1 = variable_define.o CONFIG_PARS.o \
        get_config.o qcmain.o readraw.o datasort.o indexx.o xytorth.o \
        slctbeam.o num_sign.o std_dev.o \
        azimcontr.o num_signavg.o num_signavg1.o \
        setapecho.o var.o \
        gatecontr.o dpqc.o get_k121.o\
	convert2.o convert4.o \
	convert2_two.o convert4_two.o dumpvar.o \
	get_mark.o Pre_Data_Anaylsis.o qc_nonm.o \
\
nssl_qcmain.o NoiseFltr.o RemvPattern.o SunBeamFltr.o \
\
variable_define.o dealiase_main.o \
setup_radar.o prep_radar.o orig_radar.o \
setup_noise_remove.o noise_rm.o area0vr_rm.o \
setup_circle.o \
setup_wind_profile.o tendency_method.o gradient_fst_0Vr.o \
tendency_0Vr.o line_regression.o \
cost_function.o search_min.o get_u0v0.o \
get_residual.o decide_first_tilt.o \
profile_continue_qc.o profile_differ_qc.o full_profile.o \
decide_vadflag.o sort2.o \
multi_check_driver.o cal_vadvel.o \
reference_check.o dealias.o vadtilt_check.o \
continue_check.o unfold_simple.o filling_spval.o \
extr_dealiase_main.o ex_setup_radar.o ex_prep_radar.o \
radarlib3d.o maproj3d.o classical_VAD.o VADLSF.o \
vad2bufr.o pbl2bufr.o

F90	= ifort -nofor-main
RM	= /bin/rm -f
FFLAGSf = -c -O3 -g
FFLAGSF = -c -O3 -g
LFLAGS = -O3 -g -o

#LIBNSSL = ../../nssl_reflec_qc_lib.a
#LIBSRC  = ../../nssl_reflec_src_lib.a
#LIBunfold=
#LIBS    = -L/nwprod/lib -lw3emc_4 -lw3nco_4
#LIBSJ   = -L/nwprod/lib -lbufr_v10.2.3_4_64 

LIB	= ../../ncep_vr_qc_lib.a

ALL:: $(LIB)
$(LIB): $(OBJS1)
	ar -r -v $(LIB) $(OBJS1)


.SUFFIXES:
.SUFFIXES: .o .f .f90 

.f90.o:
	$(F90) $(FFLAGSf) $<
.f.o:
	$(F90) $(FFLAGSF) $<

clean:
	$(RM) $(EXEC) $(OBJS1) *.mod

clean2:
	$(RM) $(EXEC)
