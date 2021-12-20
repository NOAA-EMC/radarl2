
OBJS1 = CONFIG_PARS.o reflectivity.o mosaic_adapt_pars.o PHYCST.o BRIGHTBAND.o RADAR2.o \
        maplib3d.o  compare_tilt_lut.o  dataSelect.o \
        get_radattr_realtime.o     ini_grd.o \
        read_RUCWindField.o    readTiltData.o    readtrn_func.o\
        set_grd.o     src.o   get_brightband.o  alignment.o
F90	= ifort -nofor-main
RM	= /bin/rm -f
FFLAGSf = -c -O3 -g
FFLAGSF = -c -O3 -g
ARFLAGS = -r -v
AR  = ar
LIB     = ../../nssl_reflec_src_lib.a
FOBS=$(LIB)($(OBJS1))

ALL:: $(LIB)

$(LIB): $(FOBS)

.SUFFIXES:
.SUFFIXES: .a .o .f .f90

.f90.o:
	$(F90) $(FFLAGSf) $<
.f.o:
	$(F90) $(FFLAGSF) $<
.o.a:
	$(AR) $(ARFLAGS) $@ $<

clean::
	$(RM) $(OBJS1) *.mod 
