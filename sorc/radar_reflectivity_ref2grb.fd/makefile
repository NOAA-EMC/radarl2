SHELL=/bin/sh

SRCS1 = radar_reflectivity_ref2grb.f90

OBJS1 = radar_reflectivity_ref2grb.o

F90     = ftn
RM      = /bin/rm -f
CMD =    radar_reflectivity_ref2grb
FFLAGSf = -c -O2
FFLAGSF = -c -O2
LFLAGS =  -o

#LIBS= -L/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4

LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}
LIBS1 = ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
#LIBS1 = -L/nwprod/lib -lg2_4 -ljasper -lpng -lz

all: $(CMD)

$(CMD):   $(OBJS1)
 
	$(F90) -o $(@) $(OBJS1) $(LIBS1) $(LIBS)

.SUFFIXES:
.SUFFIXES: .o .f .f90

.f90.o:
	$(F90) $(FFLAGSf) $<
.f.o:
	$(F90) $(FFLAGSF) $<


clean:
	rm -f $(CMD) $(OBJS1)

install:
	mv $(CMD) ../../exec/
