# 
# Makefile for the Level II Radar Ingest
#

HOME_sorc=./level2_qc_lib
HOME_exec=.

# Objects
COBJ  = wsr88d.o combine_radials.o message31_decoder.o orpggdr.o le_logerr.o misc_table.o pbd_process_data.o str.o recomb_dp_fields.o  message31sw.o get_memory.o
FOBJ  = CONFIG_PARS.o build8_qc_bufr.o build10_recomb.o build10_superres.o
OBJS = $(COBJ) $(FOBJ)
SRCC = wsr88d.c combine_radials.c message31_decoder.c orpggdr.c pbd_process_header.c le_logerr.c misc_table.c pbd_process_data.c str.c message31sw.c get_memory.c
SRCF = CONFIG_PARS.f90 build8_qc_bufr.f90 build10_recomb.f90 build10_superres.f90

# Libraries
NSSL_LIB = $(HOME_exec)/nssl_reflec_src_lib.a
LIBvrqc = $(HOME_exec)/ncep_vr_qc_lib.a
LIBbz2 = $(GEMOLB)/libbz2.a
PRD_LIBS = $(BUFR_LIB4) $(LIBbz2)
#PRD_LIBS = -L/nwprod/lib -lbufr_v10.2.3_4_64 -L/nwprod/gempak/nawips1/os/linux2.6.32_x86_64/lib -lbz2 
LIBS = $(LIBvrqc) $(NSSL_LIB) $(PRD_LIBS)

# Flags
CC  = icc
F90 = ifort
RM  = /bin/rm -f

CFLAGS_base = -c -I /gpfs/dell1/nco/ops/nwprod/gempak.v7.3.1/nawips/os/linux3.10.0_x86_64/include -I ../include 
CFLAGS_normal = -DUNDERSCORE -g -O2
CFLAGS_debug = -g -traceback -DUNDERSCORE  -i4

FFLAGSf_base = -c -I $(HOME_sorc)/ncep_vr_qc.fd 
FFLAGSf_normal = -O3 -g -nofor-main -i4
FFLAGSf_debug = -g -traceback -nofor-main -i4 

LFLAGS_base = -nofor-main
LFLAGS_normal = -O3 -g
LFLAGS_debug = -g -traceback -i4

# Executable
EXEC = wsr88d_level2
#EXEC = ../v_final_test/wsr88d_level2
NOP3 = nop3

# Start of target definitions
normal: 
	sh -c "export FFLAGSf='$(FFLAGSf_base) $(FFLAGSf_normal)'; \
	    export FFLAGSF='$(FFLAGSF_base) $(FFLAGSF_normal)'; \
	    export CFLAGS='$(CFLAGS_base) $(CFLAGS_normal)'; \
	    export LFLAGS='$(LFLAGS_base) $(LFLAGS_normal)'; \
	    make $(EXEC)"
debug: 
	sh -c "export FFLAGSf='$(FFLAGSf_base) $(FFLAGSf_debug)'; \
	    export FFLAGSF='$(FFLAGSF_base) $(FFLAGSF_debug)'; \
	    export CFLAGS='$(CFLAGS_base) $(CFLAGS_debug)'; \
	    export LFLAGS='$(LFLAGS_base) $(LFLAGS_debug)'; \
	    make $(EXEC)"

$(EXEC): $(OBJS) $(LIBvrqc) $(NSSL_LIB) Makefile
	$(F90) $(LFLAGS) -o $@ $(OBJS) $(LIBS)

$(NOP3).o: nop3.c Makefile
	icc -O3 -g -maix64 -c -o $@ nop3.c
$(NOP3): $(NOP3).o
	icc -O3 -g -maix64 -o $@ nop3.o -lC

clean:
	$(RM) $(EXEC) $(OBJS)

realclean: clean
	$(RM) MAP LMAP core

.SUFFIXES:
.SUFFIXES: .o .c .f .f90 

.c.o:
	$(CC) $(CFLAGS) $<

.f90.o:
	$(F90) $(FFLAGSf) $<
.f.o:
	$(F90) $(FFLAGSF) $<
