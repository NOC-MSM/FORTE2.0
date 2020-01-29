#
#
#     Moma make file for linux with intel compiler
#
#

# Compiler options:
# -O2 =  Common optimizations
# -u = Assume Implicit none
# -g = Add debug information 
# -r8 = Promote REAL variables and constants to DOUBLE PRECISION
# -i4 Makes integer = integer*4
# -mp = Numerical conformance to IEEE standard
# -CB = Check array bounds
# -CU = Check for uninitialised variables
# -tpp5 , -tpp6 , -tpp7 (target to Pentium , Pro/II/III , P4 ) 
# -w95 = suppress warnings about non standard fortran 95
#

FC  = ifort -qopenmp -fpe0 -fpp -O3 -ip -mp1 -align -auto -r8 -i4
CC = icc -ipo
LDR = ifort -fpe0 -fpp -O3 -ipo -mp1 -align -auto -r8 -i4
FCFAST = ifort -qopenmp -fpe0 -fpp -O3 -ip -mp1 -align -auto -r8 -i4

#This is the version compatible with netcdf	
HDF_ROOT = $(HDF5_DIR)
HDF_INC_DIR = $(HDF_ROOT)/include
HDF_LIB_DIR = $(HDF_ROOT)/lib

NC_ROOT    = $(NETCDF_DIR)
NC_INC_DIR = $(NC_ROOT)/include
NC_LIB_DIR = $(NC_ROOT)/lib

PVM_ROOT=../../pvm3
PVM_ARCH=LINUX64

INCLUDE = -I$(NC_INC_DIR) -I$(HDF_INC_DIR)
LIBDIRS = -L$(NC_LIB_DIR) -L$(HDF_LIB_DIR)

SYSLIBS = -Vaxlib

PVM_INC_DIR = $(PVM_ROOT)/include/$(PVM_ARCH)
PVM_LIB_DIR = $(PVM_ROOT)/lib/$(PVM_ARCH)

MPATH = ..
PVM_INC_DIR = $(PVM_ROOT)/include/$(PVM_ARCH)
INCLUDE = -I$(PVM_INC_DIR) -I$(MPATH)

FFILES =  tau.F flx.F stpcmo.F inicmo.F locwrite.F locread.F halte.F \
          CLIM_Abort.F CLIM_Define.F CLIM_Defport.F CLIM_Delta.F \
          CLIM_Export.F CLIM_Fish.F CLIM_Hostdt.F CLIM_Import.F \
          CLIM_Init.F CLIM_Nodedt.F CLIM_Quit.F CLIM_Reset.F \
          CLIM_Start.F CLIM_Stepi.F CLIM_Time.F CLIM_Trace.F CLIM_Wait.F
CFILES =  wallclk.c


OFILES = $(FFILES:.F=.o) $(CFILES:.c=.o)
.SUFFIXES:
.SUFFIXES: .o .F .c

#suppress SCCS retrieval
.SCCS_GET:

#default target:
.F.o : 
	$(FC) $(DOPTNS) $(INCLUDE) -c $<

.c.o:
	$(CC) -c $(CFILES)

liboasis.a :  $(OFILES)
	xiar q  liboasis.a $(OFILES)

clean :	
	rm -f *.o *.a
