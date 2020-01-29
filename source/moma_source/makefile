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
# -w95 = suppress warnings about non standard fortran 95
#
FC  =  ifort -fpe0 -fpp -O3 -ipo -mp1 -qopenmp -r8 -i4
CC = icc -ip -openmp
LDR = ifort -fpe0 -fpp -O3 -ipo -mp1 -qopenmp -r8 -i4
FCFAST = ifort -fpe0 -fpp -O3 -ipo -mp1 -align -qopenmp -r8 -i4

#This is the version compatible with netcdf	
HDF_ROOT = $(HDF5_DIR)
HDF_INC_DIR = $(HDF_ROOT)/include
HDF_LIB_DIR = $(HDF_ROOT)/lib

NC_ROOT    = $(NETCDF_DIR)
NC_INC_DIR = $(NC_ROOT)/include
NC_LIB_DIR = $(NC_ROOT)/lib

#PVM_ROOT=
PVM_ARCH=LINUX64
PVM_INC_DIR = $(PVM_ROOT)/include/$(PVM_ARCH)
PVM_LIB_DIR = $(PVM_ROOT)/lib/$(PVM_ARCH)

SYSLIBS = -Vaxlib

#DOPTNS =  -Daverage -Dcyclic -Dde_checkbd -Dde_checkbd_T -Dflux_write -Dgent_mcwilliams -Dgm_diag -Dfull_griffies -Dhalo2 -Disopycmix -Dmsq -Dnetcdf -DNONLIN_FREE_SURFACE -DREAL_8 -Doasis -Dw_write -Doasis_infl -Dc_write -Dnflux_write -Drho_is_write -Dtendency_write -Dcmip_flx -Dperturbation -Dcmip_flxrd
#DOPTNS =  -Daverage -Dcyclic -Dde_checkbd -Dde_checkbd_T -Dflux_write -Dgent_mcwilliams -Dgm_diag -Dfull_griffies -Dhalo2 -Disopycmix -Dmsq -Dnetcdf -DNONLIN_FREE_SURFACE -DREAL_8 -Doasis -Doasis_infl -Dw_write -Dcmip_flx -Dsst_wr
DOPTNS =  -Daverage -Dcyclic -Dde_checkbd -Dde_checkbd_T -Dflux_write -Dgent_mcwilliams -Dgm_diag -Dfull_griffies -Dhalo2 -Disopycmix -Dmsq -Dnetcdf -DNONLIN_FREE_SURFACE -DREAL_8 -Doasis -Doasis_infl -Dw_write
#DOPTNS =  -Daverage -Dcyclic -Dde_checkbd -Dde_checkbd_T -Dflux_write -Dgent_mcwilliams -Dgm_diag -Dfull_griffies -Dhalo2 -Disopycmix -Dmsq -Dnetcdf -DNONLIN_FREE_SURFACE -DREAL_8 -Dzero_fluxes

#LIBS =           -lnetcdf -lhdf5_hl -lhdf5 -loasis -lfpvm3 -lgpvm3 -lpvm3
LIBS =  -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -loasis -lfpvm3 -lgpvm3 -lpvm3
ADD_FS =  average.F invtri.F  isopyc.F netcdf_moma.F

#Makefile - main section

#Note - ORDER here is important - netcdf before hdf.
INCLUDE = -I$(NC_INC_DIR) -I$(HDF_INC_DIR) -I$(PVM_INC_DIR)
LIBDIRS = -L$(NC_LIB_DIR) -L$(HDF_LIB_DIR) -L$(PVM_LIB_DIR) -LClim

FFILES = addv.F clinic.F   grids.F     pdiag.F  setvbc.F  tmngr.F \
    archrd.F     diag.F     indp.F  read_kmt.F    snap.F tmnset.F \
    archwr.F   docmnt.F   matrix.F    restio.F   state.F  topog.F \
     bcest.F     moma.F   setkmt.F      step.F  tracer.F \
    checks.F    frees.F   ocn1st.F      setp.F  theta0.F \
    stream.F   $(ADD_FS)

# Name of this file:
MAKEFILE = makefile
# Name of final executable
EXEC = moma

OFILES = $(FFILES:.F=.o)
.SUFFIXES:
.SUFFIXES: .o .F

#suppress SCCS retrieval
.SCCS_GET:

#default target:
all : $(EXEC)

.F.o : 
	$(FC) $(DOPTNS) $(INCLUDE) -c $<

$(EXEC) :  $(OFILES) $(MAKEFILE)
	$(LDR) $(LIBDIRS) -o $(EXEC) $(OFILES) $(LIBS)

#fast - compiles all files at once (this takes time but allows cross-file
# optimisations to be done)
fast    : $(FFILES) $(MAKEFILE)
	$(FCFAST) $(DOPTNS) $(INCLUDE) $(LIBDIRS) $(LIBS) -o $(EXEC) $(FFILES) Clim/*.o

clean :	
	rm -f *.o *.list

cleaner : 
	rm -f moma *.o *.list

#Create dependencies:
depend :
	makedepend  $(DOPTNS) $(INCLUDE) -f $(MAKEFILE) $(FFILES)

# If the makefile changes - rebuild all
$(OFILES) : $(MAKEFILE)

# Dependency information included by make depend after here:
# DO NOT DELETE

addv.o: cpp_options.h param.h pconst.h frees.h levind.h timelv.h slabs.h
addv.o: grdvar.h coord.h scalar.h
clinic.o: cpp_options.h param.h pconst.h scalar.h switch.h coord.h grdvar.h
clinic.o: levind.h timelv.h slabs.h frees.h chmix.h cvbc.h cvmix.h
grids.o: cpp_options.h param.h pconst.h scalar.h coord.h grdvar.h
pdiag.o: cpp_options.h param.h pconst.h coord.h ctmngr.h frees.h timelv.h
pdiag.o: slabs.h
setvbc.o: cpp_options.h param.h pconst.h cvbc.h grdvar.h scalar.h slabs.h
setvbc.o: timelv.h levind.h ctmngr.h coord.h frees.h oasis.h
tmngr.o: cpp_options.h param.h pconst.h ctmngr.h switch.h timelv.h
archrd.o: cpp_options.h iounit.h pconst.h
diag.o: cpp_options.h param.h pconst.h scalar.h switch.h grdvar.h levind.h
diag.o: timelv.h slabs.h cdiag.h
indp.o: cpp_options.h
read_kmt.o: cpp_options.h param.h pconst.h coord.h iounit.h levind.h switch.h
snap.o: cpp_options.h iounit.h pconst.h
tmnset.o: cpp_options.h pconst.h
archwr.o: cpp_options.h iounit.h pconst.h
docmnt.o: cpp_options.h param.h pconst.h switch.h versno.h
matrix.o: cpp_options.h param.h pconst.h
restio.o: cpp_options.h param.h pconst.h ctmngr.h frees.h iounit.h levind.h
restio.o: slabs.h switch.h timelv.h
state.o: cpp_options.h param.h pconst.h dncoef.h
topog.o: cpp_options.h param.h pconst.h coord.h grdvar.h levind.h
bcest.o: cpp_options.h param.h pconst.h coord.h
moma.o: cpp_options.h param.h pconst.h scalar.h switch.h coord.h ctmngr.h average.h
moma.o: grdvar.h levind.h timelv.h slabs.h cdiag.h chmix.h cvbc.h cvmix.h
moma.o: frees.h iounit.h versno.h oasis.h
setkmt.o: cpp_options.h param.h pconst.h coord.h levind.h
step.o: cpp_options.h param.h pconst.h scalar.h switch.h ctmngr.h timelv.h
step.o: slabs.h frees.h cdiag.h
tracer.o: cpp_options.h param.h pconst.h scalar.h grdvar.h levind.h timelv.h
tracer.o: slabs.h chmix.h cvbc.h cvmix.h frees.h
checks.o: cpp_options.h param.h pconst.h scalar.h switch.h iounit.h
frees.o: cpp_options.h param.h pconst.h chmix.h ctmngr.h frees.h grdvar.h
frees.o: levind.h scalar.h switch.h timelv.h
ocn1st.o: cpp_options.h param.h pconst.h coord.h ctmngr.h cvbc.h frees.h
ocn1st.o: grdvar.h iounit.h levind.h scalar.h slabs.h switch.h
setp.o: cpp_options.h
theta0.o: cpp_options.h
stream.o: cpp_options.h param.h pconst.h ctmngr.h frees.h iounit.h levind.h
stream.o: slabs.h switch.h timelv.h grdvar.h coord.h scalar.h
average.o: cpp_options.h param.h pconst.h average.h timelv.h slabs.h frees.h
average.o: cvbc.h ctmngr.h netcdf_moma.h
invtri.o: cpp_options.h param.h pconst.h grdvar.h chmix.h levind.h frees.h
isopyc.o: cpp_options.h param.h pconst.h levind.h grdvar.h timelv.h slabs.h
isopyc.o: scalar.h iounit.h coord.h dncoef.h chmix.h
netcdf_moma.o: cpp_options.h param.h pconst.h ctmngr.h netcdf_moma.h slabs.h
netcdf_moma.o: timelv.h frees.h levind.h iounit.h cvbc.h
netcdf_moma.o: $(NC_INC_DIR)/netcdf.inc
netcdf_moma.o: coord.h
