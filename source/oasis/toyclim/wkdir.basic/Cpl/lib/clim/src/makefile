
# Makefile CLIM library
# ---------------------
#
CMD	=	clim.$(PVM_ARCH).a
#
OBJLIST	=	CLIM_Abort.o    CLIM_Define.o    CLIM_Hostdt.o    CLIM_Quit.o\
                CLIM_Time.o     CLIM_Defport.o   CLIM_Import.o    CLIM_Reset.o\
                CLIM_Trace.o    CLIM_Delta.o     CLIM_Init.o      CLIM_Start.o\
                CLIM_Wait.o     CLIM_Export.o    CLIM_Nodedt.o    CLIM_Stepi.o\
		wallclk.o
#
# include specific ARCH declarations
#
include ../arch/maki.$(PVM_ARCH)
#
#
.SUFFIXES:	.F .c .o
#
all:	$(CMD)
#

$(CMD): $(OBJS)
	ar cr $(CMD) $(OBJS)
	$(RANLIB) $(CMD)
	\mv $(CMD) ../lib

.F.o:
	$(FORTRAN) $(FFLAGS) -I../include -D$(PVM_ARCH) -c $*.F

.c.o:
	$(CPP) -I../include -D$(PVM_ARCH) $*.c
	$(CC) $(CFLAGS) -c $*.i

clean:
	\rm -f *.o *.i
	\rm -f $(CMD) 
