# ------------------------ FILE: MAKEFILE ------------------------





POBJS=update.o proc_dir.o utils.o proc_pl.o proc_if.o proc_ibd.o strfns.o
MOBJS=mklib.o utils.o strfns.o
ROBJS=rsort.o utils.o strfns.o
INCLUDE=defns.h structs.h


# Uncomment the following to use alternative string compare functions
#STRCMP=-DALT_STRCMP
STRCMP=


# for the GNU C compiler
CFLAGS= -O -DSAVE_LOG -DDEBUG
CC=cc
#CFLAGS= -O -DSAVE_LOG $(STRCMP)
LINK=cc
LFLAGS=


all: nupdate mklib rsort

nupdate : $(POBJS) $(INCLUDE)
	$(LINK) $(POBJS) -o nupdate

mklib : $(MOBJS) $(INCLUDE)
	$(LINK) $(MOBJS) -o mklib

rsort : $(ROBJS) $(INCLUDE)
	$(LINK) $(ROBJS) -o rsort

utils.o : utils.c $(INCLUDE)

update.o : update.c $(INCLUDE)

proc_pl.o : proc_pl.c $(INCLUDE)

proc_dir.o : proc_dir.c $(INCLUDE)

proc_if.o : proc_if.c $(INCLUDE)

proc_ibd.o : proc_ibd.c $(INCLUDE)

mklib.o : mklib.c $(INCLUDE)

strfns.o : strfns.c



clean :
	rm -f *.o
	rm -f nupdate mklib rsort
	rm -f core
