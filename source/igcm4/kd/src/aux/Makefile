
OBJS = ismax.o ismin.o lensig.o second.o ssum.o

.f.o:
	$(FC) -c $(F77FLAGS) $*.f

libsgiaux1.a: $(OBJS)
	ar q ../../lib$(MACH)aux.a $(OBJS)

clean:
	\rm -f $(OBJS) *.a
