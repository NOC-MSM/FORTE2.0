OBJS =gwtlt.o   hessen.o  qreig.o   qrt.o     ranf.o

.f.o:
	$(FC) -c $(F77FLAGS) $*.f

libsgiutil1.a: $(OBJS)
	ar q ../../lib$(MACH)util.a $(OBJS)

clean:
	\rm -f $(OBJS) *.a
