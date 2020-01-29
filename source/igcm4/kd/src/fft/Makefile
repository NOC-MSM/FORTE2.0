OBJS = fax.o fft991.o fftrig.o qpassm.o rpassm.o set99.o

.f.o:
	$(FC) -c $(F77FLAGS) $*.f


libsgiblas1.a: $(OBJS)
	ar q ../../lib$(MACH)fft.a $(OBJS)

clean:
	\rm -f $(OBJS) *.a
