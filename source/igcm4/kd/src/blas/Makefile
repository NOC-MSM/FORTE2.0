OBJS = cdotc.o   icamax.o  isamax.o  scnrm2.o  sgemm.o   sger.o    sgetrf.o  slaswp.o  sswap.o   strmv.o   strti2.o  xerbla.o cdotu.o   ilaenv.o  lsame.o   sdot.o    sgemv.o   sgetf2.o  sgetri.o  sscal.o   strmm.o   strsm.o   strtri.o

.f.o:
	$(FC) -c $(F77FLAGS) $*.f

libsgiblas1.a: $(OBJS)
	ar q ../../lib$(MACH)blas.a $(OBJS)

clean:
	\rm -f $(OBJS) *.a
