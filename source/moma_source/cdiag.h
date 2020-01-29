c====================== include file "cdiag.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     variables used for computing diagnostics:
c
c     ektot    = total kinetic energy per unit volume
c     tddt     = rate of change of tracer per unit volume
c     dtabs    = average per unit volume of modulus of rate of change of tracer 
c     tvar     = rate of change of tracer variance per unit volume
c
#endif
      _MOMA_REAL ektot, tddt(nt), dtabs(nt),tvar(nt)

      common /cdiag/ ektot, tddt, dtabs,tvar

