c====================== include file "levind.h" ========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     vertical level indicators which define model geometry & 
c     bottom topography:
c
c     kmt = number of vertical boxes over "t" points
c     kmu = number of vertical boxes over "u,v" points
c
#endif
      integer kmt(imt,jmt), kmu(imt,jmt), kmd(imt,jmt)
      common /levind/ kmt, kmu,kmd

