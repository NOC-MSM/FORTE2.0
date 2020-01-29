c====================== include file "grdvar.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c  This version assumes all dx and dy boxes are of equal size when
c  measured in degrees.  Variables dxt, dxu etc replaced by dx, 
c  dyt,dyu etc by dy and dzt by dz.
c
c     dx     = longitudinal width of "t" grid box (in cm)
c     dxr    = reciprocal of "dxt"
c     dx2r   = reciprocal of "2*dxt"
c     dx4r   = reciprocal of "4*dxt"
c
c     dy     = latitudinal height of "t" grid box (in cm)
c     dyr    = reciprocal of "dyt"
c     dy2r   = reciprocal of "2*dyt"
c     dy4r   = reciprocal of "4*dyt"
c
c     csu     = cosine of "u,v" grid point latitude
c     csur    = reciprocal of "csu"
c     cst     = cosine of "t" grid point latitude
c     cstr    = reciprocal of "cst"
c     phi     = latitude of "u,v" grid point in radians
c     phit    = latitude of "t" grid point in radians
c     sine    = sine of "u,v" grid point latitude
c     tng     = tan of "u,v" grid point latitude
c     fcor    = 2*omega*sine(j)
c
c     dz(k)  = level thickness of "t" and "u,v" grid boxes (in cm)
c               (vertical separation between "w" velocity points)
c     c2dt(k)= "2*dzt"
c     dzr(k) = reciprocal of dzt
c     dz2r(k)= reciprocal of "2*dzt"
c     dz4r(k) = reciprocal of "4*dzt"
c     dzw(k)  = vertical separation between centers of levels k & k+1
c     dzwr(k) = reciprocal of dzw
c     dzw2r(k)= reciprocal of "2*dzw"
c     dzur(k)= upper diffusion grid factor = 1.0/(dzw(k-1)*dz(k))
c     dzlr(k)= lower diffusion grid factor = 1.0/(dzw(k)*dz(k))
c     dzwur(k)= upper diffusion grid factor = 1.0/(dz(k)*dzw(k))
c     dzwlr(k)= lower diffusion grid factor = 1.0/(dz(k+1)*dzw(k))
c     tanra(j)= tan(j)/radius on the "u,v" grid
c     dzsr(k) = reciprocal of sum of dz(k) and dz(k+1)
c
#endif
      _MOMA_REAL dx, dxr, dx2r, dx4r, dy, dyr, dy2r, dy4r,
     &                csu(jmt),  csur(jmt),  cst(jmt),   cstr(jmt),
     &                phi(jmt),  phit(jmt),  sine(jmt),  tng(jmt),
     &                fcor(jmt), tanra(jmt),
     &                dz(km),    dzr(km),    dz2r(km),   c2dz(km), 
     &                dzw(0:km), dzwr(0:km), dzw2r(0:km), dzsr(KM),
     &                dzur(km), dzlr(km),dz4r(km)

      common /grdvar/ dx, dxr, dx2r, dx4r, dy, dyr, dy2r, dy4r,
     &                csu,  csur,  cst,   cstr,
     &                phi,  phit,  sine,  tng,
     &                fcor, tanra,
     &                dz,    dzr,    dz2r,   c2dz, 
     &                dzw, dzwr, dzw2r, dzsr,
     &                dzur, dzlr, dz4r

