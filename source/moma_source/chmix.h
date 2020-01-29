c====================== include file "chmix.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     horizontal mixing coefficients
c
c     am  = lateral eddy viscosity (cm**2/sec)
c     ah  = lateral eddy diffusivity (cm**2/sec)
c     aq  = lateral turbulent kinetic energy diffusivity 
c           (cm**2/sec)
c     aidif = fraction of diffusion done implicitly (0.0 to 1.0)
c       slmx   = max slope of isopycnals
c       ahisop = isopycnal tracer diffusivity(cm**2/sec)
c       athkdf = isopycnal thickness diffusivity (cm**2/sec)
c       ahsteep= steep slope horizontal diffusivity(cm**2/sec)
c       fzisop = vertical structure function
#endif
      _MOMA_REAL am, ah,
     &        bbu(jmt), ccu(jmt), ddu(jmt), ggu(jmt), hhu(jmt),
     &        bbt(jmt), cct(jmt), ddt(jmt)
      common /chmix/ am, ah, 
     &        bbu, ccu, ddu, ggu, hhu,
     &        bbt, cct, ddt

#ifdef isopycmix
      _MOMA_REAL 
     &        slmx, ahisop, athkdf, ahsteep
     &        ,aidif
#ifdef pkimix
     &        ,psi1,psi2,A_pki
#endif

      common /chmix_iso/ 
     &        slmx, ahisop, athkdf, ahsteep
     &        ,aidif
#ifdef pkimix
     &        ,psi1,psi2,A_pki
#endif
#endif /* isopycmix */

