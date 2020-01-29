c====================== include file "slabs.h" =========================
#ifdef hcomments
c23456789012345678901234567890123456789012345678901234567890123456789012
c=======================================================================
c
c     %Z% SCCS module: %M%, version %I%
c
c     This defines the basic array storage scheme.  In the MOM
c     model the data is stored on disk in slabs (hence the name).
c     For array processors it is stored in large arrays distributed
c     over the different processors
c
c     time levels defined in timelv.h
c
c     nm     = points to time level itt-2
c     nc     = points to current time level itt-1
c     np     = points to time level itt
c
c     the slab data is:
c
c     t = tracer quantites. the 1st is temperature & 
c                           the 2nd is salinity
c         if nt > 2 then other tracers are allowed.
c     u = zonal component of velocity.
c     v = meridional component of velocity.
c
c     (note: only the baroclinic component of the velocity is
c      stored on the slab disks. in memory, the barotropic
c      component is added on to give the full velocity)
c
#endif
       _MOMA_REAL t(km,imt,jmt,nt,3),
     &                u(km,imt,jmt,3), v(km,imt,jmt,3)
#ifdef tendency_write
     & ,tend_adv(km,imt,jmt,nt),tend_hdiff(km,imt,jmt,nt)
     & ,tend_vdiff(0:km,imt,jmt,nt)
#endif
#ifdef nflux_write
     & ,nflux(km,imt,jmt,nt),tracerbit
#endif
#ifdef rho_is_write
     & ,rho_is(km,imt,jmt)  !Time average in situ density (kg/m^3)
#endif
#ifdef w_write
     & ,w(0:km,imt,jmt)
#endif
#ifdef c_write
     & ,cs(km,imt,jmt)
#endif
      common /slabs/ t,u,v
#ifdef tendency_write
     & ,tend_adv,tend_hdiff,tend_vdiff
#endif
#ifdef nflux_write
     & ,nflux,tracerbit
#endif
#ifdef rho_is_write
     & ,rho_is  !Time average in situ density (kg/m^3)
#endif
#ifdef w_write
     & ,w
#endif
#ifdef c_write
     & ,cs
#endif
#ifdef conv_write
      integer conv(km,imt,jmt)
      common /slabs_i/ conv
#endif
#ifdef hcomments
c
c  the working arrays are
c 
c     uclin  = internal mode u (used for advective fluxes &
c               diagnostics)
c     vclin  = internal mode v 
c     rhoo   = density at (i  ,j  ) on the "t" grid
c     rhpo   = density at (i+1,j  ) on the "t" grid
c     rhop   = density at (i  ,j+1) on the "t" grid
c     rhpp   = density at (i+1,j+1) on the "t" grid
c     dpdx   = zonal gradient of pressure on "u,v" grid
c     dpdy   = meridional gradient of pressure on "u,v" grid
c     fue    = advective coeff for eastern face of "u,v" grid box
c              in "clinic" & "t" grid box in "tracer"
c     fuw    = advective coeff for western face of grid box
c     fvn    = advective coeff for northern face of grid box
c     fvs    = advective coeff for southern face of grid box
c     fm     = (0,1) over "t" grid (land,ocean) points
c     gm     = (0,1) over "u,v" grid (land,ocean) points
c     vmf    = array (1) used for vertical differences of u
c     vmf    = array (2) used for vertical differences of v
c     vtf    = array used for vertical tracer flux
c     fw     = vertical velocity defined at the bottom of "u,v" 
c              boxes in "clinic" and "t" boxes in "tracer"
c     fwb1   = w * (quantity 1) defined at the bottoms of the boxes
c     fwb2   = w * (quantity 2) defined at the bottoms of the boxes
c     restr  = term for restoring surface tracers to prescribed 
c              values via newtonain damping
c     rests  = time scale for restoring surface tracers (days)
c
#ifdef gm_diag
c       adv_vetiso = isopycnal advective vel on east face of "T" cell
c       adv_vntiso = isopycnal advective vel on north face of "T" cell
c               (Note: this includes the cosine factor as in "adv_vnt")
c       adv_vbtiso = isopycnal advective vel on bottom face of "T" cell
#endif
#endif

       _MOMA_REAL 
     &     dpdx(km),  dpdy(km),
     &     fue(km), fuw(km),   fvn(km),  fvs(km),
     &     vmf(0:km,2),   vtf(0:km),
     &     fw(0:km),  fwb1(0:km), fwb2(0:km), temp (km),
     &     rmskn(km), rmsks(km), rmske(km), rmskw(km),
     &     T_up(KM), Diff_msqUp(0:km)
     &     ,rhoo(km),   rhpo(km),   rhpp(km),   rhop(km)
#ifdef msq
      _MOMA_REAL 
     &     rmsknn(km), rmskss(km), rmskee(km), rmskww(km)
     &     ,rmsknd(km), rmsksd(km), rmsked(km), rmskwd(km)
#endif

#ifdef isopycmix
      _MOMA_REAL
     &     tmask(KMP1,-2:2,-2:2),alphai(KM,-1:1,-1:1)
     &     ,betai(KM,-1:1,-1:1)                                         
     &     ,K11(KM,-1:0),K22(KM,-1:0),K33(KM)                           
     &     ,ddxt(KM,-1:1,-1:1,NT),ddyt(KM,-1:1,-1:1,NT)                 
     &     ,ddzt(0:KM,-1:1,-1:1,NT),diff_fn(KM,-1:0,NT)                 
     &     ,diff_fe(KM,-1:0,NT)                                         
     &     ,drodxe(0:1,KM,-1:0),drodyn(0:1,KM,-1:0)                     
     &     ,drodze(0:1,0:1,KM,-1:0)                                     
     &     ,drodzn(0:1,0:1,KM,-1:0)                                     
     &     ,Ai_ez(0:1,0:1,KM,-1:0),Ai_nz(0:1,0:1,KM,-1:0)               
     &     ,drodxb(0:1,0:1,KM),drodyb(0:1,0:1,KM)                       
     &     ,Ai_bx(0:1,0:1,KM),Ai_by(0:1,0:1,KM)                         
     &     ,drodzb(0:1,KM),diff_fbiso(0:KM,NT)
#ifdef gm_diag
      _MOMA_REAL 
     &     adv_vetiso(KM,-1:0),adv_vntiso(KM,-1:0)                          
     &     ,adv_fbiso(KM,NT),adv_vbtiso(0:KM)                             
#endif
#endif

      integer 
     &     maskpo(km), maskmo(km), maskop(km), maskom(km)

       common /work/
     &     dpdx,  dpdy,
     &     fue, fuw,   fvn,  fvs,
     &     vmf,   vtf,
     &     fw,  fwb1, fwb2, temp ,
     &     rmskn, rmsks, rmske, rmskw,
     &     maskpo, maskmo, maskop, maskom,
     &     T_up , Diff_msqUp
     &     ,rhoo,   rhpo,   rhpp,   rhop
#ifdef msq
     &     ,rmsknn, rmskss, rmskee, rmskww
     &     ,rmsknd, rmsksd, rmsked, rmskwd
#endif
C$OMP THREADPRIVATE( /work/ )

#ifdef isopycmix
      common /iso_work/
     &     K11,K22,K33
     &     ,tmask,alphai,betai
     &     ,ddxt,ddyt
     &     ,ddzt,diff_fn
     &     ,diff_fe
     &     ,drodxe,drodyn
     &     ,drodze
     &     ,drodzn
     &     ,Ai_ez,Ai_nz
     &     ,drodxb,drodyb
     &     ,Ai_bx,Ai_by
     &     ,drodzb,diff_fbiso
#ifdef gm_diag      
     &     ,adv_vetiso,adv_vntiso
     &     ,adv_fbiso,adv_vbtiso
#endif
C$OMP THREADPRIVATE( /iso_work/ )
#endif
#ifdef presetp
#ifdef hcomments
c
c  common press stores the pressure field precalculated for
c  subroutine clinic by subroutine setp
c
#endif
      _MOMA_REAL p(km,imt,jmt)
      common /press/  p
#endif
     
#ifdef adiabatic_msq
      _MOMA_REAL bihar_flux_e,bihar_flux_n,Froxb,Froyb,slope_bx,slope_by
      common /bihar_flux_blk/bihar_flux_e(km,0:1,nt)
     &      ,bihar_flux_n(km,0:1,nt)
     &      ,Froxb(0:1,0:1,km),Froyb(0:1,0:1,km)
     &      ,slope_bx(0:1,0:1,km),slope_by(0:1,0:1,km)
C$OMP THREADPRIVATE( /bihar_flux_blk/ )
#endif /* adiabatic_msq */
#ifdef gmHDF
      _MOMA_REAL taper_bx,taper_by, UpDFlux
#ifdef adiabatic_msq
     &     ,flux_bx,flux_by
#endif
      common/gmflux_diag/taper_bx(0:1,0:1,KM),taper_by(0:1,0:1,KM)
     &      ,UpDFlux(0:km,nt)
#ifdef adiabatic_msq
     &     ,flux_bx(0:1,0:1,KM),flux_by(0:1,0:1,KM)
#endif
C$OMP THREADPRIVATE( /gmflux_diag/ )
#endif

