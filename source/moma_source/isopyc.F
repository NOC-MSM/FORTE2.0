#include "cpp_options.h"
#ifdef isopycmix
      subroutine isopyc (ic,jc)
      implicit none
c
c=======================================================================
c
c     Compute isopycnal diffusion coefficients Ai_ez, Ai_nz, Ai_bx,
c     Ai_by, and the Gent/McWilliams advection velocities.
c
c     output:
c       Ai_ez = diffusion coefficient centered on east face of T cells
c       Ai_nz = diffusion coefficient centered on north face of T cells
c       Ai_bx = diffusion coefficient centered on bottom face of T cells
c       Ai_by = diffusion coefficient centered on bottom face of T cells
c
#ifdef gm_diag
c       adv_vetiso = isopycnal advective vel on east face of "T" cell
c       adv_vntiso = isopycnal advective vel on north face of "T" cell
c               (Note: this includes the cosine factor as in "adv_vnt")
c       adv_vbtiso = isopycnal advective vel on bottom face of "T" cell
#endif
c
c=======================================================================
c
# include "param.h"
# include "levind.h"
c Arguments:
      integer ic,jc
c pki addition
      if (kmt(ic,jc) .eq. 0) return
c
c
c-----------------------------------------------------------------------
c     estimate alpha, beta, and gradients on sides of T cells
c-----------------------------------------------------------------------
c
      call elements (ic,jc)
c
#ifdef full_griffies 
c-----------------------------------------------------------------------
c     compute Ai_ez centered on eastern face of T cells
c     for surrounding points
c-----------------------------------------------------------------------
c
      call ai_east (ic,jc)
c
c-----------------------------------------------------------------------
c     compute Ai_nz centered on the northern face of T cells
c     for surrounding points
c-----------------------------------------------------------------------
c
      call ai_north (ic,jc)
c
c-----------------------------------------------------------------------
c     evaluate Ai_bx & Ai_by centered on bottom face of T cells
c     for surrounding points
c-----------------------------------------------------------------------
#endif /* full_griffies */

c
      call ai_bottom (ic,jc)
c
c
#ifdef gm_diag
c-----------------------------------------------------------------------
c     compute isopycnal advective velocities for tracers
c     calculate (uiso,viso,wiso) for surrounding points
c-----------------------------------------------------------------------
c
      call isopyc_adv (ic,jc)
c
#endif
c
c-----------------------------------------------------------------------
c     compute isopycnal fluxes
c-----------------------------------------------------------------------

      call isoflux (ic,jc)

      return
      end
c
      subroutine elements (ic,jc)
      implicit none
c
c=======================================================================
c     Estimate alpha, beta, and normal gradients on faces of T cells
c=======================================================================
c
# include "param.h"
# include "grdvar.h"
# include "timelv.h"
# include "slabs.h"
# include "scalar.h"
# include "iounit.h"
# include "levind.h"
# include "coord.h"

c  agn 23/1/2001....
#if !defined Wright && defined linear_rho 
#include "lin_dncoef.h"
#endif /* !Wright && linear_rho  */
c ..................
c Arguments:
      integer ic,jc

c  agn 12/1/2001....
       _MOMA_REAL biggest
#ifdef pdkIso
     & , rhob(kmrl),rhobt(kmrl),tb(kmrl),sb(kmrl)
     & , deltaT, deltaS
#else
     & , to(km), so(km), c(km,9)
     & , tq,sq
#endif
       integer n
       common /elements_com/ 
#ifdef pdkIso
     & rhob,rhobt,tb,sb
#else
     & to, so, c
#include "dncoef.h"
#endif
C$OMP THREADPRIVATE( /elements_com/ )
       parameter (biggest = 0.e300)
c ..................

cppppppppppppppppppppppppppppp
c     pdk changes
c     these lines included to generalise production of alphai and betai
c-------------------------------------------------------------------------
cmarc 5/11/2001 commented out if defined, ok with AGN
c-------------------------------------------------------------------------
c#if defined linear_rho && defined Wright
       integer kmrl,kmrlp1
c Since we hve no bbl in this routine...
       parameter(kmrl =km,kmrlp1=kmrl+1) 
c#endif
cppppppppppppppppppppppppppppp
c Local vars:
       integer i,iq,j,jq,k,kmc,kr
c
c-----------------------------------------------------------------------
c     tmask for surrounding points
c-----------------------------------------------------------------------
c
       do 21 j=-2,2
       do 20 i=-2,2
        kmc=kmt(ic+i,jc+j)
c  agn 9/2/2001....
        do k=1,kmc
c ..................
          tmask(k,i,j)=c1
        enddo                   !1-kmc
        do k=kmc+1,kmrlp1
          tmask(k,i,j)=c0
        enddo                   !kmc+1 - kmrlp1
 20    continue
 21    continue

c
c-----------------------------------------------------------------------
c     initialise various quantities
c-----------------------------------------------------------------------
c
       do 42 j=-1,1
       do 41 i=-1,1
       do 40 k=1,kmrl
            alphai(k,i,j)=c0
            betai(k,i,j)=c0
c agn change 12/1/2001.......
            ddzt(k,i,j,1)=biggest
#ifndef no_salt
            ddzt(k,i,j,2)=-biggest
#endif
c ............................
 40       continue
 41    continue
 42    continue
       
       do n=1,NT
         do j=-1,1
           do i=-1,1
             do k=1,kmrl
               ddxt(k,i,j,n)=c0
               ddyt(k,i,j,n)=c0
             enddo
           enddo
         enddo
       enddo

#ifdef full_griffies
       do 62 i=-1,0
         do 61 k=1,kmrl
           K11(k,i)=c0
           K22(k,i)=c0
           do 60 n=1,NT
             diff_fn(k,i,n)=c0
             diff_fe(k,i,n)=c0
 60        continue
 61      continue
 62    continue
#endif

       do 83 i=-1,0
         do 82 k=1,kmrl
#ifdef gm_diag
           adv_vetiso(k,i)=c0
           adv_vntiso(k,i)=c0
#endif
           do 81 iq=0,1
#ifdef full_griffies
             drodxe(iq,k,i)=c0
             drodyn(iq,k,i)=c0
#endif
             do 80 jq=0,1
c     agn 12/1/2001....
               drodze(iq,jq,k,i)=-biggest
               drodzn(iq,jq,k,i)=-biggest
c     ............................
#ifdef full_griffies
               Ai_ez(iq,jq,k,i)=c0
               Ai_nz(iq,jq,k,i)=c0
#endif
 80          continue
 81        continue
 82      continue
 83    continue
       do 122 k=1,kmrl
         K33(k)=c0
#ifdef gm_diag
         adv_vbtiso(k)=c0
#endif
         do 100 n=1,NT
           diff_fbiso(k,n)=c0
#ifdef gm_diag
           adv_fbiso(k,n)=c0
#endif
 100     continue
         do 121 kr=0,1
           drodzb(kr,k)=-biggest
           do 120 jq=0,1
             drodxb(jq,kr,k)=c0
             drodyb(jq,kr,k)=c0
             Ai_bx(jq,kr,k)=c0
             Ai_by(jq,kr,k)=c0
 120       continue
 121     continue
 122   continue

c
c
c-----------------------------------------------------------------------
c     alpha and beta at centers of T cells
c-----------------------------------------------------------------------
c

c  agn 23/1/2001....
#ifdef Wright
       j=0
       do i = -1,1
         ii = ic + i
         kmc=kmt(ii,jc)
         call drho_wright_n(zt,t(1,ii,jc,1,nm), t(1,ii,jc,NT,nm)
     &        ,alphai(1,i,j),betai(1,i,j),kmc)
       end do
       
       i=0
       do j = -1,1,2
         jj = jc + j
         kmc = kmt(ic,jj)
         call drho_wright_n(zt,t(1,ic,jj,1,nm), t(1,ic,jj,NT,nm)
     &        ,alphai(1,i,j),betai(1,i,j),kmc)
       end do
       
#else /* .not. Wright */
#ifdef  linear_rho
       do j=-1,1
         do i=-1,1
           do k=1,kmt(ic+i,jc+j)
             alphai(k,i,j) = dRhodT
#ifndef no_salt
             betai(k,i,j) = dRhods
#endif /* .not. no_salt */
           end do
         end do
       end do
#else /* .not. linear_rho */
#ifdef pdkIso
#include "pdkIso.F"
#else 
#ifdef no_salt
       sq = c0
#endif
       do 141 i=-1,1                                                 
         do 140 k=1,kmt(ic+i,jc)                                       
           tq = t(k,ic+i,jc,1,nm)-to(k)                              
#ifndef no_salt
           sq = t(k,ic+i,jc,2,nm)-so(k)                              
#endif
           alphai(k,i,0) = c(k,1) + (c(k,4) + c(k,7)*sq)*sq              
     &          +(c2*c(k,3) + c2*c(k,8)*sq + c3*c(k,6)*tq)*tq 
#ifndef no_salt
           betai(k,i,0) =(c(k,4) + c2*c(k,7)*sq + c(k,8)*tq)*tq       
     &          + c(k,2) + (c2*c(k,5) + c3*c(k,9)*sq)*sq      
#endif
 140     continue                                                      
 141   continue                                                      
       
       do 160 k=1,kmt(ic,jc-1)                                     
         tq = t(k,ic,jc-1,1,nm)-to(k)                              
#ifndef no_salt
         sq = t(k,ic,jc-1,2,nm)-so(k)                              
#endif
         alphai(k,0,-1) = c(k,1) + (c(k,4) + c(k,7)*sq)*sq            
     &        +(c2*c(k,3) + c2*c(k,8)*sq + c3*c(k,6)*tq)*tq 
#ifndef no_salt
         betai(k,0,-1) =(c(k,4) + c2*c(k,7)*sq + c(k,8)*tq)*tq       
     &        + c(k,2) + (c2*c(k,5) + c3*c(k,9)*sq)*sq      
#endif
 160   continue                                                      
       
       do 180 k=1,kmt(ic,jc+1)                                     
         tq = t(k,ic,jc+1,1,nm)-to(k)                              
#ifndef no_salt
         sq = t(k,ic,jc+1,2,nm)-so(k)                              
#endif
         alphai(k,0,1) = c(k,1) + (c(k,4) + c(k,7)*sq)*sq            
     &        +(c2*c(k,3) + c2*c(k,8)*sq + c3*c(k,6)*tq)*tq 
#ifndef no_salt
         betai(k,0,1) =(c(k,4) + c2*c(k,7)*sq + c(k,8)*tq)*tq       
     &        + c(k,2) + (c2*c(k,5) + c3*c(k,9)*sq)*sq      
#endif
 180   continue                                                      
       
#endif /*pdkIso*/
#endif /* .not. linear_rho */
#endif /* .not. Wright */
       
c     
c-----------------------------------------------------------------------
c     gradients at bottom face of T cells
c-----------------------------------------------------------------------
c     
       do 203 j=-1,1
         do 202 i=-1,1
           
c     agn change 22/1/2001.......
           ddzt(0,i,j,1)=biggest
#ifndef no_salt
           ddzt(0,i,j,2)=-biggest
#endif
           do 201 n=1,NT
c     ............................
             do 200 k=1,kmt(ic+i,jc+j)-1
               ddzt(k,i,j,n) = dzwr(k)*
     &              (t(k,ic+i,jc+j,n,nm) - t(k+1,ic+i,jc+j,n,nm))
 200         continue
 201       continue
 202     continue
 203   continue
c
c-----------------------------------------------------------------------
c     gradients at eastern face of T cells
c-----------------------------------------------------------------------
c
       do 223 n=1,NT
         do 222 j=-1,1
           do 221 i=-1,1
             do 220 k=1,kmt(ic+i,jc+j)
               ddxt(k,i,j,n) = tmask(k,i+1,j)*cstr(jc+j)*
     &              dxr*(t(k,ic+i+1,jc+j,n,nm) - t(k,ic+i,jc+j,n,nm))
 220         continue
 221       continue
 222     continue
 223   continue
c
c-----------------------------------------------------------------------
c     gradients at northern face of T cells
c-----------------------------------------------------------------------
c
       do 243 n=1,NT
         do 242 j=-1,1
           do 241 i=-1,1
             do 240 k=1,kmt(ic+i,jc+j)
               ddyt(k,i,j,n) = tmask(k,i,j+1)*dyr*
     &              (t(k,ic+i,jc+j+1,n,nm) - t(k,ic+i,jc+j,n,nm))
 240         continue
 241       continue
 242     continue
 243   continue

      do 302 i=-1,0
        do 301 k=1,kmt(ic+i,jc)
          do 300 iq=0,1
#ifdef full_griffies
            drodxe(iq,k,i)=alphai(k,i+iq,0)*ddxt(k,i,0,1)
#ifndef no_salt
     &           + betai(k,i+iq,0)*ddxt(k,i,0,2)
#endif
#endif /* full_griffies */
            do 280 kr=0,1
              drodze(iq,kr,k,i) =
     &             alphai(k,i+iq,0)*ddzt(k-1+kr,i+iq,0,1)
#ifndef no_salt
     &             + betai(k,i+iq,0)*ddzt(k-1+kr,i+iq,0,2)
#endif
 280        continue
 300      continue
 301    continue
 302  continue

      do 362 j=-1,0
        do 361 k=1,kmt(ic,jc+j)
          do 360 jq=0,1
#ifdef full_griffies
            drodyn(jq,k,j) =    alphai(k,0,j+jq)*ddyt(k,0,j,1)
#ifndef no_salt
     &                     + betai(k,0,j+jq)*ddyt(k,0,j,2)
#endif
#endif /* full_griffies */
            do 320 kr=0,1
              drodzn(jq,kr,k,j) =
     &             alphai(k,0,j+jq)*ddzt(k-1+kr,0,j+jq,1)
#ifndef no_salt
     &             + betai(k,0,j+jq)*ddzt(k-1+kr,0,j+jq,2)
#endif
 320        continue
 360      continue
 361    continue
 362  continue
          
      do 421 k=1,kmt(ic,jc)-1
        do 420 kr=0,1
          drodzb(kr,k) =    alphai(k+kr,0,0)*ddzt(k,0,0,1)
#ifndef no_salt
     &         + betai(k+kr,0,0)*ddzt(k,0,0,2)
#endif
         do 380 iq=0,1
           drodxb(iq,kr,k) = alphai(k+kr,0,0)*ddxt(k+kr,iq-1,0,1)
#ifndef no_salt
     &          + betai(k+kr,0,0)*ddxt(k+kr,iq-1,0,2)
#endif
#ifdef adiabatic_msq
           Froxb(iq,kr,k) = (
     &          alphai(k+kr,0,0)*bihar_flux_e(k+kr,iq,1)
#ifndef no_salt
     &          + betai(k+kr,0,0)*bihar_flux_e(k+kr,iq,2)
#endif
     &          )*dyr*dzr(k+kr)
#endif /* adiabatic_msq */
 380     continue
         do 400 jq=0,1
           drodyb(jq,kr,k) = alphai(k+kr,0,0)*ddyt(k+kr,0,jq-1,1)
#ifndef no_salt
     &          + betai(k+kr,0,0)*ddyt(k+kr,0,jq-1,2)
#endif
#ifdef adiabatic_msq
           Froyb(jq,kr,k) = (
     &          alphai(k+kr,0,0)*bihar_flux_n(k+kr,jq,1)
#ifndef no_salt
     &          + betai(k+kr,0,0)*bihar_flux_n(k+kr,jq,2)
#endif
     &          )*dxr*dzr(k+kr)*csur(jc+jq-1)
#endif /* adiabatic_msq */
 400     continue
 420  continue
 421  continue

      return
      end

#ifdef full_griffies
      subroutine ai_east (ic,jc)
      implicit none
c
c=======================================================================
c     compute "Ai_ez" & "K11" at the center of eastern face of T cells.
c=======================================================================
c
# include "param.h"
# include "grdvar.h"
# include "timelv.h"
# include "slabs.h"
# include "chmix.h"
# include "iounit.h"
# include "levind.h"

c Arguments:
      integer ic,jc
c  agn 22/1/2001....
c       _MOMA_REAL notQuitesoBig
c       parameter (notQuitesoBig = 1.e290)
c ..................
c  agn 24/1/2001....
      _MOMA_REAL belowSurface
c ..................
c local vars:
      integer i,k,kr,iq
      _MOMA_REAL Ai11,Ai13,Asteep,sumz,sxe,abssxe,taper
c     
c
c-----------------------------------------------------------------------
c     compute "Ai_ez" on east face of T cell. Note re-scaling factor
c     which reduces mixing coefficient "Ai" where slope "sxe"
c     exceeds the critical slope "sc" for the small slope approx.
c-----------------------------------------------------------------------
c
      do 61 i=-1,0
        do 60 k=1,kmt(ic+i,jc)
          Ai11 = ahisop
          Asteep = ahsteep
#ifdef gent_mcwilliams
          Ai13 = ahisop-athkdf
#else 
          Ai13 = Ai11
#endif
          sumz = c0
          do 21 kr=0,1
            
c     agn 24/1/2001....
#ifndef KT
            if((k+kr).gt.1) then
              belowSurface = c1
            else
              belowSurface = c0
            endif
#endif
c     ..................
            do 20 iq=0,1
c     agn 22/1/2001....
              sxe =    -drodxe(iq,k,i)/(drodze(iq,kr,k,i)-epsln)
c     ..................
              abssxe = abs(sxe)
              if (abssxe .gt. slmx) then
                taper = (slmx/(abssxe + epsln))**2
              else
                taper = c1
              endif
              Ai_ez(iq,kr,k,i) = Ai13*taper*sxe
c     agn 24/1/2001....
     &             *tmask(k+kr,i+iq,0)
#ifndef KT
     &             *belowSurface
#endif
c     &            *tmask(k,i+1,0)
c     ..................
#ifdef pkimix
     &             + 0.5*(psi1(k-1+kr,ic,jc) + psi1(k-1+kr,ic,jc-1))
#endif
              sumz = sumz +
     &             dzw(k-1+kr)
     &             *max(Asteep,Ai11*taper)
c     agn 24/1/2001....
c     &                 *tmask(k,i+1,0)
     &             *tmask(k+kr,i+iq,0)
#ifndef KT
     &             *belowSurface
#endif
 20         continue
 21       continue
          K11(k,i) = dz4r(k)*sumz
 60     continue
 61   continue
c     
      return
      end
      
      subroutine ai_north (ic,jc)
      implicit none
c     
c=======================================================================
c     compute "Ai_nz" & "K22" at the center of northern face of T cells.
c=======================================================================
c     
# include "param.h"
# include "grdvar.h"
# include "timelv.h"
# include "slabs.h"
# include "chmix.h"
# include "levind.h"
      
c     Arguments:
      integer ic,jc
c     Local vars:
      _MOMA_REAL Ai22,Ai23,asteep,sumz,syn,abssyn,taper
      integer j,k,jq,kr
c  agn 24/1/2001....
      _MOMA_REAL belowSurface
c     ..................
c     
c-----------------------------------------------------------------------
c     compute "Ai_nz" on north face of T cell. Note re-scaling factor
c     which reduces mixing coefficient "Ai" where slope "syn"
c     exceeds the critical slope "sc" for the small slope approx.
c-----------------------------------------------------------------------
c
      do 61 j=-1,0
        do 60 k=1,kmt(ic,jc+j)
          Ai22 = ahisop
          Asteep = ahsteep
#ifdef gent_mcwilliams
          Ai23 = ahisop-athkdf
#else 
          Ai23 = Ai22
#endif
          sumz = c0
          do 21 kr=0,1
c     agn 24/1/2001....
#ifndef KT
            if(k+kr.gt.1) then 
              belowSurface = 1.d0
            else
              belowSurface = 0.d0
            endif
#endif
c     ..................
            do 20 jq=0,1
c     agn 22/1/2001....
              syn = -drodyn(jq,k,j)/(drodzn(jq,kr,k,j)-epsln)
c     ..................
c     if(ic.eq.(icend-3).and.jc.ge.22.
c     &   .and.(k.eq.kmt(ic,jc).or.k.eq.kmt(ic,jc)-1).and.j.eq.0)then
c     write(19,2002) jc,jq,kr,k,j,drodzn(jq,kr,k,j)
c     &              ,drodyn(jq,k,j)
c     2002   format(1x,'drodzn(',i2,'|',i2,'|',i2,'|',i2'|',i2')=',2g11.4)
c     end if
              abssyn = abs(syn)
              if (abssyn .gt. slmx) then
                taper = (slmx/(abssyn + epsln))**2
              else
                taper = c1
              endif
              Ai_nz(jq,kr,k,j) = Ai23*taper*syn
c     agn 24/1/2001....
     &             *tmask(k+kr,0,j+jq)
#ifndef KT
     &             *belowSurface
#endif
c     &                *tmask(k,0,j+1)
c     ..................
#ifdef pkimix
     &             + 0.5*(psi2(k-1+kr,ic-1,jc) + psi2(k-1+kr,ic,jc))
#endif
              sumz = sumz +
     &             dzw(k-1+kr)
     &             *max(Asteep,Ai22*taper)
c     agn 24/1/2001....
c     &                 *tmask(k,0,j+1)
     &             *tmask(k+kr,0,j+jq)
#ifndef KT
     &             *belowSurface
#endif
 20         continue
 21       continue
          K22(k,j) = dz4r(k)*sumz
c     if(ic.eq.3.and.jc.ge.22.and.jc.le.24
c     &            .and.k.le.6)then
c     write(19,*)'jc=',jc,'n=',n,'k=',k,'j=',j
c     write(19,*) K22(k,j),diff_fn(k,j,n),flux_y
c     end if
 60     continue
 61   continue
c     
      return
      end
#endif /* full_griffies */
      
      
      
      subroutine ai_bottom (ic,jc)
      implicit none
c     
c=======================================================================
c     compute Ai_bx, Ai_by, and K33 at the center of the bottom face of
c     T cells.
c=======================================================================
c     
# include "param.h"
# include "grdvar.h"
# include "timelv.h"
# include "slabs.h"
# include "chmix.h"
# include "levind.h"
c     Subroutine Arguments:
      integer ic,jc
c     Local vars:
      _MOMA_REAL Ai33,Ai31,Ai32,facty,sumx,sumy,
     &     abssxb,abssyb,sxb,syb,taper
      integer iq,jq,k,kr
c-----------------------------------------------------------------------
c     compute "Ai_bx", "Ai_by", & K33 on bottom face of T cell. Note
c     re-scaling factor to reduce mixing coefficient "Ai" where slopes
c     "sxb" and "syb" exceeds the critical slope "sc" for the small slope
c     approx.
c-----------------------------------------------------------------------
      Ai33 = ahisop
#ifdef full_griffies
      Ai31 = ahisop+athkdf
#else /* assume ahisop=athkdf */
      Ai31 = c2*ahisop
c     print*,'Ai31,c2,ahisop: ',Ai31,c2,ahisop
      
#endif /* full_griffies */
      
      Ai32 = Ai31
#ifdef Ntaper
      flxmx = slmx*1e8
#endif /* ! Ntaper */
      do 60 k=1,kmt(ic,jc)-1
c     
c     eastward slopes at the base of T cells
c     
        sumx = c0
        do 21 iq=0,1
          do 20 kr=0,1
            sxb = -drodxb(iq,kr,k)/(drodzb(kr,k)-epsln)
            abssxb = abs(sxb)
            if (abssxb .gt. slmx) then
              taper = (slmx/(abssxb + epsln))**2
            else
              taper = c1
            endif
#ifdef Ntaper
#ifdef adiabatic_msq
#ifdef full_amsq
            fxb = -Froxb(iq,kr,k)/(drodzb(kr,k)-epsln)
            absfxb = abs(fxb)
            if (absfxb .gt. flxmx) then
              fxb = (flxmx/(absfxb))*fxb
            endif
#endif
            slope_bx(iq,kr,k)=sxb*taper
#endif
            Ai_bx(iq,kr,k) = taper*Ai31*sxb
#ifdef full_amsq
     &           + fxb
#endif
            
#ifdef pkimix
     &           - p5*(psi1(k,ic-1+iq,jc) + psi1(k,ic-1+iq,jc-1))
#endif
            sumx = sumx + taper*sxb*(Ai33*sxb*taper
#ifdef full_amsq
     &           + fxb
#endif
     &           )
#else /* ! Ntaper */
#ifdef adiabatic_msq
#ifdef full_amsq
            fxb = -Froxb(iq,kr,k)/(drodzb(kr,k)-epsln)
#endif
            slope_bx(iq,kr,k)=sxb*taper
#endif
            Ai_bx(iq,kr,k) = taper*(Ai31*sxb
#ifdef full_amsq
     &           + fxb
#endif
     &           )
            
#ifdef pkimix
     &           - 0.5*(psi1(k,ic-1+iq,jc) + psi1(k,ic-1+iq,jc-1))
#endif
            sumx = sumx + taper*sxb*(Ai33*sxb
#ifdef full_amsq
     &           + fxb
#endif
     &           )
#endif /* ! Ntaper */
#ifdef gmHDF
            taper_bx(iq,kr,k)=taper
#ifdef full_amsq
            flux_bx(iq,kr,k) = fxb
#endif
#endif
 20       continue
 21     continue
c     
c     northward slopes at the base of T cells
c     
        sumy = c0
        do 41 jq=0,1
          facty = csu(jc-1+jq)
          do 40 kr=0,1
            syb = -drodyb(jq,kr,k)/(drodzb(kr,k)-epsln)
            abssyb = abs(syb)
            if (abssyb .gt. slmx) then
              taper = (slmx/(abssyb + epsln))**2
            else
              taper = c1
            endif
#ifdef Ntaper
#ifdef adiabatic_msq
#ifdef full_amsq
            fyb = -Froyb(iq,kr,k)/(drodzb(kr,k)-epsln)
            absfyb = abs(fyb)
            if (absfyb .gt. flxmx) then
              fyb = (flxmx/(absfyb))*fyb
            endif
#endif
            slope_by(jq,kr,k)=syb*taper
#endif
            Ai_by(jq,kr,k) = taper*Ai32*syb
#ifdef full_amsq
     &           + fyb
#endif
            
#ifdef pkimix
     &            - p5*(psi2(k,ic-1,jc-1+jq) + psi2(k,ic,jc-1+jq))
#endif
            sumy = sumy + taper*syb*facty*(Ai33*syb*taper
#ifdef full_amsq
     &           + fyb
#endif
     &           )
#else /* ! Ntaper */
#ifdef adiabatic_msq
#ifdef full_amsq
            fyb = -Froyb(jq,kr,k)/(drodzb(kr,k)-epsln)
#endif
            slope_by(jq,kr,k)=syb*taper
#endif
            Ai_by(jq,kr,k) = taper*(Ai32*syb
#ifdef full_amsq
     &           + fyb
#endif
     &           )
#ifdef pkimix
     &           - p5*(psi2(k,ic-1,jc-1+jq) + psi2(k,ic,jc-1+jq))
#endif
            sumy = sumy + taper*syb*facty*(Ai33*syb
#ifdef full_amsq
     &           + fyb
#endif
     &           )
#endif /* ! Ntaper */
#ifdef gmHDF
            taper_by(jq,kr,k)=taper
#ifdef full_amsq
            flux_by(jq,kr,k) = fyb
#endif
#endif
 40       continue
 41     continue
c     
        K33(k) = (p25*sumx + p25*cstr(jc)*sumy)
c     print*,'k33=',K33(k),k
 60   continue
c     
      return
      end
      
      

      subroutine isoflux (ic,jc)
      implicit none
c     
c=======================================================================
c     isopycnal diffusive tracer fluxes are computed.
c=======================================================================
c
# include "param.h"
# include "grdvar.h"
# include "scalar.h"
# include "timelv.h"
# include "slabs.h"
# include "chmix.h"
# include "iounit.h"
# include "levind.h"
c Subroutine Arguments:
      integer ic,jc
c     Local vars:
      _MOMA_REAL boxa,boxar,sumx,sumy
      integer iq,jq,k,kr,kmc, n
#ifdef full_griffies
      integer i,j,km1kr,kpkr
      _MOMA_REAL dxd,dyd,sumz,flux_x,flux_y,csu_dz4r
      integer kmrl,kmrlp1
c     Since we hve no bbl in this routine...
      parameter(kmrl =km,kmrlp1=kmrl+1) 
#endif
c     .... 22/4 for debugging of adiabat_msq
      boxa = dx*cst(jc)*dy
      boxar = c1/boxa
c     ................................................
c     
c-----------------------------------------------------------------------
c     construct total isopycnal tracer flux at east face of "T" cells
c-----------------------------------------------------------------------
c     c
      do 200 n=1,NT
c     if(ic.eq.(icend-3).and.jc.ge.22) then
c     write(19,*)'jc= ',jc,' ic=',ic,'m= ',n
c     end if
        
#ifdef full_griffies
c-----------------------------------------------------------------------
c     construct tracer fluxes at east & north faces of "T" cells
c     first ........at east faces of "T" cells
c-----------------------------------------------------------------------
        do 61 i=-1,0
          dxd=dxr*cstr(jc)
          kmc=kmt(ic+i,jc)
          do 60 k=1,kmc
            sumz = c0
            do 21 kr=0,1
              km1kr = max(k-1+kr,1)
c     agn 9/2/2001 .......
              kpkr = min(k+kr,kmrl)
c..................................
              do 20 iq=0,1
                sumz = sumz + Ai_ez(iq,kr,k,i)
     &               *(t(km1kr,ic+iq+i,jc,n,nm)-t(kpkr,ic+iq+i,jc,n,nm))
 20           continue
 21         continue
            flux_x = dz4r(k)*sumz
            diff_fe(k,i,n) = K11(k,i)*dxd*
     &           (t(k,ic+1+i,jc,n,nm) - t(k,ic+i,jc,n,nm))
     &           + flux_x
 60       continue
 61     continue
c
c-----------------------------------------------------------------------
c     now at north faces of "T" cells
c-----------------------------------------------------------------------
c
        do 121 j=-1,0
          dyd=dyr
          kmc=kmt(ic,jc+j)
          do 120 k=1,kmc
            csu_dz4r = csu(jc+j)*dz4r(k)
            sumz = c0
            do 81 kr=0,1
              km1kr = max(k-1+kr,1)
c     agn 9/2/2001 .......
              kpkr = min(k+kr,kmrl)
c..................................
              do 80 jq=0,1
                sumz = sumz + Ai_nz(jq,kr,k,j)
     &               *(t(km1kr,ic,jc+jq+j,n,nm)-t(kpkr,ic,jc+jq+j,n,nm))
 80           continue
 81         continue
            flux_y = csu_dz4r*sumz
            diff_fn(k,j,n) = K22(k,j)*csu(jc+j)*dyd*
     &           (t(k,ic,jc+1+j,n,nm)-t(k,ic,jc+j,n,nm))
     &           + flux_y
 120      continue
 121    continue
 2001   format(1x,'K22(',i2,'|',i2,'|',i2,')=',g11.4
     &       ,5x,'diff_fn(',i2,'|',i2,'|',i2,')=',g11.4
     &       ,5x,'flux_y(',i2,'|',i2,'|',i2,')=',g11.4 )
c     
#endif /* full_griffies */
c-----------------------------------------------------------------------
c     compute the vertical tracer flux "diff_fbiso" containing the K31
c     and K32 components which are to be solved explicitly. The K33
c     component will be treated implicitly. Note that there are some
c     cancellations of dxu(i-1+iq) and dyu(jrow-1+jq)
c-----------------------------------------------------------------------
c     
        kmc = kmt(ic,jc)
        do 180 k=1,kmc-1
          sumx = c0
          do 141 iq=0,1
            do 140 kr=0,1
              sumx = sumx
     &             + Ai_bx(iq,kr,k)*
     &             cstr(jc)*(t(k+kr,ic+iq,jc,n,nm) 
     &             - t(k+kr,ic-1+iq,jc,n,nm))
#ifdef adiabatic_msq
     &             + bihar_flux_e(k+kr,iq,n)*dx*dzr(k+kr)*dyr
     &             *slope_bx(iq,kr,k)
#endif
 140        continue
 141      continue
          
          sumy = c0
          do 161 jq=0,1
            do 160 kr=0,1
              sumy = sumy
     &             + Ai_by(jq,kr,k)*
     &             csu(jc-1+jq)*(t(k+kr,ic,jc+jq,n,nm)
     &             -t(k+kr,ic,jc-1+jq,n,nm))
#ifdef adiabatic_msq
     &             + bihar_flux_n(k+kr,jq,n)*dxr*dzr(k+kr)
     &             *dy*slope_by(jq,kr,k)
#endif
 160        continue
 161      continue
          diff_fbiso(k,n) = (dx4r*sumx
     &         + dy4r*cstr(jc)*sumy)
 180    continue
        diff_fbiso(0,n)  = c0
        diff_fbiso(kmc,n) = c0
 200  continue
c     
      return
      end

      
#ifdef gm_diag
      subroutine isopyc_adv (ic,jc)
      implicit none
c     
c=======================================================================
c     compute isopycnal transport velocities.
c     
c     author:  Gokhan Danabasoglu   e-mail => gokhan@isis.cgd.ucar.edu
C     
C     NOTE:
C     No allowance has been made for the Shared Boundary
c=======================================================================
c     
# include "param.h"
# include "grdvar.h"
# include "timelv.h"
# include "slabs.h"
# include "chmix.h"
# include "levind.h"
c     Subroutine Arguments:
      integer ic,jc
c     local vars:
      integer kmrl,kmrlp1
c     Since we hve no bbl in this routine...
      parameter(kmrl =km,kmrlp1=kmrl+1) 
      _MOMA_REAL top_bc(kmrl), bot_bc(kmrl)
      integer i,j,k,km1,kp1
      _MOMA_REAL Ath0,at,bt,stn,sbn,absstn,abssbn,ath_t,ath_b
      _MOMA_REAL absste,abssbe,ste,sbe,ab,bb
c     
      do k=1,kmrl
        top_bc(k) = c1
        bot_bc(k) = c1
      enddo
      top_bc(1)  = c0
      bot_bc(kmrl) = c0
c
c-----------------------------------------------------------------------
c     compute the meridional component of the isopycnal mixing velocity
c     at the center of the northern face of the "T" cells.
c-----------------------------------------------------------------------
c
      do j=-1,0
        do k=1,kmt(ic,jc+j)
          km1 = max(k-1,1)
          kp1 = min(k+1,kmrl)
          Ath0 = athkdf
c     
          at =     (alphai(k,0,j) + alphai(k,0,j+1) + alphai(km1,0,j)
     &              + alphai(km1,0,j+1))
          bt =     (betai(k,0,j) + betai(k,0,j+1) + betai(km1,0,j)
     &         + betai(km1,0,j+1))
          stn = -(at*(ddyt(k,0,j,1) + ddyt(km1,0,j,1))
     &         + bt*(ddyt(k,0,j,2) + ddyt(km1,0,j,2))) /
     &         (at*(ddzt(km1,0,j,1) + ddzt(km1,0,j+1,1))
     &         + bt*(ddzt(km1,0,j,2) + ddzt(km1,0,j+1,2))-epsln)
c     
          ab =     (alphai(k,0,j) + alphai(k,0,j+1) + alphai(kp1,0,j)
     &         + alphai(kp1,0,j+1))
          bb =     (betai(k,0,j) + betai(k,0,j+1) + betai(kp1,0,j)
     &         + betai(kp1,0,j+1))
          sbn = -(ab*(ddyt(k,0,j,1) + ddyt(kp1,0,j,1))
     &         + bb*(ddyt(k,0,j,2) + ddyt(kp1,0,j,2))) /
     &         (ab*(ddzt(k,0,j,1) + ddzt(k,0,j+1,1))
     &         + bb*(ddzt(k,0,j,2) + ddzt(k,0,j+1,2))-epsln)
          absstn = abs(stn)
          abssbn = abs(sbn)
          if (absstn .gt. slmx) then
            ath_t = Ath0*tmask(k,0,j)*tmask(k,0,j+1)
     &           *(slmx/(absstn + epsln))**2
          else
            ath_t = Ath0*tmask(k,0,j)*tmask(k,0,j+1)
          endif
          if (abssbn .gt. slmx) then
            ath_b = Ath0*tmask(kp1,0,j)*tmask(kp1,0,j+1)
     &           *(slmx/(abssbn + epsln))**2
          else
            ath_b = Ath0*tmask(kp1,0,j)*tmask(kp1,0,j+1)
          endif
          adv_vntiso(k,j) = -(ath_t*stn*top_bc(k) -
     &         ath_b*sbn*bot_bc(k))*dzr(k)*csu(jc+j)
        enddo
      enddo
c     
c-----------------------------------------------------------------------
c     compute the zonal component of the isopycnal mixing velocity
c     at the center of the eastern face of the "T" grid cell.
c-----------------------------------------------------------------------
c
      do i=-1,0
        do k=1,kmt(ic+i,jc)
          km1 = max(k-1,1)
          kp1 = min(k+1,kmrl)
          Ath0 = athkdf
c     
          at =     (alphai(k,i,0) + alphai(k,i+1,0) + alphai(km1,i,0)
     &         + alphai(km1,i+1,0))
          bt =     (betai(k,i,0) + betai(k,i+1,0) + betai(km1,i,0)
     &         + betai(km1,i+1,0))
          ste =-(at*(ddxt(k,i,0,1) + ddxt(km1,i,0,1))
     &         + bt*(ddxt(k,i,0,2) + ddxt(km1,i,0,2)))
     &         / (at*(ddzt(km1,i,0,1) + ddzt(km1,i+1,0,1))
     &         + bt*(ddzt(km1,i,0,2) + ddzt(km1,i+1,0,2))-epsln)
c     
          ab =     (alphai(k,i,0) + alphai(k,i+1,0) + alphai(kp1,i,0)
     &         + alphai(kp1,i+1,0))
          bb =     (betai(k,i,0) + betai(k,i+1,0) + betai(kp1,i,0)
     &         + betai(kp1,i+1,0))
          sbe =-(ab*(ddxt(k,i,0,1) + ddxt(kp1,i,0,1))
     &         + bb*(ddxt(k,i,0,2) + ddxt(kp1,i,0,2))) /
     &         (ab*(ddzt(k,i,0,1) + ddzt(k,i+1,0,1))
     &         + bb*(ddzt(k,i,0,2) + ddzt(k,i+1,0,2))-epsln)
          
          absste = abs(ste)
          abssbe = abs(sbe)
          if (absste .gt. slmx) then
            ath_t = Ath0*tmask(k,i,0)*tmask(k,i+1,0)
     &           *(slmx/(absste + epsln))**2
          else
            ath_t = Ath0*tmask(k,i,0)*tmask(k,i+1,0)
          endif
          if (abssbe .gt. slmx) then
            ath_b = Ath0*tmask(kp1,i,0)*tmask(kp1,i+1,0)
     &           *(slmx/(abssbe + epsln))**2
          else
            ath_b = Ath0*tmask(kp1,i,0)*tmask(kp1,i+1,0)
          endif
          adv_vetiso(k,i) = -(ath_t*ste*top_bc(k) -
     &         ath_b*sbe*bot_bc(k))*dzr(k)
        enddo
      enddo
c     
c-----------------------------------------------------------------------
c     compute the vertical component of the isopycnal mixing velocity
c     at the center of the bottom face of the "T" cells, using the
c     continuity equation for the isopycnal mixing velocities
c-----------------------------------------------------------------------
c     
      adv_vbtiso(0) = c0
      adv_vbtiso(kmt(ic,jc)) = c0
c     
      do k=1,kmt(ic,jc)-1
        adv_vbtiso(k) = dz(k)*cstr(jc)*(
     &       (adv_vetiso(k,0) - adv_vetiso(k,-1))*dxr +
     &       (adv_vntiso(k,0) - adv_vntiso(k,-1))*dyr)
      enddo
c     
      do k=1,kmt(ic,jc)-1
        adv_vbtiso(k) = adv_vbtiso(k) + adv_vbtiso(k-1)
      enddo
c     
      return
      end
#endif /* gm_diag */

#endif /* isopycmix */
