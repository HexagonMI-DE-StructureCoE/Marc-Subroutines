      subroutine forcem(press,th1,th2,nn,n)
      implicit real*8 (a-h,o-z)                                               dp

c* * * * * *
c      Guardando su documentazione manuale d
c      scopro che per un elemento 3d th1 coordinate th2 vettore forza
c
c     defined non-uniformed distributed force on an element.
c
c     press        distributed load increment magnitude
c     th1          coordinate
c     th2          coordinate
c     nn           integration point number
c     n            element number,etc
c
c* * * * * *

      dimension th1(3),th2(3),n(7)
c
c... Include common block creeps to obtain total time
c.... (cptim) and time increment (timinc)
      include "../common/creeps"
c
c... Define b and v
      b=1.0d0
      v=sqrt(2.0d0)
c
c... Determine current centre of load
      distan=v*(cptim+timinc)
      xc=0.5d0*sqrt(2.0d0)*distan
      yc=0.5d0*sqrt(2.0d0)*distan
c
c... Boundaries of loaded area
      xmin=xc-b/2.0d0
      xmax=xc+b/2.0d0
      ymin=yc-b/2.0d0
      ymax=yc+b/2.0d0
c
c... Define non-zero load only if integration point is
c... located within the boundaries of loaded area
      press=0.0d0
      if (th1(1).le.xmax.and.th1(1).ge.xmin.and.
     $ th1(2).le.ymax.and.th1(2).ge.ymin) press=2.0d0
c
c... Define direction of load
      th2(1) = 0.0d0
      th2(2) = 0.0d0
      th2(3) =-1.0d0
c
      return
      end
