      subroutine ushell(thick,xintp,ncrd,m,nn)
      implicit real*8 (a-h,o-z)                                               dp
      dimension xintp(*),m(2)
   
c...   Programmed to modify the thickness of a plate according
c...   to the formula h=h0(1-sin(pi*x/l)*sin(pi*y/l)).
c

c
c...   include common block concom for increment number (inc)
      include '../common/concom'
c
c...   define pi
      parameter (pi=3.1415927d0)
c
c...   define thickness only during increment 0
      if (inc.gt.0) go to 999
c
c...   define thickness h0 and length rlen
      h0=0.1d0
      rlen=10.0d0
c
c...   use analytical formula
      x=xintp(1)
      y=xintp(2)
      thick=h0*(1.0d0-0.25d0*sin(pi*x/rlen)*sin(pi*y/rlen))
      
      return
      end
