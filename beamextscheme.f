!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2015 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine beamextscheme(yil,ndim,nfield,lakonl,npropstart,prop,
     &  field,mi)
!
!     provides the extrapolation scheme for beams with a cross section
!     which is not rectangular nor elliptical
!
      implicit none
!
      character*8 lakonl
!
      integer npropstart,mi(*),ndim,nfield,j,k,l
!
      real*8 prop(*),ratio,ratio2,yil(ndim,mi(1)),yig(nfield,mi(1)),
     &  field(999,20*mi(3)),r,scal,a8(8,8),pmean1(nfield),
     &  pmean2(nfield),t1,t2,t3,t4,a,b,dummy,shp(4,20),xis(8,3),
     &  bf_top,tf_top,hw,tw,bf_bot,tf_bot,
     &  y_node(8),z_node(8),y_ip(24),z_ip(24),d2,
     &  d2_min,w_sum,w(24),total_w
!
!     extrapolation from a 2x2x2=8 integration point scheme in a hex to
!     the vertex nodes
!    
      data a8 /2.549d0,-.683d0,.183d0,-.683d0,-.683d0,.183d0,
     &        -.04904d0,.183d0,-.683d0,2.549d0,-.683d0,.183d0,
     &        .183d0,-.683d0,.183d0,-.04904d0,-.683d0,.183d0,
     &        -.683d0,2.549d0,.183d0,-.04904d0,.183d0,-.683d0,
     &        .183d0,-.683d0,2.549d0,-.683d0,-.04904d0,.183d0,
     &        -.683d0,.183d0,-.683d0,.183d0,-.04904d0,.183d0,
     &        2.549d0,-.683d0,.183d0,-.683d0,.183d0,-.683d0,
     &        .183d0,-.04904d0,-.683d0,2.549d0,-.683d0,.183d0,
     &        .183d0,-.04904d0,.183d0,-.683d0,-.683d0,.183d0,
     &        -.683d0,2.549d0,-.04904d0,.183d0,-.683d0,.183d0,
     &        .183d0,-.683d0,2.549d0,-.683d0/  
!
      if(lakonl(8:8).eq.'P') then
!
!        pipe cross section
!
         ratio=(prop(npropstart+1)-prop(npropstart+2))/
     &              prop(npropstart+1)
         ratio2=ratio*ratio
!
         r=dsqrt((ratio2+1.d0)/2.d0)
!
         scal=dsqrt(2.d0/3.d0)/r
!
         do k=1,nfield
            pmean1(k)=(yil(k,2)+yil(k,4)+yil(k,6)+yil(k,8))/4.d0
            pmean2(k)=(yil(k,10)+yil(k,12)+yil(k,14)+yil(k,16))/4.d0
         enddo
!
         do k=1,nfield
            yig(k,1)=pmean1(k)+(yil(k,6)-pmean1(k))*scal
            yig(k,2)=pmean2(k)+(yil(k,14)-pmean2(k))*scal
            yig(k,3)=pmean1(k)+(yil(k,8)-pmean1(k))*scal
            yig(k,4)=pmean2(k)+(yil(k,16)-pmean2(k))*scal
            yig(k,5)=pmean1(k)+(yil(k,4)-pmean1(k))*scal
            yig(k,6)=pmean2(k)+(yil(k,12)-pmean2(k))*scal
            yig(k,7)=pmean1(k)+(yil(k,2)-pmean1(k))*scal
            yig(k,8)=pmean2(k)+(yil(k,10)-pmean2(k))*scal
         enddo
!
         do j=1,8
            do k=1,nfield
               field(k,j)=0.d0
               do l=1,8
                  field(k,j)=field(k,j)+a8(j,l)*yig(k,l)
               enddo
            enddo
         enddo
!
      elseif(lakonl(8:8).eq.'B') then
!
!        BOX cross section
         a=prop(npropstart+1)
         b=prop(npropstart+2)
         t1=prop(npropstart+3)
         t2=prop(npropstart+4)
         t3=prop(npropstart+5)
         t4=prop(npropstart+6)
!
         xis(1,1) = -dsqrt(3.0d0)
         xis(1,2) = -(t4-t2-2*b)/((-2*b)+t2+t4)
         xis(1,3) = (t3-t1+2*a)/((-2*a)+t1+t3)
         xis(2,1) = dsqrt(3.0d0)
         xis(2,2) = -(t4-t2-2*b)/((-2*b)+t2+t4)
         xis(2,3) = (t3-t1+2*a)/((-2*a)+t1+t3)
         xis(3,1) = dsqrt(3.0d0)
         xis(3,2) = -(t4-t2+2*b)/((-2*b)+t2+t4)
         xis(3,3) = (t3-t1+2*a)/((-2*a)+t1+t3)
         xis(4,1) = -dsqrt(3.0d0)
         xis(4,2) = -(t4-t2+2*b)/((-2*b)+t2+t4)
         xis(4,3) = (t3-t1+2*a)/((-2*a)+t1+t3)
         xis(5,1) = -dsqrt(3.0d0)
         xis(5,2) = -(t4-t2-2*b)/((-2*b)+t2+t4)
         xis(5,3) = (t3-t1-2*a)/((-2*a)+t1+t3)
         xis(6,1) = dsqrt(3.0d0)
         xis(6,2) = -(t4-t2-2*b)/((-2*b)+t2+t4)
         xis(6,3) = (t3-t1-2*a)/((-2*a)+t1+t3)
         xis(7,1) = dsqrt(3.0d0)
         xis(7,2) = -(t4-t2+2*b)/((-2*b)+t2+t4)
         xis(7,3) = (t3-t1-2*a)/((-2*a)+t1+t3)
         xis(8,1) = -dsqrt(3.0d0)
         xis(8,2) = -(t4-t2+2*b)/((-2*b)+t2+t4)
         xis(8,3) = (t3-t1-2*a)/((-2*a)+t1+t3)
!
         do k=1,nfield
            yig(k,1)=yil(k,9)
            yig(k,2)=yil(k,25)
            yig(k,3)=yil(k,29)
            yig(k,4)=yil(k,13)
            yig(k,5)=yil(k,5)
            yig(k,6)=yil(k,21)
            yig(k,7)=yil(k,17)
            yig(k,8)=yil(k,1)
         enddo
!
         do j=1,8
            call shape8h(xis(j,1),xis(j,2),xis(j,3),dummy,dummy,shp,1)
            do k=1,nfield
               field(k,j)=0.0d0
               do l=1,8
                 field(k,j)=field(k,j)+shp(4,l)*yig(k,l)
               enddo
            enddo
         enddo
!
! >>> H-BEAM MODIFICATION START
      elseif(lakonl(8:8).eq.'H') then
         bf_top = prop(npropstart+1)
         tf_top = prop(npropstart+2)
         hw     = prop(npropstart+3)
         tw     = prop(npropstart+4)
         bf_bot = prop(npropstart+5)
         tf_bot = prop(npropstart+6)
         y_node(1) =  hw/2.d0 + tf_top
         y_node(2) =  hw/2.d0 + tf_top
         y_node(3) = -hw/2.d0 - tf_bot
         y_node(4) = -hw/2.d0 - tf_bot
         y_node(5) =  hw/2.d0 + tf_top
         y_node(6) =  hw/2.d0 + tf_top
         y_node(7) = -hw/2.d0 - tf_bot
         y_node(8) = -hw/2.d0 - tf_bot
         z_node(1) = -bf_top/2.d0
         z_node(2) =  bf_top/2.d0
         z_node(3) =  bf_bot/2.d0
         z_node(4) = -bf_bot/2.d0
         z_node(5) = -bf_top/2.d0
         z_node(6) =  bf_top/2.d0
         z_node(7) =  bf_bot/2.d0
         z_node(8) = -bf_bot/2.d0
         do j=1,8
            y_ip(j)    =  hw/2.d0 + tf_top/2.d0
            z_ip(j)    =  (bf_top/4.d0) * (-1)**j
         enddo
         do j=9,16
            y_ip(j)    =  0.d0
            z_ip(j)    =  (tw/4.d0) * (-1)**j
         enddo
         do j=17,24
            y_ip(j)    = -hw/2.d0 - tf_bot/2.d0
            z_ip(j)    =  (bf_bot/4.d0) * (-1)**j
         enddo
         do j=1,8
            do l=1,24
               d2 = (y_node(j) - y_ip(l))**2 + (z_node(j) - z_ip(l))**2
               if(d2.lt.1.d-10) d2 = 1.d-10
               w(l) = 1.d0/d2
            enddo
            total_w = 0.d0
            do l=1,24
               total_w = total_w + w(l)
            enddo
            do l=1,24
               w(l) = w(l)/total_w
            enddo
            do k=1,nfield
               field(k,j) = 0.d0
               do l=1,24
                  field(k,j) = field(k,j) + w(l)*yil(k,l)
               enddo
            enddo
         enddo
! >>> H-BEAM MODIFICATION END
!
! >>> L-ANGLE MODIFICATION START
      elseif(lakonl(8:8).eq.'L') then
         do k=1,nfield
            yig(k,1)=yil(k,1)
            yig(k,2)=yil(k,2)
            yig(k,3)=yil(k,3)
            yig(k,4)=yil(k,4)
            yig(k,5)=yil(k,5)
            yig(k,6)=yil(k,6)
            yig(k,7)=yil(k,7)
            yig(k,8)=yil(k,8)
         enddo
         do j=1,8
            call shape8h(xis(j,1),xis(j,2),xis(j,3),dummy,dummy,shp,1)
            do k=1,nfield
               field(k,j)=0.0d0
               do l=1,8
                 field(k,j)=field(k,j)+shp(4,l)*yig(k,l)
               enddo
            enddo
         enddo
! >>> L-ANGLE MODIFICATION END
!
! >>> CHANNEL MODIFICATION START
      elseif(lakonl(8:8).eq.'C') then
         do k=1,nfield
            yig(k,1)=yil(k,1)
            yig(k,2)=yil(k,2)
            yig(k,3)=yil(k,3)
            yig(k,4)=yil(k,4)
            yig(k,5)=yil(k,5)
            yig(k,6)=yil(k,6)
            yig(k,7)=yil(k,7)
            yig(k,8)=yil(k,8)
         enddo
         do j=1,8
            call shape8h(xis(j,1),xis(j,2),xis(j,3),dummy,dummy,shp,1)
            do k=1,nfield
               field(k,j)=0.0d0
               do l=1,8
                 field(k,j)=field(k,j)+shp(4,l)*yig(k,l)
               enddo
            enddo
         enddo
! >>> CHANNEL MODIFICATION END
!
      endif
!
      return
      end