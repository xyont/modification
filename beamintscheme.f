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
      subroutine beamintscheme(lakonl,mint3d,npropstart,prop,
     &  kk,xi,et,ze,weight)
!
!     provides the integration scheme for beams with a cross section
!     which is not rectangular nor elliptical
!
!     mint3d: number of integration points (returned if kk=0)
!     xi,et,ze: local coordinates of integration point kk
!     weight: weight for integration point kk
!
      implicit none
!
      character*8 lakonl
!
      integer mint3d,npropstart,jj,kk
!
! >>> MODIFICATION START: Deklarasi variabel tambahan
      real*8 prop(*),xi,et,ze,weight,ratio,ratio2,dtheta,theta,r,
     &  t1,t2,t3,t4,a,b,bf_top,tf_top,hw,tw,bf_bot,tf_bot,
     &  y_top,y_bot,y_mid,z_center,z_left,z_right,
     &  b_angle,t_angle,bf_ch,tf_ch,hw_ch,tw_ch
! >>> MODIFICATION END
!
      if(lakonl(8:8).eq.'P') then
!
!        pipe cross section
!
         if(kk.eq.0) then
            mint3d=16
            return
         endif
!
!        ratio of inner radius to outer radius
!
         ratio=(prop(npropstart+1)-prop(npropstart+2))/
     &              prop(npropstart+1)
         ratio2=ratio*ratio
!
         if(kk.gt.8) then
            jj=kk-8
            xi=1.d0/dsqrt(3.d0)
         else
            jj=kk
            xi=-1.d0/dsqrt(3.d0)
         endif
!
!        pi/4
!
         dtheta=datan(1.d0)
!
         theta=(jj-1)*dtheta
         r=dsqrt((ratio2+1.d0)/2.d0)
!
         et=r*dcos(theta)
         ze=r*dsin(theta)
         weight=dtheta*(1.d0-ratio2)/2.d0
!
      elseif(lakonl(8:8).eq.'B') then
         if(kk.eq.0) then
           mint3d=32
           return
         endif
!
!        2 pts in long direction xi
!
         if(kk.gt.16) then
            jj=kk-16
            xi=1.d0/dsqrt(3.d0)
         else
            jj=kk
            xi=-1.d0/dsqrt(3.d0)
         endif
!
!        pts in cross sections
!
         a=prop(npropstart+1)
         b=prop(npropstart+2)
         t1=prop(npropstart+3)
         t2=prop(npropstart+4)
         t3=prop(npropstart+5)
         t4=prop(npropstart+6)
!
         if(jj.eq.1)then 
            et = -(t4-b)/b
            ze = -(t1-a)/a
            weight = -((((-2*a)+2*t1+t3)*t4+t1*t2-2*b*t1)/(a*b))/6.0d+0
         elseif(jj.eq.2)then 
            et = -((3*t4-t2-2*b)/b)/4.0d+0
            ze = -(t1-a)/a
            weight = -((2*t1*t4+2*t1*t2-4*b*t1)/(a*b))/3.0d+0
         elseif(jj.eq.3)then 
            et = -((t4-t2)/b)/2.0d+0
            ze = -(t1-a)/a
            weight = -((t1*t4+t1*t2-2*b*t1)/(a*b))/3.0d+0
         elseif(jj.eq.4)then 
            et = -((t4-3*t2+2*b)/b)/4.0d+0
            ze = -(t1-a)/a
            weight = -((2*t1*t4+2*t1*t2-4*b*t1)/(a*b))/3.0d+0
         elseif(jj.eq.5)then 
            et = (t2-b)/b
            ze = -(t1-a)/a
            weight = -((t1*t4+t2*t3+(2*t1-2*a)*t2-2*b*t1)/(a*b))/6.0d+0
         elseif(jj.eq.6)then 
            et = (t2-b)/b
            ze = ((t3-3*t1+2*a)/a)/4.0d+0
            weight = -((2*t2*t3+(2*t1-4*a)*t2)/(a*b))/3.0d+0
         elseif(jj.eq.7)then 
            et = (t2-b)/b
            ze = ((t3-t1)/a)/2.0d+0
            weight = -((t2*t3+(t1-2*a)*t2)/(a*b))/3.0d+0
         elseif(jj.eq.8)then 
            et = (t2-b)/b
            ze = ((3*t3-t1-2*a)/a)/4.0d+0
            weight = -((2*t2*t3+(2*t1-4*a)*t2)/(a*b))/3.0d+0
         elseif(jj.eq.9)then 
            et = (t2-b)/b
            ze = (t3-a)/a
            weight = -((t3*t4+(2*t2-2*b)*t3+(t1-2*a)*t2)/(a*b))/6.0d+0
         elseif(jj.eq.10)then 
            et = -((t4-3*t2+2*b)/b)/4.0d+0
            ze = (t3-a)/a
            weight = -((2*t3*t4+(2*t2-4*b)*t3)/(a*b))/3.0d+0
         elseif(jj.eq.11)then 
            et = -((t4-t2)/b)/2.0d+0
            ze = (t3-a)/a
            weight = -((t3*t4+(t2-2*b)*t3)/(a*b))/3.0d+0
         elseif(jj.eq.12)then 
            et = -((3*t4-t2-2*b)/b)/4.0d+0
            ze = (t3-a)/a
            weight = -((2*t3*t4+(2*t2-4*b)*t3)/(a*b))/3.0d+0
         elseif(jj.eq.13)then 
            et = -(t4-b)/b
            ze = (t3-a)/a
            weight = -((((-2*a)+t1+2*t3)*t4+(t2-2*b)*t3)/(a*b))/6.0d+0
         elseif(jj.eq.14)then 
            et = -(t4-b)/b
            ze = ((3*t3-t1-2*a)/a)/4.0d+0
            weight = -(((2*t3+2*t1-4*a)*t4)/(a*b))/3.0d+0
         elseif(jj.eq.15)then 
            et = -(t4-b)/b
            ze = ((t3-t1)/a)/2.0d+0
            weight = -(((t3+t1-2*a)*t4)/(a*b))/3.0d+0
         elseif(jj.eq.16)then 
            et = -(t4-b)/b
            ze = ((t3-3*t1+2*a)/a)/4.0d+0
            weight = -(((2*t3+2*t1-4*a)*t4)/(a*b))/3.0d+0
         endif
!
! >>> H-BEAM MODIFICATION START (Hanya menyisakan satu blok)
      elseif(lakonl(8:8).eq.'H') then
         if(kk.eq.0) then
            mint3d = 24
            return
         endif
         bf_top = prop(npropstart+1)
         tf_top = prop(npropstart+2)
         hw     = prop(npropstart+3)
         tw     = prop(npropstart+4)
         bf_bot = prop(npropstart+5)
         tf_bot = prop(npropstart+6)
         if(kk.le.8) then
            jj = kk
            et =  hw/2.d0 + tf_top/2.d0
            ze = (bf_top/4.d0) * (-1)**jj
            weight = (bf_top*tf_top)/4.d0
         elseif(kk.le.16) then
            jj = kk - 8
            et = 0.d0
            ze = (tw/4.d0) * (-1)**jj
            weight = (hw*tw)/8.d0
         else
            jj = kk - 16
            et = -hw/2.d0 - tf_bot/2.d0
            ze = (bf_bot/4.d0) * (-1)**jj
            weight = (bf_bot*tf_bot)/4.d0
         endif
         if(kk.le.12) then
            xi = -1.d0/dsqrt(3.d0)
         else
            xi =  1.d0/dsqrt(3.d0)
         endif
! >>> H-BEAM MODIFICATION END
!
! >>> L-ANGLE MODIFICATION START
      elseif(lakonl(8:8).eq.'L') then
         if(kk.eq.0) then
            mint3d = 16
            return
         endif
         b_angle = prop(npropstart+1)
         t_angle = prop(npropstart+2)
         if(kk.le.8) then
            xi = -1.d0/dsqrt(3.d0)
            jj = kk
         else
            xi =  1.d0/dsqrt(3.d0)
            jj = kk - 8
         endif
         if(jj.le.4) then
            et = t_angle/4.d0
            ze = b_angle/4.d0 + (jj-1)*b_angle/4.d0
            weight = (b_angle*t_angle)/4.d0
         else
            et = b_angle/4.d0 + (jj-5)*b_angle/4.d0 + t_angle/2.d0
            ze = t_angle/4.d0
            weight = (b_angle*t_angle)/4.d0
         endif
! >>> L-ANGLE MODIFICATION END
!
! >>> CHANNEL MODIFICATION START
      elseif(lakonl(8:8).eq.'C') then
         if(kk.eq.0) then
            mint3d = 24
            return
         endif
         bf_ch = prop(npropstart+1)
         tf_ch = prop(npropstart+2)
         hw_ch = prop(npropstart+3)
         tw_ch = prop(npropstart+4)
         if(kk.le.8) then
            jj = kk
            et =  hw_ch/2.d0 + tf_ch/2.d0
            ze = tw_ch/2.d0 + bf_ch/4.d0 * (mod(jj,2)*2-1)
            weight = (bf_ch*tf_ch)/4.d0
         elseif(kk.le.16) then
            jj = kk - 8
            et = 0.d0
            ze = tw_ch/4.d0 * (mod(jj,2)*2-1)
            weight = (hw_ch*tw_ch)/8.d0
         else
            jj = kk - 16
            et = -hw_ch/2.d0 - tf_ch/2.d0
            ze = tw_ch/2.d0 + bf_ch/4.d0 * (mod(jj,2)*2-1)
            weight = (bf_ch*tf_ch)/4.d0
         endif
         if(kk.le.12) then
            xi = -1.d0/dsqrt(3.d0)
         else
            xi =  1.d0/dsqrt(3.d0)
         endif
! >>> CHANNEL MODIFICATION END 
      endif
!
      return
      end