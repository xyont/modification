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
      subroutine gen3dfrom1d(i,kon,ipkon,lakon,ne,iponor,xnor,knor,
     &  thicke,ntrans,inotr,trab,nk,nk_,co,offset,mi)
!
!     expands 1d element i into a 3d element
!
      implicit none
!
      character*8 lakon(*)
!
      integer mi(*),i,kon(*),ipkon(*),ne,iponor(2,*),knor(*),ntrans,
     &  inotr(2,*),nk,nk_,indexe,j,nodel(8),indexx,indexk,k,nodeb(8,3),
     &  nope,ishift
!
      real*8 xnor(*),thicke(mi(3),*),trab(7,*),co(3,*),offset(2,*),
     &  thickb(2,3),xnorb(6,3),sc
! >>> MODIFICATION START: deklarasi variabel tambahan (bersih)
      real*8 xdir(3),ydir(3),zdir(3),bf_top,tf_top,hw,tw,bf_bot,tf_bot
      real*8 b_angle,t_angle,bf_ch,tf_ch,hw_ch,tw_ch
! >>> MODIFICATION END
!
      indexe=ipkon(i)
!
!     check whether linear or quadratic
!
      if((lakon(i)(3:3).eq.'1').or.(lakon(i)(4:4).eq.'2')) then
         nope=2
         if(lakon(i)(4:4).eq.'R') then
            ishift=8
         else
            ishift=11
         endif
      elseif((lakon(i)(3:3).eq.'2').or.(lakon(i)(4:5).eq.'3')) then
         nope=3
         ishift=20
      endif
!
!     localizing the nodes, thicknesses and normals for the
!     beam element
!            
      do j=1,nope
         nodel(j)=kon(indexe+j)
         kon(indexe+ishift+j)=nodel(j)
         indexx=iponor(1,indexe+j)
         indexk=iponor(2,indexe+j)
         thickb(1,j)=thicke(1,indexe+j)
         thickb(2,j)=thicke(2,indexe+j)
         do k=1,6
            xnorb(k,j)=xnor(indexx+k)
         enddo
         do k=1,8
            nodeb(k,j)=knor(indexk+k)
         enddo
         if(ntrans.gt.0) then
            do k=1,8
               inotr(1,nodeb(k,j))=inotr(1,nodel(j))
            enddo
         endif
      enddo
!
!     generating the 3-D element topology for beam elements
!
      if(lakon(i)(8:8).ne.'C') then
!
!     >>> RECTANGULAR CROSS SECTION
!
         kon(indexe+1)=nodeb(1,1)
         do j=1,3
            co(j,nodeb(1,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &           +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
         enddo
         kon(indexe+2)=nodeb(1,nope)
         do j=1,3
            co(j,nodeb(1,nope))=co(j,nodel(nope))
     &           -thickb(1,nope)*xnorb(j,nope)*(.5d0+offset(1,i))
     &           +thickb(2,nope)*xnorb(j+3,nope)*(.5d0-offset(2,i))
         enddo
         kon(indexe+3)=nodeb(2,nope)
         do j=1,3
            co(j,nodeb(2,nope))=co(j,nodel(nope))
     &           -thickb(1,nope)*xnorb(j,nope)*(.5d0+offset(1,i))
     &           -thickb(2,nope)*xnorb(j+3,nope)*(.5d0+offset(2,i))
         enddo
         kon(indexe+4)=nodeb(2,1)
         do j=1,3
            co(j,nodeb(2,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
         enddo
         kon(indexe+5)=nodeb(4,1)
         do j=1,3
            co(j,nodeb(4,1))=co(j,nodel(1))
     &           +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &           +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
         enddo
         kon(indexe+6)=nodeb(4,nope)
         do j=1,3
            co(j,nodeb(4,nope))=co(j,nodel(nope))
     &           +thickb(1,nope)*xnorb(j,nope)*(.5d0-offset(1,i))
     &           +thickb(2,nope)*xnorb(j+3,nope)*(.5d0-offset(2,i))
         enddo
         kon(indexe+7)=nodeb(3,nope)
         do j=1,3
            co(j,nodeb(3,nope))=co(j,nodel(nope))
     &           +thickb(1,nope)*xnorb(j,nope)*(.5d0-offset(1,i))
     &           -thickb(2,nope)*xnorb(j+3,nope)*(.5d0+offset(2,i))
         enddo
         kon(indexe+8)=nodeb(3,1)
         do j=1,3
            co(j,nodeb(3,1))=co(j,nodel(1))
     &           +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
         enddo
!
         if(nope.eq.3) then
            kon(indexe+9)=nodeb(1,2)
            do j=1,3
               co(j,nodeb(1,2))=co(j,nodel(2))
     &              -thickb(1,2)*xnorb(j,2)*(.5d0+offset(1,i))
     &              +thickb(2,2)*xnorb(j+3,2)*(.5d0-offset(2,i))
            enddo
            kon(indexe+10)=nodeb(5,3)
            do j=1,3
               co(j,nodeb(5,3))=co(j,nodel(3))
     &              -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &              -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
            enddo
            kon(indexe+11)=nodeb(2,2)
            do j=1,3
               co(j,nodeb(2,2))=co(j,nodel(2))
     &              -thickb(1,2)*xnorb(j,2)*(.5d0+offset(1,i))
     &              -thickb(2,2)*xnorb(j+3,2)*(.5d0+offset(2,i))
            enddo
            kon(indexe+12)=nodeb(5,1)
            do j=1,3
               co(j,nodeb(5,1))=co(j,nodel(1))
     &              -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &              -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
            enddo
            kon(indexe+13)=nodeb(4,2)
            do j=1,3
               co(j,nodeb(4,2))=co(j,nodel(2))
     &              +thickb(1,2)*xnorb(j,2)*(.5d0-offset(1,i))
     &              +thickb(2,2)*xnorb(j+3,2)*(.5d0-offset(2,i))
            enddo
            kon(indexe+14)=nodeb(7,3)
            do j=1,3
               co(j,nodeb(7,3))=co(j,nodel(3))
     &              +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &              -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
            enddo
            kon(indexe+15)=nodeb(3,2)
            do j=1,3
               co(j,nodeb(3,2))=co(j,nodel(2))
     &              +thickb(1,2)*xnorb(j,2)*(.5d0-offset(1,i))
     &              -thickb(2,2)*xnorb(j+3,2)*(.5d0+offset(2,i))
            enddo
            kon(indexe+16)=nodeb(7,1)
            do j=1,3
               co(j,nodeb(7,1))=co(j,nodel(1))
     &              +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &              -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
            enddo
            kon(indexe+17)=nodeb(8,1)
            do j=1,3
               co(j,nodeb(8,1))=co(j,nodel(1))
     &              -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &              +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
            enddo
            kon(indexe+18)=nodeb(8,3)
            do j=1,3
               co(j,nodeb(8,3))=co(j,nodel(3))
     &              -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &              +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
            enddo
            kon(indexe+19)=nodeb(6,3)
            do j=1,3
               co(j,nodeb(6,3))=co(j,nodel(3))
     &              -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &              -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
            enddo
            kon(indexe+20)=nodeb(6,1)
            do j=1,3
               co(j,nodeb(6,1))=co(j,nodel(1))
     &              -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &              -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
            enddo
!
            do j=1,3
               co(j,nodeb(5,2))=co(j,nodel(2))
     &              -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &              -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
            enddo
            do j=1,3
               co(j,nodeb(7,2))=co(j,nodel(2))
     &              +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &              -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
            enddo
            do j=1,3
               co(j,nodeb(8,2))=co(j,nodel(2))
     &              -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &              +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
            enddo
            do j=1,3
               co(j,nodeb(6,2))=co(j,nodel(2))
     &              -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &              -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
            enddo
         endif
!
      else
!
!     >>> CIRCULAR CROSS SECTION
!
         if(nope.eq.2) then
            write(*,*) '*ERROR in gen3dfrom1d: element ',i,
     &        'is a linear beam element with circular cross section'
            write(*,*) '       Please use quadratic elements for beams
     &with circular cross section.'
            call exit(201)
         endif
!
         sc=.5d0/dsqrt(2.d0)
         kon(indexe+1)=nodeb(1,1)
         do j=1,3
            co(j,nodeb(1,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*(sc+offset(1,i))
     &           +thickb(2,1)*xnorb(j+3,1)*(sc-offset(2,i))
         enddo
         kon(indexe+2)=nodeb(1,3)
         do j=1,3
            co(j,nodeb(1,3))=co(j,nodel(3))
     &           -thickb(1,3)*xnorb(j,3)*(sc+offset(1,i))
     &           +thickb(2,3)*xnorb(j+3,3)*(sc-offset(2,i))
         enddo
         kon(indexe+3)=nodeb(2,3)
         do j=1,3
            co(j,nodeb(2,3))=co(j,nodel(3))
     &           -thickb(1,3)*xnorb(j,3)*(sc+offset(1,i))
     &           -thickb(2,3)*xnorb(j+3,3)*(sc+offset(2,i))
         enddo
         kon(indexe+4)=nodeb(2,1)
         do j=1,3
            co(j,nodeb(2,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*(sc+offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*(sc+offset(2,i))
         enddo
         kon(indexe+5)=nodeb(4,1)
         do j=1,3
            co(j,nodeb(4,1))=co(j,nodel(1))
     &           +thickb(1,1)*xnorb(j,1)*(sc-offset(1,i))
     &           +thickb(2,1)*xnorb(j+3,1)*(sc-offset(2,i))
         enddo
         kon(indexe+6)=nodeb(4,3)
         do j=1,3
            co(j,nodeb(4,3))=co(j,nodel(3))
     &           +thickb(1,3)*xnorb(j,3)*(sc-offset(1,i))
     &           +thickb(2,3)*xnorb(j+3,3)*(sc-offset(2,i))
         enddo
         kon(indexe+7)=nodeb(3,3)
         do j=1,3
            co(j,nodeb(3,3))=co(j,nodel(3))
     &           +thickb(1,3)*xnorb(j,3)*(sc-offset(1,i))
     &           -thickb(2,3)*xnorb(j+3,3)*(sc+offset(2,i))
         enddo
         kon(indexe+8)=nodeb(3,1)
         do j=1,3
            co(j,nodeb(3,1))=co(j,nodel(1))
     &           +thickb(1,1)*xnorb(j,1)*(sc-offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*(sc+offset(2,i))
         enddo
         kon(indexe+9)=nodeb(1,2)
         do j=1,3
            co(j,nodeb(1,2))=co(j,nodel(2))
     &           -thickb(1,2)*xnorb(j,2)*(sc+offset(1,i))
     &           +thickb(2,2)*xnorb(j+3,2)*(sc-offset(2,i))
         enddo
         kon(indexe+10)=nodeb(5,3)
         do j=1,3
            co(j,nodeb(5,3))=co(j,nodel(3))
     &           -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &           -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
         enddo
         kon(indexe+11)=nodeb(2,2)
         do j=1,3
            co(j,nodeb(2,2))=co(j,nodel(2))
     &           -thickb(1,2)*xnorb(j,2)*(sc+offset(1,i))
     &           -thickb(2,2)*xnorb(j+3,2)*(sc+offset(2,i))
         enddo
         kon(indexe+12)=nodeb(5,1)
         do j=1,3
            co(j,nodeb(5,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
         enddo
         kon(indexe+13)=nodeb(4,2)
         do j=1,3
            co(j,nodeb(4,2))=co(j,nodel(2))
     &           +thickb(1,2)*xnorb(j,2)*(sc-offset(1,i))
     &           +thickb(2,2)*xnorb(j+3,2)*(sc-offset(2,i))
         enddo
         kon(indexe+14)=nodeb(7,3)
         do j=1,3
            co(j,nodeb(7,3))=co(j,nodel(3))
     &           +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &           -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
         enddo
         kon(indexe+15)=nodeb(3,2)
         do j=1,3
            co(j,nodeb(3,2))=co(j,nodel(2))
     &           +thickb(1,2)*xnorb(j,2)*(sc-offset(1,i))
     &           -thickb(2,2)*xnorb(j+3,2)*(sc+offset(2,i))
         enddo
         kon(indexe+16)=nodeb(7,1)
         do j=1,3
            co(j,nodeb(7,1))=co(j,nodel(1))
     &           +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
         enddo
         kon(indexe+17)=nodeb(8,1)
         do j=1,3
            co(j,nodeb(8,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &           +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
         enddo
         kon(indexe+18)=nodeb(8,3)
         do j=1,3
            co(j,nodeb(8,3))=co(j,nodel(3))
     &           -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &           +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
         enddo
         kon(indexe+19)=nodeb(6,3)
         do j=1,3
            co(j,nodeb(6,3))=co(j,nodel(3))
     &           -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &           -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
         enddo
         kon(indexe+20)=nodeb(6,1)
         do j=1,3
            co(j,nodeb(6,1))=co(j,nodel(1))
     &           -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &           -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
         enddo
!
         do j=1,3
            co(j,nodeb(5,2))=co(j,nodel(2))
     &           -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
         enddo
         do j=1,3
            co(j,nodeb(7,2))=co(j,nodel(2))
     &           +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &           -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
         enddo
         do j=1,3
            co(j,nodeb(8,2))=co(j,nodel(2))
     &           -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &           +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
         enddo
         do j=1,3
            co(j,nodeb(6,2))=co(j,nodel(2))
     &           -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &           -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
         enddo
!
! >>> H-BEAM MODIFICATION START
      elseif(lakon(i)(8:8).eq.'H') then
         bf_top = thickb(1,1)
         hw     = thickb(2,1)
         tf_top = 0.1d0 * hw
         tw     = 0.1d0 * hw
         bf_bot = bf_top
         tf_bot = tf_top
         do j=1,3
            xdir(j) = xnorb(j,1)
            ydir(j) = xnorb(j+3,1)
         enddo
         zdir(1) = xdir(2)*ydir(3) - xdir(3)*ydir(2)
         zdir(2) = xdir(3)*ydir(1) - xdir(1)*ydir(3)
         zdir(3) = xdir(1)*ydir(2) - xdir(2)*ydir(1)
         
         kon(indexe+1)=nodeb(1,1)
         do j=1,3
            co(j,nodeb(1,1))=co(j,nodel(1)) -bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
         enddo
         kon(indexe+2)=nodeb(1,nope)
         do j=1,3
            co(j,nodeb(1,nope))=co(j,nodel(nope)) -bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
         enddo
         kon(indexe+3)=nodeb(2,nope)
         do j=1,3
            co(j,nodeb(2,nope))=co(j,nodel(nope)) +bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
         enddo
         kon(indexe+4)=nodeb(2,1)
         do j=1,3
            co(j,nodeb(2,1))=co(j,nodel(1)) +bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
         enddo
         kon(indexe+5)=nodeb(4,1)
         do j=1,3
            co(j,nodeb(4,1))=co(j,nodel(1)) -bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
         enddo
         kon(indexe+6)=nodeb(4,nope)
         do j=1,3
            co(j,nodeb(4,nope))=co(j,nodel(nope)) -bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
         enddo
         kon(indexe+7)=nodeb(3,nope)
         do j=1,3
            co(j,nodeb(3,nope))=co(j,nodel(nope)) +bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
         enddo
         kon(indexe+8)=nodeb(3,1)
         do j=1,3
            co(j,nodeb(3,1))=co(j,nodel(1)) +bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
         enddo
         
         if(nope.eq.3) then
            kon(indexe+9)=nodeb(1,2)
            do j=1,3
               co(j,nodeb(1,2))=co(j,nodel(2)) -bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
            enddo
            kon(indexe+10)=nodeb(5,3)
            do j=1,3
               co(j,nodeb(5,3))=co(j,nodel(3)) +(hw/2.d0+tf_top/2.d0)*ydir(j)
            enddo
            kon(indexe+11)=nodeb(2,2)
            do j=1,3
               co(j,nodeb(2,2))=co(j,nodel(2)) +bf_top/2.d0*zdir(j) +(hw/2.d0+tf_top)*ydir(j)
            enddo
            kon(indexe+12)=nodeb(5,1)
            do j=1,3
               co(j,nodeb(5,1))=co(j,nodel(1)) +(hw/2.d0+tf_top/2.d0)*ydir(j)
            enddo
            kon(indexe+13)=nodeb(4,2)
            do j=1,3
               co(j,nodeb(4,2))=co(j,nodel(2)) -bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
            enddo
            kon(indexe+14)=nodeb(7,3)
            do j=1,3
               co(j,nodeb(7,3))=co(j,nodel(3)) -(hw/2.d0+tf_bot/2.d0)*ydir(j)
            enddo
            kon(indexe+15)=nodeb(3,2)
            do j=1,3
               co(j,nodeb(3,2))=co(j,nodel(2)) +bf_bot/2.d0*zdir(j) -(hw/2.d0+tf_bot)*ydir(j)
            enddo
            kon(indexe+16)=nodeb(7,1)
            do j=1,3
               co(j,nodeb(7,1))=co(j,nodel(1)) -(hw/2.d0+tf_bot/2.d0)*ydir(j)
            enddo
            kon(indexe+17)=nodeb(8,1)
            do j=1,3
               co(j,nodeb(8,1))=co(j,nodel(1)) +tw/2.d0*zdir(j) +(hw/2.d0)*ydir(j)
            enddo
            kon(indexe+18)=nodeb(8,3)
            do j=1,3
               co(j,nodeb(8,3))=co(j,nodel(3)) +tw/2.d0*zdir(j) +(hw/2.d0)*ydir(j)
            enddo
            kon(indexe+19)=nodeb(6,3)
            do j=1,3
               co(j,nodeb(6,3))=co(j,nodel(3)) -tw/2.d0*zdir(j) +(hw/2.d0)*ydir(j)
            enddo
            kon(indexe+20)=nodeb(6,1)
            do j=1,3
               co(j,nodeb(6,1))=co(j,nodel(1)) -tw/2.d0*zdir(j) +(hw/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(5,2))=co(j,nodel(2)) +(hw/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(7,2))=co(j,nodel(2)) -(hw/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(8,2))=co(j,nodel(2)) +tw/2.d0*zdir(j)
            enddo
            do j=1,3
               co(j,nodeb(6,2))=co(j,nodel(2)) -tw/2.d0*zdir(j)
            enddo
         endif
! >>> H-BEAM MODIFICATION END
!
! >>> L-ANGLE MODIFICATION START
      elseif(lakon(i)(8:8).eq.'L') then
         b_angle = thickb(1,1)
         t_angle = thickb(2,1)
         do j=1,3
            xdir(j) = xnorb(j,1)
            ydir(j) = xnorb(j+3,1)
         enddo
         zdir(
         zdir(1) = xdir(2)*ydir(3) - xdir(3)*ydir(2)
         zdir(2) = xdir(3)*ydir(1) - xdir(1)*ydir(3)
         zdir(3) = xdir(1)*ydir(2) - xdir(2)*ydir(1)
         
         ! 8 NODE SUDUT
         kon(indexe+1)=nodeb(1,1)
         do j=1,3
            co(j,nodeb(1,1))=co(j,nodel(1)) +(t_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
         enddo
         kon(indexe+2)=nodeb(1,nope)
         do j=1,3
            co(j,nodeb(1,nope))=co(j,nodel(nope)) +(t_angle/2.d0)*ydir(j) +(b_angle)*zdir(j)
         enddo
         kon(indexe+3)=nodeb(2,nope)
         do j=1,3
            co(j,nodeb(2,nope))=co(j,nodel(nope)) +(b_angle)*ydir(j) +(b_angle)*zdir(j)
         enddo
         kon(indexe+4)=nodeb(2,1)
         do j=1,3
            co(j,nodeb(2,1))=co(j,nodel(1)) +(b_angle)*ydir(j) +(t_angle/2.d0)*zdir(j)
         enddo
         kon(indexe+5)=nodeb(4,1)
         do j=1,3
            co(j,nodeb(4,1))=co(j,nodel(1)) +(t_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
         enddo
         kon(indexe+6)=nodeb(4,nope)
         do j=1,3
            co(j,nodeb(4,nope))=co(j,nodel(nope)) +(t_angle/2.d0)*ydir(j) +(b_angle)*zdir(j)
         enddo
         kon(indexe+7)=nodeb(3,nope)
         do j=1,3
            co(j,nodeb(3,nope))=co(j,nodel(nope)) +(b_angle)*ydir(j) +(b_angle)*zdir(j)
         enddo
         kon(indexe+8)=nodeb(3,1)
         do j=1,3
            co(j,nodeb(3,1))=co(j,nodel(1)) +(b_angle)*ydir(j) +(t_angle/2.d0)*zdir(j)
         enddo
         
         ! NODE TENGAH (9-20)
         if(nope.eq.3) then
            kon(indexe+9)=nodeb(1,2)
            do j=1,3
               co(j,nodeb(1,2))=co(j,nodel(2)) +(t_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+10)=nodeb(5,3)
            do j=1,3
               co(j,nodeb(5,3))=co(j,nodel(3)) +(b_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+11)=nodeb(2,2)
            do j=1,3
               co(j,nodeb(2,2))=co(j,nodel(2)) +(t_angle/2.d0)*ydir(j) +(b_angle)*zdir(j)
            enddo
            kon(indexe+12)=nodeb(5,1)
            do j=1,3
               co(j,nodeb(5,1))=co(j,nodel(1)) +(b_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+13)=nodeb(4,2)
            do j=1,3
               co(j,nodeb(4,2))=co(j,nodel(2)) +(b_angle)*ydir(j) +(t_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+14)=nodeb(7,3)
            do j=1,3
               co(j,nodeb(7,3))=co(j,nodel(3)) +(b_angle)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+15)=nodeb(3,2)
            do j=1,3
               co(j,nodeb(3,2))=co(j,nodel(2)) +(b_angle)*ydir(j) +(b_angle)*zdir(j)
            enddo
            kon(indexe+16)=nodeb(7,1)
            do j=1,3
               co(j,nodeb(7,1))=co(j,nodel(1)) +(b_angle)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+17)=nodeb(8,1)
            do j=1,3
               co(j,nodeb(8,1))=co(j,nodel(1)) +(t_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+18)=nodeb(8,3)
            do j=1,3
               co(j,nodeb(8,3))=co(j,nodel(3)) +(t_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+19)=nodeb(6,3)
            do j=1,3
               co(j,nodeb(6,3))=co(j,nodel(3)) +(b_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
            enddo
            kon(indexe+20)=nodeb(6,1)
            do j=1,3
               co(j,nodeb(6,1))=co(j,nodel(1)) +(b_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
            enddo
            ! Node knot untuk L-Angle
            do j=1,3
               co(j,nodeb(5,2))=co(j,nodel(2)) +(b_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            do j=1,3
               co(j,nodeb(7,2))=co(j,nodel(2)) +(b_angle)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            do j=1,3
               co(j,nodeb(8,2))=co(j,nodel(2)) +(t_angle/2.d0)*ydir(j) +(b_angle/2.d0)*zdir(j)
            enddo
            do j=1,3
               co(j,nodeb(6,2))=co(j,nodel(2)) +(b_angle/2.d0)*ydir(j) +(t_angle/2.d0)*zdir(j)
            enddo
         endif
! >>> L-ANGLE MODIFICATION END
!
! >>> CHANNEL MODIFICATION START
      elseif(lakon(i)(8:8).eq.'C') then
         bf_ch = thickb(1,1)
         hw_ch = thickb(2,1)
         tf_ch = 0.1d0 * hw_ch
         tw_ch = 0.1d0 * hw_ch
         do j=1,3
            xdir(j) = xnorb(j,1)
            ydir(j) = xnorb(j+3,1)
         enddo
         zdir(1) = xdir(2)*ydir(3) - xdir(3)*ydir(2)
         zdir(2) = xdir(3)*ydir(1) - xdir(1)*ydir(3)
         zdir(3) = xdir(1)*ydir(2) - xdir(2)*ydir(1)
         
         kon(indexe+1)=nodeb(1,1)
         do j=1,3
            co(j,nodeb(1,1))=co(j,nodel(1)) + (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+2)=nodeb(1,nope)
         do j=1,3
            co(j,nodeb(1,nope))=co(j,nodel(nope)) + (tw_ch/2.d0+bf_ch)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+3)=nodeb(2,nope)
         do j=1,3
            co(j,nodeb(2,nope))=co(j,nodel(nope)) + (tw_ch/2.d0+bf_ch)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+4)=nodeb(2,1)
         do j=1,3
            co(j,nodeb(2,1))=co(j,nodel(1)) + (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+5)=nodeb(4,1)
         do j=1,3
            co(j,nodeb(4,1))=co(j,nodel(1)) - (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+6)=nodeb(4,nope)
         do j=1,3
            co(j,nodeb(4,nope))=co(j,nodel(nope)) - (tw_ch/2.d0+bf_ch)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+7)=nodeb(3,nope)
         do j=1,3
            co(j,nodeb(3,nope))=co(j,nodel(nope)) - (tw_ch/2.d0+bf_ch)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         kon(indexe+8)=nodeb(3,1)
         do j=1,3
            co(j,nodeb(3,1))=co(j,nodel(1)) - (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
         enddo
         
         ! NODE TENGAH (9-20) untuk Channel
         if(nope.eq.3) then
            kon(indexe+9)=nodeb(1,2)
            do j=1,3
               co(j,nodeb(1,2))=co(j,nodel(2)) + (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
            enddo
            kon(indexe+10)=nodeb(5,3)
            do j=1,3
               co(j,nodeb(5,3))=co(j,nodel(3)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+11)=nodeb(2,2)
            do j=1,3
               co(j,nodeb(2,2))=co(j,nodel(2)) + (tw_ch/2.d0+bf_ch)*zdir(j) + (hw_ch/2.d0+tf_ch)*ydir(j)
            enddo
            kon(indexe+12)=nodeb(5,1)
            do j=1,3
               co(j,nodeb(5,1))=co(j,nodel(1)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+13)=nodeb(4,2)
            do j=1,3
               co(j,nodeb(4,2))=co(j,nodel(2)) + (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
            enddo
            kon(indexe+14)=nodeb(7,3)
            do j=1,3
               co(j,nodeb(7,3))=co(j,nodel(3)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+15)=nodeb(3,2)
            do j=1,3
               co(j,nodeb(3,2))=co(j,nodel(2)) + (tw_ch/2.d0+bf_ch)*zdir(j) - (hw_ch/2.d0+tf_ch)*ydir(j)
            enddo
            kon(indexe+16)=nodeb(7,1)
            do j=1,3
               co(j,nodeb(7,1))=co(j,nodel(1)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+17)=nodeb(8,1)
            do j=1,3
               co(j,nodeb(8,1))=co(j,nodel(1)) + (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+18)=nodeb(8,3)
            do j=1,3
               co(j,nodeb(8,3))=co(j,nodel(3)) + (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+19)=nodeb(6,3)
            do j=1,3
               co(j,nodeb(6,3))=co(j,nodel(3)) + (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0)*ydir(j)
            enddo
            kon(indexe+20)=nodeb(6,1)
            do j=1,3
               co(j,nodeb(6,1))=co(j,nodel(1)) + (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0)*ydir(j)
            enddo
            ! Node knot untuk Channel
            do j=1,3
               co(j,nodeb(5,2))=co(j,nodel(2)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) + (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(7,2))=co(j,nodel(2)) + (tw_ch/2.d0+bf_ch/2.d0)*zdir(j) - (hw_ch/2.d0+tf_ch/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(8,2))=co(j,nodel(2)) + (tw_ch/2.d0)*zdir(j) + (hw_ch/2.d0)*ydir(j)
            enddo
            do j=1,3
               co(j,nodeb(6,2))=co(j,nodel(2)) + (tw_ch/2.d0)*zdir(j) - (hw_ch/2.d0)*ydir(j)
            enddo
         endif
! >>> CHANNEL MODIFICATION END
      endif
!
      return
      end