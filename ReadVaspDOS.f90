module ReadVaspDOS
 
 Implicit None
 contains
 subroutine ReadDOS(TDOS,PDOS,Natms)
 INTEGER Natms,i,j,k,l,m
 REAL,allocatable::TDOS(:,:),PDOS(:,:,:)
 Character(len=1)::junk 
 Integer LayerList(100,100)

 Allocate(TDOS(301,3))
 Allocate(PDOS(NAtms,301,4))
 open(unit=3,file="DOSCAR",status='unknown')
 do i=1,6
 read(3,*) Junk 
 enddo
 do i=1,301
  READ(3,*)(TDOS(i,j),j=1,3)
 enddo
 do i=1,Natms
  READ(3,*)Junk
   do j=1,301
    READ(3,*)(PDOS(i,j,k),k=1,4)
   enddo
 enddo
 end subroutine
end module
