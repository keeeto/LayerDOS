module idlayers
 Use AtomsModule
 Implicit none
 Contains
 Subroutine Laters(AtCoor,Natms,LATTICE,Nlayers,LayerList)
 Integer i,j,k,l,Natms,Nlayers
 Integer LayerList(100,100)              ! Contains the index of atoms in layer N
                                         ! First number is the NAtms in the layer
 Real:: bin_width,startlayer,LATTICE(3,3)
 type(Atoms),allocatable:: AtCoor(:)

 print *, "Bin Width:"
 read(*,*) bin_width
 Nlayers=NINT(LATTICE(3,3)/bin_width)
 Do j=0,NINT(LATTICE(3,3)/bin_width)
 LayerList(j,1)=0
 Do i=1,natms
 if(AtCoor(i)%Coords(3).GE.(bin_width*(j)))then
 if(AtCoor(i)%Coords(3).LT.(bin_width*(j+1)))then
  LayerList(j,1)=LayerList(j,1)+1
  LayerList(j,LayerList(j,1)+1)=i
 endif
 endif
 Enddo
 Enddo
 Do j=0,NINT(LATTICE(3,3)/bin_width)
 PRINT *, LayerList(j,1)
 Enddo
 End Subroutine

 Subroutine LDOS(PDOS,LayerList,Nlayers,NAtms,LocDOS)
 Integer i,j,k,l,NLayers,Natms,m
 Integer LayerList(100,100)
 REAL,allocatable::PDOS(:,:,:),LocDOS(:,:,:)

  Allocate(LocDos(Nlayers,301,4))

 DO i=1,NLayers
  if(LayerList(i,1).NE.0)Then
   do j=2,LayerList(i,1)+1
    do k=1,301
     m=LayerList(i,j)
     LocDOS(i,k,1)=PDOS(m,k,1)
     LocDOS(i,k,2)=LocDOS(i,k,2)+PDOS(m,k,2)
     LocDOS(i,k,3)=LocDOS(i,k,3)+PDOS(m,k,3)
    enddo
   enddo
   do k=1,301
    LocDOS(i,k,2)=LocDOS(i,k,2)/LayerList(i,1)
    LocDOS(i,k,3)=LocDOS(i,k,3)/LayerList(i,1)
   enddo
  endif
 Enddo

 end Subroutine
end module
