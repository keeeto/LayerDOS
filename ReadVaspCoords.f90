module ReadVaspCoords
 Use AtomsModule
 Implicit None
 Contains
 Subroutine ReadCoords(Infile,AtCoor,Ntypes,TotNum,LATTICE)
 INTEGER :: i,j,k,l,m,Ntypes,Natoms(10)
 INTEGER ::TotNum
 REAL :: LATTICE(3,3),ScaleF
 Character(len=2):: CTypes,Names(10)
 Character(len=7)::Infile
 Type (Atoms), allocatable :: AtCoor(:) 

 Open(Unit=1,file=Infile,status='unknown')
 m=0
 TotNum=0
 Read (1,*) (Names(i),i=1,Ntypes)
 Read (1,*) ScaleF
 Do i=1,3
  Read (1,*)(LATTICE(i,j),j=1,3)
 Enddo 
 Read (1,*) (Natoms(i),i=1,Ntypes)
 Do i=1,Ntypes
 TotNum=TotNum+Natoms(i)
 Enddo
 allocate(AtCoor(TotNum))
 Read (1,*) CTypes
 Do i=1,Ntypes
  Do j=1,Natoms(i)
   m=m+1
   AtCoor(m)%AtName=Names(i)
   Read (1,*) (AtCoor(m)%Coords(k),k=1,3)
  Enddo
 Enddo
 IF(CTypes.EQ."Di")Then
 Do i=1,TotNum
 Do j=1,3
  AtCoor(i)%Coords(j)=AtCoor(i)%Coords(1)*LATTICE(j,1)+ &
  AtCoor(i)%Coords(2)*LATTICE(j,2)+ &
  AtCoor(i)%Coords(3)*LATTICE(j,3)
 Enddo
 Enddo
 Endif 
 Close(1)
 End Subroutine
 End module ReadVaspCoords
