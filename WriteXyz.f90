module WriteXyz
 Use AtomsModule
 Implicit None
 Contains
 Subroutine WriteOut(AtCoor,Natms)
 Integer Natms,i,j
 Type(Atoms),allocatable::AtCoor(:)
 OPEN(unit=2,file='Coords.xyz',status='unknown')
 write(2,*) Natms
 write(2,*) "Atoms"
 Do i=1,Natms
  Write(2,1001) AtCoor(i)%AtName,(AtCoor(i)%Coords(j),j=1,3)
 Enddo
 Close(2)
1001 format(A4,F12.6,F12.6,F12.6)
 End Subroutine
 End Module

