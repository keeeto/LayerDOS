 Program PDosPlot
 Use ReadVaspCoords 
 Use WriteXyz
 Use AtomsModule
 Use idlayers
 Use ReadVaspDOS
 Implicit None
 Character(len=7)::Infile
 REAL::LATTICE(3,3)
 REAL,allocatable::TDOS(:,:),PDOS(:,:,:),LocDOS(:,:,:)
 Integer:: NTypes,NAtoms,Nlayers,LayerList(100,100)
 type (Atoms), allocatable:: Coordinates(:)
 Integer i,j,k

 Print *, "Name of Coordinate File:"
 Read(*,*) Infile
 Print *, "Number of Atom types:"
 REad(*,*) Ntypes
 call ReadCoords(Infile,Coordinates,Ntypes,NAtoms,LATTICE) 
 print *, "Finished Reading"
 call WriteOut(Coordinates,Natoms)
 print *, Natoms
 call Laters(Coordinates,Natoms,LATTICE,NLayers,LayerList)
 call ReadDOS(TDOS,PDOS,NAtoms)
 call LDOS(PDOS,LayerList,NLayers,NAtoms,LocDOS)

  open(Unit=4,file="l.DOS",Status='Unknown')
 Do i=1,Nlayers
  If(LayerList(i,1).NE.0)THEN
  write(4,*)"-----LAYER",i,"DOS-----"
  Do j=1,301
  write(4,*)(LocDOS(i,j,k),k=1,4)
  Enddo
  ENDIF
 Enddo
  close(4) 
 END Program
