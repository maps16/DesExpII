Program EnergiaPromedio

  Implicit None
  Integer :: state10, state11
  Real :: dens74, dens1
  Real :: a74, b74, a1, b1
  Real :: u74, u1

  Open(10, file="a_starT1.dat", status="old", action="read")
  Open(11, file="a_starT74.dat", status="old", action="read")
  open(12, file="EnergyPromedio.dat", action="write")

  Read:Do
     
     Read(10,*, iostat=state1) dens1, a1, b1
     Read(11,*, iostat=state74) dens74, a74 , b74

     u1 = -dens1 * a1
     u74 = -dens74 * a74

     If((state10 == 0 .OR. State11 ==0) Exit 
     Write(12,*) dens1, u1, u74
     Write(*,*) dens1, u1, u74
     
  End Do Read

End Program EnergiaPromedio
