Program CalcPressZ

  Implicit None
  Real, Parameter :: PI =4*atan(1.0)
  Real :: Dens
  Real :: a, b, adev
  Real :: phs, pA, p
  Real :: trash
  Integer :: State

  Open(14, file="PrssZwanzig.dat", action="write")
  Open(15, file="b.dat", action="read", status="old")
  
  Read :Do
     Read(15,*,iostat = state) dens, trash , b
     
     a = 1.99 + 3.35*dens - 0.21*dens*dens
     adev = 3.35 - 0.42*dens
     !b= (2*Pi)/3 * ghs1

     phs = dens * ( 1 + dens* b  ) 
     pA = dens*dens*( a + dens* adev  )

     P = phs - pA
     Write(*,*) dens, p
     Write(14,*) dens , P
     If ( state .LT. 0 ) Exit
  End Do Read

End Program CalcPressZ
