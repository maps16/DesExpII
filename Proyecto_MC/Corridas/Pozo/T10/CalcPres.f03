Program CalcPresion

  Implicit None
  Real, Parameter :: Pi=4.0*atan(1.0)
  Real, Parameter :: lamda = 1.25
  Real :: Dens
  Character :: Filename, cons
  
  Dens = 0.1
  CONC: Do while (Dens .LE. 1.0)
     Write(Cons,256) Dens
     Filename = "gdr"//trim(Cons)//".dat"
     Write(*,*) "Archivo: ",Filename


     Dens = Dens + 0.1
  End Do CONC
  

  

End Program CalcPresion
