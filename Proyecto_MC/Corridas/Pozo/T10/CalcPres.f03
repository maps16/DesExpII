Program CalcPresion

  Implicit None
  Real, Parameter :: Pi=4.0*atan(1.0)
  Real, Parameter :: lamda = 1.25
  Real :: Dens
  Integer :: Den
  Character (len=11) :: Filename, Cons
  
  
  CONC: Do Den=1 , 10
     Write(*,256) Den
     Write(Cons,256) Den
     Filename = "gdr"//trim(Cons)//".dat"
     Write(*,*) "Archivo: ",Filename
     Dens = Den * 0.1

     
  End Do CONC
  

  
256 Format (I2.2)
End Program CalcPresion
