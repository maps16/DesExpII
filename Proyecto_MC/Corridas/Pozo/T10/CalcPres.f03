Program CalcPresion

  Implicit None
  Real, Parameter :: Pi=4.0*atan(1.0)
  Real, Parameter :: lamda = 1.25
  Real :: Dens
  Real, allocatable, Dimension(:) :: r, gdr
  Integer :: Den, State
  Integer :: i ,j
  Character (len=11) :: Filename, Cons
  
  
  CONC: Do Den=1 , 10

     Write(Cons,256) Den
     Filename = "gdr"//trim(Cons)//".dat"
     Write(*,*) "Archivo: ",Filename
     Dens = Den * 0.1

     Open(3, file=filename, status="old", action="read")
     i=0
     ReadSize: Do
        Read(3,*, iostat=state)
        i=i+1
        If ( state .LT. 0 ) Exit
     End Do ReadSize
     Write(*,*) i




     Close(3)

     
  End Do CONC
  

  
256 Format (I2.2)
End Program CalcPresion
