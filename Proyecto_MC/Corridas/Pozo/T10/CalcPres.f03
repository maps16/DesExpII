Program CalcPresion

  Implicit None
  Real, Parameter :: Pi=4.0*atan(1.0)
  Real, Parameter :: lamda = 1.25
  Real :: Dens
  Real, allocatable, Dimension(:) :: r, gdr
  Real :: g1, glmin, glplu
  Integer :: Den, State
  Integer :: i ,j, k
  Character (len=11) :: Filename, Cons
  
  
  CONC: Do Den=1 , 10

     Write(Cons,256) Den
     Filename = "gdr"//trim(Cons)//".dat"
     Write(*,*) "Archivo: ",Filename
     Dens = Den * 0.1

     Open(3, file=filename)
     i=0
     ReadSize: Do

        Read(3,*, iostat=state)
        If ( state .LT. 0 ) Exit
        i=i+1
        
     End Do ReadSize
     !Write(*,*) i
     Rewind(3)
     Allocate( r(i), gdr(i) )

     Save: Do j=1, i
        
        Read(3,*) r(j) , gdr(j)
        !If ( state .LT. 0 ) Exit
        
     End Do Save
     Write(*,*) i
     FindG1 : Do j = 1 ,i
        k = j
        If( r(j) .GT. 1.0)  Exit
        
     End Do FindG1
     g1 = gdr(k)
     Write(*,*)k, r(k), g1








     

     Close(3)

     Deallocate(r,gdr)
     
  End Do CONC
  

  
256 Format (I2.2)
End Program CalcPresion
