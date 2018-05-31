Program CalcPresion

  Implicit None
  Real, Parameter :: Pi=4.0*atan(1.0)
  Real, Parameter :: lamda = 1.25
  Real :: Dens
  Real, allocatable, Dimension(:) :: r, gdr
  Real :: g1, glmin, glplu
  Real :: P
  Integer :: Den, State
  Integer :: i ,j, k
  Character (len=11) :: Filename, Cons
  
  Open(15, file="Presion.dat")
  
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
     
     FindG1 : Do j = 1 ,i
        k = j
        If( r(j) .GE. 1.0)  Exit
        
     End Do FindG1
     g1 = gdr(k)
     
     FindGl : Do j = 1 ,i
        k = j
        If( r(j) .GE. lamda  )  Exit
        
     End Do FindGl
     glmin = gdr(k-1)
     glplu = gdr(k)

     !Write(*,*) g1 , glmin, glplu
     
     P = dens + (2*Pi/3)*dens*dens * ( g1 + lamda*lamda*lamda*( glplu - glmin )  )
     Write(15*,*) P


     Close(3)

     Deallocate(r,gdr)
     
  End Do CONC
  

  
256 Format (I2.2)
End Program CalcPresion
