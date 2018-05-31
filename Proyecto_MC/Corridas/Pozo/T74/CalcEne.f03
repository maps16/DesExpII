Program CalcEne

  Implicit None
  Real :: Dens
  Real, allocatable, Dimension(:) :: step, V
  Real :: Vpot, VMedia
  Integer, Parameter :: Ceq = 100000
  Integer :: Den, State
  Integer :: i ,j, k, N
  Character (len=14) :: Filename, Cons
  
  Open(15, file="Energia.dat")
  
  CONC: Do Den=1 , 10

     Write(Cons,256) Den
     Filename = "Terma"//trim(Cons)//".dat"
     Write(*,*) "Archivo: ",Filename
     Dens = Den * 0.1

     Open(3, file=filename)
     i=0
     ReadSize: Do

        Read(3,*, iostat=state)
        If ( state .LT. 0 ) Exit
        i=i+1
        
     End Do ReadSize
     Write(*,*) i

     Rewind(3)
     
     Allocate( step(i), V(i) )

     Save: Do j=1, i
        
        Read(3,*) step(j) , V(j)
        !If ( state .LT. 0 ) Exit
        
     End Do Save
     
     Suma : Do j = Ceq ,i

        Vpot = Vpot + V(j)
        
     End Do Suma

     N= i-ceq
     VMedia = Vpot / N

     Write(15,*) Dens, VMedia

     Close(3)

     Deallocate(step, V)
     
  End Do CONC
  

  
256 Format (I2.2)
End Program CalcEne
