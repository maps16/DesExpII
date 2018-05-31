Subroutine GR

  Use cte

  Implicit None

  Integer, Allocatable, Dimension(:) :: Hist

  Real, Parameter :: delTar = 0.1
  Integer :: RMAX, CRMAX
  Integer :: i, j, k
  Real    :: x0, y0, z0, xN, yN, zN
  Real    :: x0N, y0N, z0N, GDR
  Real    :: rD, rU, rL, rM, c1, c2, c2d
  Integer :: istat1
  
  Character (len = 80) :: err_msg1
  

  RMAX = Int(RCut/delTar)
  
  Allocate( Hist(Rmax+100) , STAT = istat1, ERRMSG = err_msg1)
  Hist = 0
  write(*,*) "Whatevs"
  Do i = 1, Nc

     Do j = 1, Nc
        
        If (i /= j) Then

           Do k = 1, NN2
              

              x0 = CX(i, k)
              y0 = CY(i, k)
              z0 = CZ(i, k)

              xN = CX(j, k)
              yN = CY(j, k)
              zN = CZ(j, k)
              
              x0N = xN - x0
              y0N = yN - y0
              z0N = zN - z0

              x0N = x0N - Ls * Anint(x0N/Ls)
              y0N = y0N - Ls * Anint(y0N/Ls)
              z0N = z0N - Ls * Anint(z0N/Ls)

              rD = sqrt( (x0N * x0N) + (y0N * y0N) + (z0N * z0N) )

              CRMAX = Int(rD/delTar) + 1

              If((CRMAX .LE. RMAX)) Then

                 Hist(CRMAX) = Hist(CRMAX) + 1
                 
              End If

           End Do

        End If

     End Do

  End Do
  !write(*,*) "Whatevs"
  c1 = (4.0/3.0) * Pi * ns

  Open(60, File = "GDR.txt")

  Do CRMAX = 1, RMAX
     rL = Real(CRMAX - 1) * delTar
     rU = RL + delTar
     rM = RL + (delTar/2.0)

     c2 = c1 * ((rU ** 3) - (rL ** 3))
     c2d = Real(Hist(CRMAX))/(Real(NN2) * Real(Nc))

     GDR =  c2d/c2

     Write(60,*) rM, GDR

  End Do

  Close(60)

  Deallocate( Hist )

End Subroutine GR
