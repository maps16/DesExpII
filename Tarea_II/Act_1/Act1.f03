!=========================================================================================
! Realiza un calculo aproximado del valor de pi mediante la generacion aleatoria de puntos
!         que se localizan dentro de un circulo unitario
! Autor:  Martin Alejandro Paredes Sosa
!=========================================================================================
Module Cte
  Implicit None
  Integer :: m
  Real, Parameter :: Pi = 3.1415
  Integer,Parameter :: Sav = 5
End Module Cte
!==========================================
Subroutine InOutCircle(x, y )                  !Checar si se encuntra dentro del circulo unitario
  Use Cte
  Implicit None
  Real :: x , y
  Logical :: logic
  logic = x*x + y*y < 1
  If (logic) then
     m = m+1
  end If
End Subroutine InOutCircle
!=========================================
Program CalcPi
  Use Cte
  Implicit None
  Real :: xRan, yRan, PiCalc, ErrorRel
  Integer :: n, i, j, seed1, seed2
  Real,allocatable :: ValTemp(:)

  seed1 = 1365
  seed2 = 6482
  n = 1000
  allocate(ValTemp(n/sav))
  m=0
  j=0 
  call srand(seed1)
  Do i=1, n, 1
     xran = ran()
     yran = ran()
     call inoutcircle(xran,yran)

     If (mod(real(n),real(sav))==0 ) Then
        !Write(*,*) mod(n/sav)
        j=j+1
        Write(*,*) j
     End If

     

  end do
  PiCalc = 4*(real(m)/real(n))
  ErrorRel =(Pi -PiCalc)/Pi *100
  Write(*,*) m, n, PiCalc
!  Write(*,*) Valtemp
  Write(*,*) ErrorRel , "%"
End Program CalcPi
