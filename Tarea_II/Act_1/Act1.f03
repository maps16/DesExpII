!=========================================================================================
! Realiza un calculo aproximado del valor de pi mediante la generacion aleatoria de puntos
!         que se localizan dentro de un circulo unitario
! Autor:  Martin Alejandro Paredes Sosa
!=========================================================================================
Module Cte
  Implicit None
  integer :: m
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
  Real :: xRan, yRan, PiCalc
  Integer :: n, i, seed1, seed2

  seed1 = 1365
  seed2 = 6482
  n = 1000000
  m = 0
  call srand(seed1)
  Do i=1, n, 1
     xran = ran()
     yran = ran()
     call inoutcircle(xran,yran)
!     write(*,*) xran, yran
  end do
  PiCalc = 4*(real(m)/real(n))
  Write(*,*) m, n, PiCalc
End Program CalcPi
