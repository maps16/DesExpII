Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real, Parameter :: PI=4*atan(1.0)
  Real :: Dens, BoxL, RCut, dRMax
  Integer :: N, NStep, ISave, IPrint, IRatio
  Real, Parameter :: Dim = 1.0/2.0         !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y
End Module cte