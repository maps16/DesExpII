Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real :: Dens, BoxL
  Integer :: N
  Real, Parameter :: Dim = 1.0/2.0 !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y
End Module cte
