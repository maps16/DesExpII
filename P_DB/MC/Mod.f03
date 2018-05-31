Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real, Parameter :: T = 0.01
  Real, Parameter :: PI=4*atan(1.0)
  Integer, Parameter :: CEq = 1000
  Real :: Dens, BoxL, RCut, dRMax
  Integer :: NN!, NStep, ISave, IPrint, IRatio, NN
  Real, Parameter :: Dim = 1.0/3.0         !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y, Z
  Real, Allocatable, Dimension(:,:) :: CX, CY, CZ

  Integer, Parameter :: N = 100
  Real , Parameter :: Dens = 0.9549
  Integer, Parameter :: NStep = 50000
  Integer, Parameter :: ISave = 100
  Integer, Parameter :: IPrint = 1000
  Integer, Parameter :: IRatio = 100
End Module cte
