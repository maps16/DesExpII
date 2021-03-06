Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real, Parameter :: PI=4*atan(1.0)
  Integer, Parameter :: CEq = 1000
  Integer, Parameter :: NNN = 2000
  Real ::  BoxL, RCut, dRMax!,  Dens
  Integer :: NN!, N, NStep, ISave, IPrint, IRatio,
  Real, Parameter :: Dim = 1.0/2.0         !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y
  Real, Allocatable, Dimension(:,:) :: CX, CY

  Integer, Parameter :: N = 100
  Integer, Parameter :: NStep = 10000
  Integer, Parameter :: Iprint = 1000
  Real, Parameter :: Dens=0.999
  Integer, Parameter :: ISave = 10
  Integer, Parameter :: IRatio = 10
End Module cte
