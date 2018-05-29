MODULE cte
Implicit None
real :: Ls, RCut, T, ji, eta, ns, tim
real :: varianza, sigma, Dt, U, U2, Du2
Integer, Parameter :: Nc = 512, NENER = 10000
Real, Parameter    :: Pi = 3.141592
Real, Parameter    :: Fi = 0.5
  Real, Allocatable, Dimension (:,:) :: CX, CY, CZ
  Real, Allocatable, Dimension (:,:) :: CXR, CYR, CZR
  Real, Allocatable, Dimension (:)   :: XR, YR, ZR
  Real, Allocatable, Dimension (:)   :: Fx, Fy, Fz
  Real, Allocatable, Dimension (:)   :: X, Y, Z
INTEGER :: NFREC, NFREC2, NSTEP, NN2
INTEGER :: KI, KI2


END MODULE cte
