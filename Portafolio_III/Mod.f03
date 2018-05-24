Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0                                   !DIAMETRO DE LAS PARTICULAS
  Real, Parameter :: PI=4.0*atan(1.0)                              !PI
  Real :: YukA, YukZk                                              !POTENCIAL YUKAWA
  Integer, Parameter :: CEq = 11000                                !CONFIGURACION DE EQUILIBRIO
!  Integer, Parameter :: NNN = 2000
  Real :: Dens, BoxL, RCut, dT                                     !PARAMETROS DE LA SIMULACION (REALES)
  Integer :: N, NStep, ISave, ISave2, IPrint, IRatio, NN           !PARAMETROS DE LA SIMULACION (ENTEROS)
  Real, Parameter :: Dim = 1.0/3.0                                 !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y, Z, XR, YR, ZR           !POSICIONES DE LAS PARTICULAS X Y Z
  Real, Allocatable, Dimension(:,:) :: CX, CY, CZ                  !MATRICES DE CONFIGURACION GR
  Real, Allocatable, Dimension(:,:) :: CXD, CYD, CZD               !MATRICES DE CONFIGURACION WDT
  Real, Allocatable, Dimension(:) :: FX, FY, FZ                    !FUERZAS DE INTERACCION
End Module cte
