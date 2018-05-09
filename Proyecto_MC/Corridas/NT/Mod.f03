Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0                                   
  Real, Parameter :: PI=4*atan(1.0)
  Real, Parameter :: Lambda =  1.25                                !TAMANO DEL POZO
  Real, Parameter :: TP = 1.0                                      !TEMPERATURA REDUCIDA
  Integer, Parameter :: CEq = 1000                                 !CONFIG DE EQUILIBRIO
!  Integer, Parameter :: NNN = 2000
  Real ::  BoxL, RCut, dRMax
  Integer :: NN
  Real, Parameter :: Dim = 1.0/3.0                                 !DIMENSIONES (3D)
  Real, Allocatable, Dimension(:) :: X, Y, Z                       !POSICIONES DE LAS PARTICULAS
  Real, Allocatable, Dimension(:,:) :: CX, CY, CZ                  !MATRICES DE CONFIGURACION

  !VARIABLES DE SIMULACION
  Integer , Parameter :: N = 64
  Integer, Parameter :: NStep = 1000                              !NSTEP - CEq CONFIG DE EQUILIBRIO
  Integer, Parameter :: IPrint = 1000
  Integer, Parameter :: ISave = 100
  Integer, Parameter :: IRatio = 100
  Real :: Dens
   
End Module cte
