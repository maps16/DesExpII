Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0                                   
  Real, Parameter :: PI=4.0*atan(1.0)
  Real, Parameter :: Lambda =  1.25                                !TAMANO DEL POZO
  Real, Parameter :: TP = 1.0                                      !TEMPERATURA REDUCIDA
  Integer, Parameter :: CEq = 100000                                !CONFIG DE EQUILIBRIO
  Real ::  BoxL, RCut, dRMax
  Integer :: NN
  Real, Parameter :: Dim = 1.0/3.0                                 !DIMENSIONES (3D)
  Real, Allocatable, Dimension(:) :: X, Y, Z                       !POSICIONES DE LAS PARTICULAS
  Real, Allocatable, Dimension(:,:) :: CX, CY, CZ                  !MATRICES DE CONFIGURACION

  !VARIABLES DE SIMULACION
  Integer , Parameter :: N = 216
  Integer, Parameter :: NStep = 300000                              !NSTEP - CEq CONFIG DE EQUILIBRIO
  Integer, Parameter :: IPrint = 1000
  Integer, Parameter :: ISave = 100
  Integer, Parameter :: IRatio = 100
  Real, Parameter :: Dens = 0.9  
   
End Module cte
