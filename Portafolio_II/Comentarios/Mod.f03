Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real, Parameter :: PI=4*atan(1.0)
  Integer, Parameter :: CEq = 1000         !CONFIGURACION DE EQUILIBRIO (SEGUN USER)
  Real ::  BoxL, RCut, dRMax!,  Dens
  Integer :: NN                            !, N, NStep, ISave, IPrint, IRatio,
  Real, Parameter :: Dim = 1.0/2.0         !Dimensiones (2D o 3D)
  Real, Allocatable, Dimension(:) :: X, Y       !POSICIONES
  Real, Allocatable, Dimension(:,:) :: CX, CY   !MATRICES DE LAS CONFIGURACIONES

  ! PARA CORRIDAS SIN INTERACCION DE USER
  Integer, Parameter :: N = 100           !NUMERO DE PARTICULAS
  Integer, Parameter :: NStep = 10000     !NUMERO DE CONFIGURACIONES (PASOS)
  Integer, Parameter :: Iprint = 1000     !IMPRESION EN PANTALLA
  Real, Parameter :: Dens=0.999           !VARIAR SEGUN CORRIDA(CONCENTRACION)
  Integer, Parameter :: ISave = 10        !CUANDO GUARDAR UNA CONFIGURACION 
  Integer, Parameter :: IRatio = 10       !CUANDO CAMBIAR EL TAMANO DE PASO
End Module cte
