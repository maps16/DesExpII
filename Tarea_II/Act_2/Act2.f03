!================================================================================
! Construccion de una configuracion inicial aleatoria en celda tridimensional
! sin traslapes
! Autor: Martin Paredes Sosa
!================================================================================

Module cte
  Implicit None
  Real, Parameter :: sigma = 1.0
  Real :: Dens, BoxL
  Integer :: N
End Module cte

Program ConfigIni
  Use cte
  Implicit None
  Real :: Dim, xRan, yRan, zRan
  Real, Allocatable, Dimension(:) :: X,Y,Z
  Integer :: i                              !CONTADOR

  
  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "INGRESE EL NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "INGRESE CONCENTRACION REDUCIDA"
  Read(*,*) Dens

  !CALCULANDO DIMENSIONES DE LA CAJA
  Dim = 1.0/3.0
  BoxL = (1.0*N/Dens )**Dim
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL

  Colocar: Do i=1, N           !BUSCAR LA POSICION ALEATORIA PARA LAS PARTICULAS
     
     
     
  End Do Colocar

  
  


End Program ConfigIni
