!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL ALEATORIA EN CELDA TRIDIMENSIONAL
! SIN TRASLAPES
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
  Real :: Dim, xRan, yRan, zRan, xij, yij, zij, dist
  Real, Allocatable, Dimension(:) :: X,Y,Z  !POSICIONES DE LAS PARTICULAS
  Integer :: i, j                              !CONTADOR
  
  
  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "INGRESE EL NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "INGRESE CONCENTRACION REDUCIDA"
  Read(*,*) Dens

  !CALCULANDO DIMENSIONES DE LA CAJA
  Dim = 1.0/3.0
  BoxL = (1.0*N/Dens )**Dim
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL

  Allocate( X(n),Y(n),z(n) )
  Open (1, File = "PosPart.dat"git ) 
  
  Colocar: Do i=1, N           !BUSCAR LA POSICION ALEATORIA PARA LAS PARTICULAS
  2  Call Random_Number(xRan)  !VALOR ALEATORIO DE POSICION X \
     Call Random_Number(yRan)  !VALOR ALEATORIO DE POSICION Y | TENTATIVO   
     Call Random_Number(zRan)  !VALOR ALEATORIO DE POSICION Z /
     !Write(*,*) xRan, yRan, zRan   !DEBUG

     !COLOCAR DENTRO DE LA CELDA
     X(i) = (xRan-0.5)*BoxL                    !\
     Y(i) = (yRan-0.5)*BoxL                    !|   [-BoxL/2 , BoxlL/2]
     Z(i) = (zRan-0.5)*BoxL                    !/
     !Write(*,*) X(i), Y(i), Z(i)               !DEBUG

     Traslape: Do j=1 , i-1
        
        xij = X(i) - X(j)              !CALCULANDO LA DISTANCIA ENTRE PARTICULAS
        yij = Y(i) - Y(j)
        zij = Z(i) - Z(j)
        !Write(*,*) i,j,xij, yij, zij !DEBUG
        dist = xij*xij + yij*yij + zij*zij
        DectTras: If(dist .LE. sigma ) Then
          !Write(*,*) "TRASLAPE", i, j  !DEBUG 
          GO TO 2
        End If DectTras
        
     End Do Traslape
     
     Write(1,*) X(i), Y(i), Z(i) !GUARDANDO EN ARCHIVO LA POSICION

  End Do Colocar

  Deallocate(X,Y,Z)
  Close(1)


End Program ConfigIni
