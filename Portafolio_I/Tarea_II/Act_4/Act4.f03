
!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL REGULAR EN CELDA TRIDIMENSIONAL SIN
! TRASLAPES
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
  Real :: Dim, xij, yij, dBoxL, dist
  Real, Allocatable, Dimension(:) :: X,Y,Z  !POSICIONES DE LAS PARTICULAS
  Integer :: i, j, k                        !CONTADOR
  Integer :: N2, N3                         !PARTICULAS 
  
  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "INGRESE EL NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "INGRESE CONCENTRACION REDUCIDA"
  Read(*,*) Dens

  
  
  !CALCULANDO DIMENSIONES DE LA CAJA
  Dim = 3.0
  N2 =anint( N**(1.0/Dim) )
  BoxL = (1.0*N/Dens )**(1.0/Dim)
  N3 = N2*N2*N2
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL
  Write(*,*) "TOTAL DE PARTICULAS EN LA CELDA COLOCADAS:", N3
  dBoxl = BoxL/N2
  
  
  Allocate( X(N2),Y(N2),Z(N2) )
  

  !BUSCANDO POSICIONES PARA LAS PARTICULAS POR EJE
  Colocar: Do i=1, N2          

     x(i) = -(BoxL-1)/2 + dBoxL*(i-1)
     y(i) = -(BoxL-1)/2 + dBoxL*(i-1)
     z(i) = -(BoxL-1)/2 + dBoxL*(i-1)
  
  End Do Colocar

  !ESCRIBIENDO EN ARCHIVO
  Open (1, File = "PosPart.dat" ) 
  EscribirX: Do i = 1, N2
     EscribirY: Do j = 1, N2
        EScribirZ: Do k = 1, N2
           Write(1,*) x(i), y(j), z(k)
        End Do EScribirZ
     End Do EscribirY
  End Do EscribirX



  Deallocate(X,Y)
  Close(1)
  
End Program ConfigIni
