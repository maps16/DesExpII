!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL ALEATORIA EN CELDA BIDIMENSIONAL
! SIN TRASLAPES
! Autor: Martin Paredes Sosa
!================================================================================

Subroutine ConfigIni(N3)
  Use cte
  Implicit None
  Real :: xRan, yRan, zRan,  xij, yij, zij,  dist
  Real :: dBoxl
  Integer :: i, j, k ,l                             !CONTADOR
  Integer :: N2, N3
  Real, Dimension(:),Allocatable :: nX, nY, nZ      !GEN
  
  !CALCULANDO DIMENSIONES DE LA CAJA
  N2 =aint( N**(1.0/Dim) ) 
  BoxL = (1.0*N2/Dens )**(1.0/Dim)
  N3 = N2*N2*N2
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL
  Write(*,*) "TOTAL DE PARTICULAS EN LA CELDA COLOCADAS:", N3
  dBoxl = BoxL/N2


  Allocate( nX(N2), nY(N2), nZ(N2), X(N3), Y(N3), Z(N3) )
 
  !GENERANDO COORDENADAS PARA POSICIONES DE LAS PARTICULAS
  GEN: Do i=1, N2          

     nx(i) = -(BoxL)/2 + dBoxL*(i-1)
     ny(i) = -(BoxL)/2 + dBoxL*(i-1)
     nz(i) = -(BoxL)/2 + dBoxL*(i-1)
     
  End Do GEN

  !ESCRIBIENDO EN ARCHIVO
  Open (1, File = "ConIni.dat" ) 
  l = 0 
  EscribirX: Do i = 1, N2
     
     EscribirY: Do j = 1, N2
        
        EScribirZ: Do k = 1, N2
           l = l + 1
           X(l) = nX(i)
           Y(l) = nY(j)
           Z(l) = nY(k)


        End Do EScribirZ

     End Do EscribirY
     
  End Do EscribirX
  Write(*,*) l !DEBUG

  Do i=1, N3
     Write(1,*) X(i), Y(i), Z(i)
  End Do
   

  Deallocate(nX, nY, nZ)
  
  Close(1)


End Subroutine ConfigIni
