!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL REGULAR EN CELDA BIDIMENSIONAL
! SIN TRASLAPES
! Autor: Martin Paredes Sosa
!================================================================================

Subroutine ConfigIniReg
  Use cte
  Implicit None
  Real :: xRan, yRan, xij, yij,  dist
  Real :: dBoxl
  Integer :: i, j, k ,l                             !CONTADOR
  Integer :: N2, N3
  Real, Dimension(:),Allocatable :: nX, nY          !GEN

  
  !CALCULANDO DIMENSIONES DE LA CAJA
  N2 =anint( N**(Dim) )
  
  !BoxL = (1.0*N/Dens )**(Dim)

  N3 = N2**(1.0/Dim)
  N = N3
  BoxL = (1.0*N/Dens )**(Dim)
  
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL
  Write(*,*) "TOTAL DE PARTICULAS COLOCADAS EN LA CELDA:", N3
  dBoxl = BoxL/N2


  Allocate( nX(N2), nY(N2) )
 
  !GENERANDO COORDENADAS PARA POSICIONES DE LAS PARTICULAS
  GEN: Do i=1, N2          

     nx(i) = (-BoxL)/2.0 + dBoxL/2.0 + dBoxL*(i-1)
     ny(i) = (-BoxL)/2.0 + dBoxL/2.0 + dBoxL*(i-1)
          
  End Do GEN

  !ESCRIBIENDO EN ARCHIVO
  Open (1, File = "ConIni.dat" ) 
  l = 0 
  EscribirX: Do i = 1, N2
     
     EscribirY: Do j = 1, N2

        l = l + 1
        X(l) = nX(i)
        Y(l) = nY(j)

     End Do EscribirY
     
  End Do EscribirX
  
  !Write(*,*) l !DEBUG

  Do i=1, N3
     Write(1,*) X(i), Y(i)
  End Do
   

  Deallocate(nX, nY)
  
  Close(1)


End Subroutine ConfigIniReg
