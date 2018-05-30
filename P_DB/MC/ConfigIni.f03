!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL ALEATORIA EN CELDA BIDIMENSIONAL
! 
! Autor: Martin Paredes Sosa
!================================================================================

Subroutine ConfigIni
  Use cte
  Implicit None
  Real :: xRan, yRan, zRan         !POSC
  Integer :: i, j                  !CONTADOR
  
  !CALCULANDO DIMENSIONES DE LA CAJA
  BoxL = (1.0*N/Dens )**Dim
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL

  Open (1, File = "ConIni.dat" ) 
  
  Colocar: Do i=1, N           !BUSCAR LA POSICION ALEATORIA PARA LAS PARTICULAS
  2  Call Random_Number(xRan)  !VALOR ALEATORIO DE POSICION X \
     Call Random_Number(yRan)  !VALOR ALEATORIO DE POSICION Y | TENTATIVO
     Call Random_Number(zRan)  !VALOR ALEATORIO DE POSICION Z /
    
     !COLOCAR DENTRO DE LA CELDA
     
     X(i) = (xRan-0.5)*(BoxL-1)                   !\
     Y(i) = (yRan-0.5)*(BoxL-1)                   !|   [-(BoxL-1)/2 , (BoxL-1)/2]
     Z(i) = (zRan-0.5)*(BoxL-1)                   !/
     
          
     Write(1,*) X(i), Y(i), Z(i)           !GUARDANDO EN ARCHIVO LA POSICION

  End Do Colocar

  Close(1)


End Subroutine ConfigIni
