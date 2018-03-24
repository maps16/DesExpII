!================================================================================
! CONSTRUCCION DE UNA CONFIGURACION INICIAL ALEATORIA EN CELDA BIDIMENSIONAL
! SIN TRASLAPES
! Autor: Martin Paredes Sosa
!================================================================================

Subroutine ConfigIni
  Use cte
  Implicit None
  Real :: xRan, yRan,  xij, yij,  dist
  Integer :: i, j                              !CONTADOR
  
  !CALCULANDO DIMENSIONES DE LA CAJA
  BoxL = (1.0*N/Dens )**(1.0/Dim)
  Write(*,*) "LONGITUD DE LA CELDA:", BoxL

  Open (1, File = "ConIni.dat" ) 
  
  Colocar: Do i=1, N           !BUSCAR LA POSICION ALEATORIA PARA LAS PARTICULAS
  2  Call Random_Number(xRan)  !VALOR ALEATORIO DE POSICION X \
     Call Random_Number(yRan)  !VALOR ALEATORIO DE POSICION Y / TENTATIVO   
    
     !COLOCAR DENTRO DE LA CELDA
     
     X(i) = (xRan-0.5)*(BoxL-1)                   !\
     Y(i) = (yRan-0.5)*(BoxL-1)                   !/   [-(BoxL-1)/2 , (BoxL-1)/2]
    
     !Write(*,*) X(i), Y(i)                       !DEBUG

     Traslape: Do j=1 , i-1
        
        xij = X(i) - X(j)              !CALCULANDO LA DISTANCIA ENTRE PARTICULAS
        yij = Y(i) - Y(j)
        !Write(*,*) i,j,xij, yij, zij  !DEBUG
        dist = xij*xij + yij*yij

        DectTras: If(dist .LE. sigma ) Then       !CHECAR SI EXISTE TRASLAPE

           GO TO 2                                !VOLVER A CALCULAR UNA POSICION IR A LOS CALL_RANDOM_NUMBER

        End If DectTras

     End Do Traslape
     
     Write(1,*) X(i), Y(i)                        !GUARDANDO EN ARCHIVO LA CONFIGURACION INICIAL

  End Do Colocar

  Close(1)


End Subroutine ConfigIni
