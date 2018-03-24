!======================================================================================
! PROGRAMA QUE COLOCA UN NUMERO N DE PARTICULAS Y LAS MUEVE CONCIDERANDO CONDICIONES
! PERIODICAS. SE HACE SEGUIMIENTO DEL MOVIMIENTO DE PARTICULAS (2 PARTICULAS).
! SE REALIZO PARA 3 DIMENSIONES (X,Y,Z). ARREGLO UNIFORME.
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!======================================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, IStep, N3                                !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY,OldZ, NewX, NewY, NewZ                 !VALORES TEMP DE POSC
  Real :: RanX, RanY,RanZ, Dummy                            !VALORES ALEATORIOS
  Logical :: Ctrl                                           !CONTROL LOGICO
  Integer :: h, p, g                                        !VALOR ALEATORIO TRAZADORAS
  Real :: Dens2                                             !DENSIDAD  FINAL
  Real, Dimension(2) :: Temp                                !TEMPORAL PARA PARTICULAS ALEATORIAS

  
  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "CONCENTRACION REDUCIDA"
  Read(*,*) Dens
  Write(*,*) "NUMERO DE CICLOS"
  Read(*,*) NStep
 

  !Allocate( X(N), Y(N), Z(N)  )

  !GENERAR LA CONFIGURACION INICIAL
  Call ConfigIni(N3)
  Write(*,*) "CONFIGURACION INICIAL LISTA"
  RCut = BoxL / 2.0
  dRMax = 1.0
 

  
  !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
  Open(2, File="ConFin.dat")
  
  !SELECCION DE PARTICULAS ALEATORIAMENTE
  Call Random_Number(Temp)
  Temp = Temp * N3
  h = Anint( Temp(1) )
  p = Anint( Temp(2) )
  !g = Anint( Temp(3) )
  Write(*,*) "SE SELECIONARON LAS PARTICULAS: ", h, p!, g
  Open(11, File= "Part1.dat")
  Open(12, File= "Part2.dat")
  !Open(13, File= "Part3.dat")

  Write(*,*) "INICIO MOVIMIENTO PARTICULAS"
  !MOVIMIENTO DE PARTICULAS ALEATORIA
  Configuracion: Do iStep = 1, NStep
     
     Particula: Do i = 1, N3
        
        OldX = X(i)
        OldY = Y(i)
        OldZ = Z(i)

        !GENERAR VALORES ALEATORIOS PARA MOV TENTATIVOS
        Call Random_Number(RanX)
        Call Random_Number(RanY)
        Call Random_Number(RanZ)

        !MOVIMIENTO DE PARTICULA
        NewX = OldX + (2.0*RanX - 1.0)*dRMax
        NewY = OldY + (2.0*RanY - 1.0)*dRMax
        NewZ = OldZ + (2.0*RanZ - 1.0)*dRMax
        
        !CONDICIONES PERIODICAS (MANTENER MISMA N EN TODA CONFIGURACION)
        NewX = NewX - BoxL*Anint(NewX/BoxL)
        NewY = Newy - BoxL*Anint(NewY/BoxL)
        Newz = NewZ - Boxl*Anint(NewZ/BoxL)

        X(i) = NewX
        Y(i) = NewY
        Z(i) = NewZ
     End Do Particula
     
     !SEGUIMIENTO DE PARTICULAS (TRAZADORAS)
     Write(11,*) X(h), Y(h), Z(h)
     Write(12,*) X(p), Y(p), Z(p)
     !Write(13,*) X(g), Y(g), Z(g)
     
  End Do Configuracion
  Write(*,*) "CONFIGURACIONES LISTAS"
  Close(11)
  Close(12)
  !Close(13)
  
  !CHECA LA DENSIDAD DE LA CELDA ORIGINAL (CONCENTRACION FINAL)
  !Call Conc(Dens2)
  
  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N3
     Write(2,*) X(i), Y(i), Z(i) 
  End Do ConfigFin

  
  
  Deallocate( X, Y, Z )

End Program Main
