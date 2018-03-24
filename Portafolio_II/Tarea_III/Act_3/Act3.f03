!======================================================================================
! PROGRAMA QUE COLOCA UN NUMERO N DE PARTICULAS Y LAS MUEVE CONCIDERANDO CONDICIONES
! PERIODICAS. SE HACE SEGUIMIENTO DEL MOVIMIENTO DE PARTICULAS (2 PARTICULAS).
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!======================================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, IStep                                    !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY, NewX, NewY                            !VALORES TEMP DE POSC
  Real :: RanX, RanY, Dummy, Dens2                          !VALORES ALEATORIOS
  Logical :: Ctrl                                           !CONTROL LOGICO
  Integer :: h, p                                           !VALOR ALEATORIO TRAZADORAS
  Real, Dimension(2) :: Temp

  
  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "CONCENTRACION REDUCIDA"
  Read(*,*) Dens
  Write(*,*) "NUMERO DE CICLOS"
  Read(*,*) NStep
 ! Write(*,*) "MONITOREO EN PANTALLA (CADA CAUNTOS CICLOS)"
 ! Read(*,*) IPrint
 ! Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION"
 ! Read(*,*) ISave
 ! Write(*,*) "FRECUENCIA DE CORRECCION EN DESPLAZAMIENTO"
 ! Read(*,*) IRatio

  Allocate( X(N), Y(N)  )

  !GENERAR LA CONFIGURACION INICIAL
  Call ConfigIni
  Write(*,*) "CONFIGURACION INICIAL LISTA"
  RCut = BoxL / 2.0
  dRMax = 0.5
  
  !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
  Open(2, File="ConFin.dat")
  
  !SELECCION DE PARTICULAS ALEATORIAMENTE
  Call Random_Number(Temp)
  Temp = Temp * N
  h = Anint( Temp(1) )
  p = Anint( Temp(2) )
  Write(*,*) "SE SELECIONARON LAS PARTICULAS: ", h, p
  Open(11, File= "Part1.dat")
  Open(12, File= "Part2.dat")
  
  !MOVIMIENTO DE PARTICULAS ALEATORIA
  Configuracion: Do iStep = 1, NStep
     
     Particula: Do i = 1, N
        
        OldX = X(i)
        OldY = Y(i)
        !CALCULAR LA ENERGIA DE LA i-PARTICULA
        !Call EnergyPart(OldX, OldY, i, VOld)

        !GENERAR VALORES ALEATORIOS PARA MOV TENTATIVOS
        Call Random_Number(RanX)
        Call Random_Number(RanY)

        !MOVIMIENTO DE PARTICULA
        NewX = OldX + (2.0*RanX - 1.0)*dRMax
        NewY = OldY + (2.0*RanY - 1.0)*dRMax

        !CONDICIONES PERIODICAS (MANTENER MISMA N EN TODA CONFIGURACION)
        NewX = NewX - BoxL*Anint(NewX/BoxL)
        NewY = Newy - BoxL*Anint(NewY/BoxL)

        X(i) = NewX
        Y(i) = NewY
        
     End Do Particula
     
     !SEGUIMIENTO DE PARTICULAS (TRAZADORAS)
     Write(11,*) X(h), Y(h)
     Write(12,*) X(p), Y(p)
     
  End Do Configuracion
  Close(11)
  Close(12)
  
  !CHECA LA DENSIDAD DE LA CELDA ORIGINAL (CONCENTRACION FINAL)
  !Call Conc(Dens2)

  
  
  
  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N
     Write(2,*) X(i), Y(i) 
  End Do ConfigFin

  
  
  Deallocate( X, Y )

End Program Main
