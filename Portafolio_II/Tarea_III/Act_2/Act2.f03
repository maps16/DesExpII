!======================================================================================
! PROGRAMA QUE COLOCA UN NUMERO N DE PARTICULAS Y LAS MUEVE SIN CONCIDERAR CONDICIONES
! PERIODICAS 
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!======================================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, k, IStep                                 !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY, NewX, NewY                            !VALORES TEMP DE POSC
  Real :: RanX, RanY, Dummy, Dens2                          !VALORES ALEATORIOS
  Logical :: Ctrl, Ctrl1, Ctrl2                             !CONTROL LOGICO

  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "CONCENTRACION REDUCIDA"
  Read(*,*) Dens
  Write(*,*) "NUMERO DE CICLOS"
  Read(*,*) NStep
  Write(*,*) "MONITOREO EN PANTALLA (CADA CAUNTOS CICLOS)"
  Read(*,*) IPrint
  Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION"
 ! Read(*,*) ISave
  Write(*,*) "FRECUENCIA DE CORRECCION EN DESPLAZAMIENTO"
 ! Read(*,*) IRatio

  Allocate( X(N), Y(N)  )

  !GENERAR LA CONFIGURACION INICIAL
  Call ConfigIni
  Write(*,*) "CONFIGURACION INICIAL LISTA"
  RCut = BoxL / 2.0
  dRMax = 0.5
  
  !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
  Open(2, File="ConFin.dat")
  
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

  End Do Configuracion

  !CHECAR CONCENTRACION FINAL EN LA CELDA ORIGINAL
  k=0                                                       !CONTADOR DE PARTICULAS DENTRO DE LA CELDA ORIGINAL
  Partic: Do i=1 , N
     Ctrl1 = X(i) .LT. Boxl/2.0 .AND. X(i) .GT. -Boxl/2.0
     Ctrl2 = Y(i) .LT. Boxl/2.0 .AND. Y(i) .GT. -Boxl/2.0
     DENS_VERIFY: If (Ctrl1 .AND. Ctrl2) Then
        k = k + 1
     End If DENS_VERIFY
  End Do Partic
  Dens2 = k / BoxL**Dim                  !CALCULAR NUEVA DENSIDAD
  Write(*,*) "DENSIDAD FINAL DE LA CELDA ",Boxl," ES: ", Dens2
  
  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N
     Write(2,*) X(i), Y(i) 
  End Do ConfigFin

  
  Deallocate( X, Y )
End Program Main
