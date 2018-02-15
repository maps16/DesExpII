!========================================================================
! PROGRAMA PRINCIPAL DEL LA SIMULACION, LLAMA SUBRUTINAS PARA REALIZAR 
! LA SIMULACION
!Autor: Martin Alejandro Paredes Sosa
!========================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, IStep                                    !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY, NewX, NewY                            !VALORES TEMP DE POSC
  Real :: RanX, RanY, Dummy                                 !VALORES ALEATORIOS
  Logical :: Ctrl                                           !CONTROL LOGICO

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
  dRMax = 0.1
  
  !CORRECCION DE LARGO ALCANCE
  VLRC = 0 !NO SE OCUPA LA CORRECCION POR SER DE CORTO ALCANCE
  
  !CALCULAR LA ENERGIA DE LA CONFIGURACION
  Call EnergyConfig(V)
  VI = V + VLRC
  Write(*,*) "ENERGIA DE LA CONFIGURACION INICIAL:",  VI

  !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
  Open(2, File="ConFin.dat")
  Open(3, File="Terma.dat" )
  !MOVIMIENTO DE PARTICULAS ALEATORIA

    
  Configuracion: Do iStep = 1, NStep
     
     Particula: Do i = 1, N
        
        OldX = X(i)
        OldY = Y(i)
        !CALCULAR LA ENERGIA DE LA i-PARTICULA
        Call EnergyPart(OldX, OldY, i, VOld)

        !GENERAR VALORES ALEATORIOS PARA MOV TENTATIVOS
        Call Random_Number(RanX)
        Call Random_Number(RanY)

        !MOVIMIENTO TENTATIVO
        NewX = OldX + (2.0*RanX - 1.0)*dRMax
        NewY = OldY + (2.0*RanY - 1.0)*dRMax

        !CONDICIONES PERIODICAS (MANTENER MISMA N EN TODA CONFIGURACION)
        NewX = NewX - BoxL*Anint(NewX/BoxL)
        NewY = Newy - BoxL*Anint(NewY/BoxL)

        !CALCULAR LA ENERGIA DE LA PARTICULA EN LA NUEVA POSICION
        Call EnergyPart(NewX, NewY, i, VNew)

        !MONTECARLO (CRITERIO DE ACEPTACION O RECHAZO DE MOV)
        DV = VNew - VOld
        Call Random_Number(Dummy) !PARA CRITERIO ENTRE 0.0 Y 75.0
        
        MonteCarlo1: If(DV .LT. 75.0 ) Then
           
           MonteCarlo2: If(DV .LE. 0.0 ) Then
              V = V + DV
              X(i) = NewX
              Y(i) = NewY

           ElseIf( EXP(-DV) .GT. Dummy ) Then
              V = V + DV
              X(i) = NewX
              Y(i) = NewY
              
           End If MonteCarlo2
        End If MONTECARLO1

        !ENERGIA POR PARTICULA
        VN = (V+VLRC)/Real(N)
        
     End Do Particula

     Write(3,*) IStep, VN

     !MONITORES DE ENERGIA PANTALLA
     Ctrl = Mod(IStep,IPrint) .EQ. 0
     MonitoreoEne: If(Ctrl) Then
        
        Write(*,*)  ISTEP, VN
        
     End If MonitoreoEne
     
  End Do Configuracion






  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N
     Write(2,*) X(i), Y(i)
  End Do ConfigFin

  
  Deallocate( X, Y )
End Program Main
