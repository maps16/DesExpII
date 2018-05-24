!========================================================================
! PROGRAMA PRINCIPAL DEL LA SIMULACION, LLAMA SUBRUTINAS PARA REALIZAR 
! LA SIMULACION
! Autor: Martin Alejandro Paredes Sosa
!========================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, IStep , k, k2                            !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY, NewX, NewY                            !VALORES TEMP DE POSC
  Real :: RanX, RanY, Dummy                                 !VALORES ALEATORIOS
  Real :: MAcep, Ratio                                      !VARIABLES DE CONTROL DE DRMAX
  Logical :: Ctrl, Ctrl1, Ctrl1A, Ctrl2                     !CONTROL LOGICO
  Integer :: istat1, istat2
  Character (len=80) :: err_msg1, err_msg2


  

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
  Read(*,*) ISave
  Write(*,*) "FRECUENCIA DE CORRECCION EN DESPLAZAMIENTO"
  Read(*,*) IRatio
  Write(*,*) "============================================================================="
  
  Concentracion:  Do While( Dens <= 0.66 )
  !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLO DE POSICION DE PARTICULAS
  Allocate( X(N), Y(N), STAT= istat1 , ERRMSG=err_msg1  )
  Open(120, File="EcEstado.dat")

  !Concentracion:  Do While( Dens <= 0.6 )
  
     !GENERAR LA CONFIGURACION INICIAL
     Call ConfigIni
     Write(*,*) "CONFIGURACION INICIAL LISTA"
     
     !CALCULO/PARAMETROS PARA INICIALIZAR
     RCut = BoxL / 2.0
     dRMax = 0.1
     MAcep = 0.0
     k2 = 0
     NN = ( NStep- CEq ) / ISave
     
     !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLOS DE CONFIGURACION
     Allocate( CX(N,NN), CY(N,NN), STAT= istat2 , ERRMSG=err_msg2 )
     
     
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
           !Write(*,*) IStep, VOld
           !go to 111
           
           !GENERAR VALORES ALEATORIOS PARA MOV TENTATIVOS
           Call Random_Number(RanX)
           Call Random_Number(RanY)
           !Write(*,*) Ranx, Rany
           
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
           !Write(*,*) Dummy
           !MONTECARLO (ACEPTANDO MOVIMIENTOS POR CRITERIOS)
           MONTECARLO1: If(DV .LT. 75.0 ) Then
              
              MONTECARLO2: If(DV .LE. 0.0 ) Then
                 V = V + DV
                 X(i) = NewX
                 Y(i) = NewY
                 MAcep = MAcep + 1.0 !MOVIMIENTO ACEPTADOS POR MONTECARLO
                 
              ElseIf( EXP(-DV) .GT. Dummy ) Then
                 V = V + DV
                 X(i) = NewX
                 Y(i) = NewY
                 MAcep = MAcep + 1.0 !MOVIMIENTOS ACEPTADOS POR MONTECARLO 
                 
              End If MONTECARLO2
           End If MONTECARLO1
           
           !ENERGIA POR PARTICULA
           VN = (V+VLRC)/Real(N)
           
        End Do Particula
        
        !GUARDANDO LA TERMALIZACION DE CADA CONFIGURACION DEL SISTEMA 
        Write(3,*) IStep, VN
        
        
        
        !AJUSTE DE DESPLAZAMIENTO DRMAX
        Ctrl1 = Mod(IStep, IRatio) == 0
        NdR : If (Ctrl1) Then
           
           Ratio =  MAcep / Real( N * IRatio  )                      !RAZON DE ACEPTADOS 
           Ctrl1A = Ratio .GT. 0.5                                  !CRITERIO DE ACEPTACION DE MOVIMIENTOS
           
           Criterio : If ( Ctrl1A ) Then
              dRMax = dRMax * 1.05                                   !CRECER DESPLAZAMIENTO 
           Else
              dRMax = dRMax * 0.95                                   !DISMINUIR DESPLAZAMIENTO
           End If Criterio
           
           MAcep = 0.0                                               !REINICIAR CONTADOR DE MOV ACEPTADOS
           
        End If NdR
        
        !MONITOREO EN PANTALLA
        Ctrl = Mod(IStep,IPrint) == 0                                !CADA QUE TANTO IMPRIMIR EN PANTALLA ENERGIA, DRMAX
        MonitoreoEne: If(Ctrl) Then
           
           Write(*,*)  ISTEP, VN, Ratio , dRMax
           
        End If MonitoreoEne
        
        !GUARDANDO CONFIGURACION (EN EQUILIBRIO)
        Ctrl2 = ( Mod(IStep,ISave) == 0 ) .AND. ( IStep .GT. CEq )
        SAV: If (Ctrl2) Then
           k2 = k2 + 1
           SAV1:Do k = 1 , N
              CX(k,k2) = X(k)
              CY(k,k2) = Y(k)
           End Do SAV1
        End If SAV
        
        !Write(*,*) "SAVE CONFIG: ", IStep                            !DEBUG
        
     End Do Configuracion
     
     Write(*,*) "DONE ALL CONFIGURATIONS"
     
     !GUARDAR CONFIG FINAL
     ConfigFin: Do i=1, N
        
        Write(2,*) X(i), Y(i)
        
     End Do ConfigFin
     Close (2)
     WRITE(*,*) "DONE SAVING CONFIG FINAL"
     
     Deallocate( X, Y )
     WRITE(*,*) "CLEAR MEMORY" !DEBUG
     
     Call GdrCalc
     WRITE(*,*) "GDR DONE CALC" !DEBUG
     
     Deallocate( CX, CY )!, STAT=istat1, ERRMSG = err_msg1 )
     !Write(*,*) istat1, err_msg1 !DEBUG
     Close(3)

     Dens = Dens + 0.02
     Write(*,*) "============================================================================="
  End Do Concentracion
  Close(120)  
  
  
  WRITE(*,*) "DONE"

  
End Program Main
