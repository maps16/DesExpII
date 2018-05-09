!========================================================================
! PROGRAMA PRINCIPAL DEL LA SIMULACION, LLAMA SUBRUTINAS PARA REALIZAR 
! LA SIMULACION
! Autor: Martin Alejandro Paredes Sosa
!========================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, l, IStep , k, k2                            !CONTADORES
  Real :: VLRC, VI, V, VOld, VNew, DV, VN                   !ENERGIAS
  Real :: OldX, OldY, OldZ, NewX, NewY, NewZ                !VALORES TEMP DE POSC
  Real :: RanX, RanY, RanZ, Dummy                                 !VALORES ALEATORIOS
  Real :: MAcep, Ratio                                      !VARIABLES DE CONTROL DE DRMAX
  Logical :: Ctrl, Ctrl1, Ctrl1A, Ctrl2                     !CONTROL LOGICO
  Integer :: istat1, istat2
  Character (len=80) :: err_msg1, err_msg2

  Open(27, File="Presion.dat")
  
  DENSi: Do l = 1, 3
     Dens = Real(l) * 0.1
     !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
     Write(*,*) "NUMERO DE PARTICULAS"
     Write(*,*) N
     Write(*,*) "CONCENTRACION REDUCIDA"
     Write(*,*) Dens
     Write(*,*) "NUMERO DE CICLOS"
     Write(*,*) NStep
     Write(*,*) "MONITOREO EN PANTALLA (CADA CAUNTOS CICLOS)"
     Write(*,*) IPrint
     Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION"
     Write(*,*) ISave
     Write(*,*) "FRECUENCIA DE CORRECCION EN DESPLAZAMIENTO"
     Write(*,*) IRatio
     Write(*,*) "============================================================================="


     !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLO DE POSICION DE PARTICULAS
     Allocate( X(N), Y(N), Z(N))!, STAT= istat1 , ERRMSG=err_msg1  )



     !GENERAR LA CONFIGURACION INICIAL
     ConIni: If( Dens .LT. 0.7) Then
        
        Call ConfigIni                           !CONFIG ALEATORIA

     Else

        Call ConfigIniReg                        !CONFIG REGULAR
        
     End If ConIni
     Write(*,*) "CONFIGURACION INICIAL LISTA"

     !CALCULO/PARAMETROS PARA INICIALIZAR
     RCut = BoxL / 2.0
     dRMax = 0.1
     MAcep = 0.0
     k2 = 0
     NN = ( NStep- CEq ) / ISave

     !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLOS DE CONFIGURACION
     Allocate( CX(N,NN), CY(N,NN), CZ(N,NN))!, STAT= istat2 , ERRMSG=err_msg2 )


     !CORRECCION DE LARGO ALCANCE
     VLRC = 0 !NO SE OCUPA LA CORRECCION POR SER DE CORTO ALCANCE

     !CALCULAR LA ENERGIA DE LA CONFIGURACION
     Call EnergyConfig(V)
     VI = V + VLRC
     Write(*,*) "ENERGIA DE LA CONFIGURACION INICIAL:",  VI

     Write(*,*) "============================================================================="
     Write(*,*) "|CONFIG||ENERGIA PARTICULA||RATIO||DR|"

     !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
     Open(2, File="ConFin.dat")
     Open(3, File="Terma.dat" )
     !MOVIMIENTO DE PARTICULAS ALEATORIA

     Configuracion: Do iStep = 1, NStep

        Particula: Do i = 1, N

           OldX = X(i)
           OldY = Y(i)
           OldZ = Z(i)

           !CALCULAR LA ENERGIA DE LA i-PARTICULA
           Call EnergyPart(OldX, OldY, OldZ, i, VOld)

           !GENERAR VALORES ALEATORIOS PARA MOV TENTATIVOS
           Call Random_Number(RanX)
           Call Random_Number(RanY)
           Call Random_Number(RanZ)

           !MOVIMIENTO TENTATIVO
           NewX = OldX + (2.0*RanX - 1.0)*dRMax
           NewY = OldY + (2.0*RanY - 1.0)*dRMax
           NewZ = OldZ + (2.0*RanZ - 1.0)*dRMax

           !CONDICIONES PERIODICAS (MANTENER MISMA N EN TODA CONFIGURACION)
           NewX = NewX - BoxL*Anint(NewX/BoxL)
           NewY = NewY - BoxL*Anint(NewY/BoxL)
           NewZ = NewZ - BoxL*Anint(NewZ/BoxL)

           !CALCULAR LA ENERGIA DE LA PARTICULA EN LA NUEVA POSICION
           Call EnergyPart(NewX, NewY, NewZ, i, VNew)

           !MONTECARLO (CRITERIO DE ACEPTACION O RECHAZO DE MOV)
           DV = VNew - VOld
           Call Random_Number(Dummy) !PARA CRITERIO ENTRE 0.0 Y 75.0

           !MONTECARLO (ACEPTANDO MOVIMIENTOS POR CRITERIOS)
           MONTECARLO1: If(DV .LT. 75.0 ) Then

              MONTECARLO2: If(DV .LE. 0.0 ) Then
                 V = V + DV
                 X(i) = NewX
                 Y(i) = NewY
                 Z(i) = NewZ
                 MAcep = MAcep + 1.0 !MOVIMIENTO ACEPTADOS POR MONTECARLO

              ElseIf( EXP(-DV) .GT. Dummy ) Then
                 V = V + DV
                 X(i) = NewX
                 Y(i) = NewY
                 Z(i) = NewZ
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
           Ratios:If (Dens .LT. 0.3) Then
              Ctrl1A = Ratio .GT. 0.95                               !CRITERIO DE ACEPTACION DE MOVIMIENTOS
           Else
              Ctrl1A = Ratio .GT. 0.5
           End If Ratios

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
              CZ(k,K2) = Z(k)

           End Do SAV1

        End If SAV


     End Do Configuracion

     Write(*,*) "DONE ALL CONFIGURATIONS"

     !GUARDAR CONFIG FINAL
     ConfigFin: Do i=1, N

        Write(2,*) X(i), Y(i), Z(i)

     End Do ConfigFin
     Close (2)
     WRITE(*,*) "DONE SAVING CONFIG FINAL"

     Deallocate( X, Y, Z )
     WRITE(*,*) "CLEAR MEMORY" !DEBUG

     Call GdrCalc(l)
     WRITE(*,*) "GDR DONE CALC" !DEBUG

     Deallocate( CX, CY, CZ )!, STAT=istat1, ERRMSG = err_msg1 )
     !Write(*,*) istat1, err_msg1 !DEBUG
     Close(3)

     WRITE(*,*) "DONE"
     WRITE(*,*) "==================================================================="
  End Do DENSI
  
End Program Main
