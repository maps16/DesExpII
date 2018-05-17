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
  Real :: OldX, OldY, OldZ, NewX, NewY, NewZ                !VALORES TEMP DE POSC
  Real :: RanX, RanY, RanZ, Dummy                                 !VALORES ALEATORIOS
  Real :: MAcep, Ratio                                      !VARIABLES DE CONTROL DE DRMAX
  Logical :: Ctrl, Ctrl1, Ctrl1A, Ctrl2                     !CONTROL LOGICO
  Integer :: istat1, istat2
  Character (len=80) :: err_msg1, err_msg2
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Dens = 0.1
  CONCE: Do While(Dens .LT. 1.0)

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
     Allocate( X(N), Y(N), STAT= istat1 , ERRMSG=err_msg1  )


     !GENERAR LA CONFIGURACION INICIAL
     Cnfg: If (Dens .LE. 0.65) Then
        Call ConfigIni
        Write(*,*) "CONFIGURACION ALEATORIA INICIAL LISTA"
     Else
        Call ConfigIniReg
        Write(*,*) "CONFIGURACION REGULAR INICIAL LISTA"
     End If Cnfg
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

     Write(Cons,256) Dens
     Filename = "Terma"//trim(Cons)//".dat"
     
     Open(3, File=Trim(Filename) )
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
           NewY = NewY - BoxL*Anint(NewY/BoxL)

           !CALCULAR LA ENERGIA DE LA PARTICULA EN LA NUEVA POSICION
           Call EnergyPart(NewX, NewY, i, VNew)

           !MONTECARLO (CRITERIO DE ACEPTACION O RECHAZO DE MOV)
           DV = VNew - VOld
           Call Random_Number(Dummy) !PARA CRITERIO ENTRE 0.0 Y 75.0

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

     Deallocate( CX, CY )

     Close(3)
     Dens = Dens + 0.1
  End Do CONCE
  
  WRITE(*,*) "DONE"

256 Format (I2.2)

End Program Main
