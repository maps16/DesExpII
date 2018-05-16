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
  Real :: RanX, RanY, RanZ, Dummy                           !VALORES ALEATORIOS
  Real :: MAcep, Ratio                                      !VARIABLES DE CONTROL DE DRMAX
  Logical :: Ctrl, Ctrl1, Ctrl1A, Ctrl2                     !CONTROL LOGICO
  Integer :: istat1, istat2
  Character (len=80) :: err_msg1, err_msg2
  Character (len=4) :: Selc



  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "NUMERO DE PARTICULAS"
  Write(*,*) N
  Write(*,*) "CONCENTRACION REDUCIDA"
  Write(*,*) Dens
  Write(*,*) "NUMERO DE CICLOS"
  Write(*,*) NStep
  Write(*,*) "MONITOREO EN PANTALLA (CADA CAUNTOS CICLOS)"
  Write(*,*) IPrint
!  Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION"
!  Read(*,*) ISave
!  Write(*,*) "FRECUENCIA DE CORRECCION EN DESPLAZAMIENTO"
!  Read(*,*) IRatio
  Write(*,*) "============================================================================="


  !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLO DE POSICION DE PARTICULAS
  Allocate( X(N), Y(N), STAT= istat1 , ERRMSG=err_msg1  )


  !GENERAR LA CONFIGURACION INICIAL
1024 Continue

  Write(*,*) "OPCIONES SON:"
  Write(*,*) "a) Reg - b) Tras -  c) Al"
  Read (*,*) Selc
  Write(*,*) "============================================================================="
  
  SELECT CASE (Selc)
  CASE ("c")

     Call ConfigIni
     Write(*,*) "CONFIGURACION INICIAL ALEATORIA LISTA"

  CASE ("a")

     Call ConfigIniReg
     Write(*,*) "CONFIGURACION INICIAL REGULAR LISTA"

  CASE ("b")
     
     Call ConfigIniTras
     Write(*,*) "CONFIGURACION INICIAL REGULAR LISTA"
     
  CASE DEFAULT
     
     Write(*,*) "NO DEFINIDO VUELVA A INTENTARLO"
     go to 1024
     
  END SELECT



  !CALCULO/PARAMETROS PARA INICIALIZAR
  RCut = BoxL / 2.0
  dRMax = 0.1
  MAcep = 0.0
  k2 = 0
  NN = ( NStep- CEq ) / ISave



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

     !MONITOREO EN PANTALLA
     Ctrl = Mod(IStep,IPrint*30) == 0                              !CADA QUE TANTO IMPRIMIR EN PANTALLA ENERGIA, DRMAX
     Monitoreo: If(Ctrl .OR. Istep == 1) Then

        Write(*,*)  "|CONFIGURACION| ENEREGIA|"

     End If Monitoreo


     Ctrl = Mod(IStep,IPrint) == 0                              !CADA QUE TANTO IMPRIMIR EN PANTALLA ENERGIA, DRMAX
     MonitoreoEne: If(Ctrl) Then

        Write(*,*)  ISTEP, VN

     End If MonitoreoEne

  End Do Configuracion

  Write(*,*) "DONE ALL CONFIGURATIONS"

  !========================================================================
  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N

     Write(2,*) X(i), Y(i)

  End Do ConfigFin
  Close (2)
  WRITE(*,*) "DONE SAVING CONFIG FINAL"

  Deallocate( X, Y )
  WRITE(*,*) "CLEAR MEMORY" !DEBUG



  Close(3)

  WRITE(*,*) "DONE"

End Program Main
