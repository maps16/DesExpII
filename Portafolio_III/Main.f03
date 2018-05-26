!========================================================================
! PROGRAMA PRINCIPAL DEL LA SIMULACION, LLAMA SUBRUTINAS PARA REALIZAR 
! LA SIMULACION. SE UTILIZA DINAMICA BROWNIANA
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA 
!========================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j, IStep , k, k2,  ki                       !CONTADORES
  Real :: RanX, RanY, RanZ                                  !VALORES ALEATORIOS PARA POSICION
  Real :: phi                                               !VARIABLE TEMP FRACCION EN VOLUMEN
  Real :: Var
  Logical :: Ctrl, Ctrl1, Ctrl2                             !CONTROL LOGICO

  Integer :: istat1, istat2
  Character (len=80) :: err_msg1, err_msg2


  !PARAMETROS DE SIMULADOR
  N = 800
  NStep =100000
  ISave = 100                                            !G(r)
  ISave2 = 100                                           !W(t), D(t)
  iPrint = 1000
  dt = 0.0004

  phi = 4.4D-4
  Dens = 6*phi/pi
  YukA = 556.0
  YukZk = 0.149
  YukA = YukA * Exp( YukZk )
  NN = ( NStep- CEq ) / ISave
  !Write(*,*) phi !DEBUG

  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "NUMERO DE PARTICULAS"
  Write(*,*) N
  Write(*,*) "CONCENTRACION REDUCIDA"
  Write(*,*) Dens
  Write(*,*) "NUMERO DE CICLOS"
  Write(*,*) NStep
  Write(*,*) "MONITOREO EN PANTALLA (CADA CAUNTOS CICLOS)"
  write(*,*) IPrint
  Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION ( G(r) )"
  write(*,*) ISave
  Write(*,*) "NUMERO DE PASOS PARA GUARDAR CONFIGURACION ( W(t),D(t)  )"
  write(*,*) ISave2
  Write(*,*) "PASO DE TIEMPO"
  write(*,*) DT
  Write(*,*) "============================================================================="

  !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLO DE POSICION DE PARTICULAS
  Allocate( X(N), Y(N), Z(N), STAT= istat1 , ERRMSG=err_msg1  )
  Allocate( XR(N), YR(N), ZR(N) )

  !ALOJAR ESPACIO EN MEMORIA PARA LAS FUERZAS DE INTERACCION
  Allocate( FX(N), FY(N), FZ(N) )

  !ALOJAR ESPACIO EN MEOMORIA PARA LOS ARREGLOS DE CONFIGURACION
  Allocate( CX(N,NN), CY(N,NN), CZ(N,NN), STAT= istat2 , ERRMSG=err_msg2 )
  Allocate( CXD(N,NN), CYD(N,NN), CZD(N,NN) )
  
  
  !GENERAR LA CONFIGURACION INICIAL
  Call ConfigIni
  Write(*,*) "CONFIGURACION INICIAL LISTA"
  
  !CALCULO/PARAMETROS PARA INICIALIZAR
  RCut = BoxL / 2.0
  Var = sqrt(2.0*dt)
  k2 = 0                                                                  !G(r)
  ki = 0                                                                  !W(t), D(t)
 

  !CALCULO DE FUERZAS DE LA CONFIGURACION INICIAL
  Open(3, File="Terma.dat" )
  Call Fuerza(iStep)

  
  !ABRIENDO ARCHIVOS PARA GUARDAR INFO DEL SISTEMA
  Open(2, File="ConFin.dat")


  !MOVIMIENTO DE PARTICULAS
  
  
  Configuracion: Do iStep = 1, NStep
     
     Particula: Do i = 1, N

        !GENERAR VALORES ALEATORIO CON DISTRIBUCION GAUSSIANA
        Call RanGauss(RanX)
        Call RanGauss(RanY)
        Call RanGauss(RanZ)

        !MOVIMINETO DE PARTICULAS EN BASE A LA ECUACION DE LANGEVIN
        !SOBREAMORTIGUADA, O REGIMEN DIFUSIVO
        !ALGORITMO DE EMARK PARA EL DESPLAZAMIENTO

        X(i) = X(i) + FX(i) * dt + Var * RanX
        Y(i) = Y(i) + FY(i) * dt + Var * RanY
        Z(i) = Z(i) + FZ(i) * dt + Var * RanZ

        XR(i) = XR(i) + FX(i) * dt + Var * RanX
        YR(i) = YR(i) + FY(i) * dt + Var * RanY
        ZR(i) = ZR(i) + FZ(i) * dt + Var * RanZ
                     
        !CONDICIONES PERIODICAS (MANTENER MISMA N EN TODA CONFIGURACION)
        X(i) = X(i) - BoxL*Anint( X(i) / BoxL )
        Y(i) = Y(i) - BoxL*Anint( Y(i) / BoxL )
        Z(i) = Z(i) - BoxL*Anint( Z(i) / BoxL )      
        
        !FIN DE MOVIMIENTO DE PARTICULAS DE LA CONFIGURACION ISTEP   
        
     End Do Particula
     
     !ALMACENANDO CONFIGURACIONES DE EQUILIBRIO CX, CY, CZ PARA LA G(R)
     Ctrl1 =  Mod( iStep, iSave) == 0
     Ctrl2 = iStep .GT. CEq
     Ctrl = Ctrl1 .AND. Ctrl2
 
     ConfigGR: If (Ctrl) Then
        k2 = k2 + 1
        
        SAV:Do k = 1 , N

           CX(k,k2) = X(k)
           CY(k,k2) = Y(k)
           CZ(k,k2) = Z(k)
           !Write(*,*) "ENTRO"
           
        End Do SAV
        
     End If ConfigGR

     !ALMACENANDO CONFIGURACIONES DE EQUILIBRIO CXD, CYD, CZD PARA W(t) Y D(t)
     Ctrl1 =  Mod( iStep, iSave2) == 0
     Ctrl2 = iStep .GT. CEq
     Ctrl = Ctrl1 .AND. Ctrl2
 
     ConfigWt: If (Ctrl) Then

        ki = ki + 1

        SAV1:Do k = 1 , N

           CXD(k,ki) = XR(k)
           CYD(k,ki) = YR(k)
           CZD(k,ki) = ZR(k)

        End Do SAV1

     End If ConfigWt

     !CALCULO DE FUERZAS DE LAS CONFIGURACIONES
     Call Fuerza(iStep)
     
  End Do Configuracion
  
  Write(*,*) "DONE CONFIGURACIONES"
  Close(3)

  !GUARDAR CONFIG FINAL
  ConfigFin: Do i=1, N
     
     Write(2,*) X(i), Y(i), Z(i)
     
  End Do ConfigFin
  Close (2)
  WRITE(*,*) "DONE SAVING CONFIG FINAL"
  Deallocate( X, Y, Z, XR, YR, ZR )
  WRITE(*,*) "CLEAR MEMORY POSICIONES" !DEBUG

  !CALCULO DE PROPIEDADES
  WRITE(*,*) "CALCULANDO GDR" !DEBUG
  Call GdrCalc
  WRITE(*,*) "GDR DONE CALC" !DEBUG
  Deallocate( CX, CY, CZ )
  WRITE(*,*) "CLEAR MEMORY CONFIGURACIONES G(r)" !DEBUG

  !CALCULO DE PROPIEDADES DE WDT
  Write(*,*) "CALCULADO WDT" !DEBUG
  Call WDT(ki)
  WRITE(*,*) "WDT DONE CALC" !DEBUG
  Deallocate( CXD, CYD, CZD )
  WRITE(*,*) "CLEAR MEMORY CONFIGURACIONES W(t)" !DEBUG
  
  
  WRITE(*,*) "DONE"
  
  
End Program Main
