! ! DINAMICA BROWNIANA
! codigo DBI

! N: Numero de particulas
! NSTEP: Numero total de configuraciones
! NENER: Configuracion inicial de Termalizacion (Equilibrio)
! dt: Tiempo de paso
! phi: Fraccion de volumen
! Nc: Num. de particulas coloidales
! A y Zk: Parametros del potencial Yukawa
! u(r) = A*exp[-z*(r-1)]/r <--- Forma del potencial
! NFREC: Frec. de almacenamiento de configuraciones
! NFREC2: Frec. para el calculo de w(t)
! sigma: diametro de las particulas coloidales
! Ls = Longitud de la arista de la simulacion
! rcut: radio de corte
! varianza


! SUBRUTINAS A UTILIZARS
! Fuerzas: Esta subrutina calcula la fuerza de interaccion de
! las particulas y la energia de la configuracion.
! gdr: Esta subrutina calcula la funcion g(r)
! wdt: subrutina para el calculo del desplazamiento cuadratico
! medio y coeficiente de difusion dependiente del tiempo
! zran: Esta subrutina genera numeros aleatorios
! azarg: Subrutina para calcular numeros aleatorios con dist. gaussiana
! configini: configuracion inicial aleatoria sin traslapes en 3D


PROGRAM DBI

  Use cte

  Implicit None
  Integer :: L, i
  Real    :: ranx, rany, ranz
  Integer :: xmod
  Integer :: error

  Dt      = 0.0004
  NFREC   = 1000
  NFREC2  = 1000
  NSTEP   = 50000

  Ns  = 0.6    !6.0 * FI/PI
  T = 0.01
  ji = 3.0
  eta = 0.0
  !write(*,*) Nstep
  NN2 = (NSTEP-NENER)/NFREC

  sigma = 1.E0

  Allocate (X(Nc), Y(Nc), Z(Nc), stat= error)
  !Write(*,*) error

  Allocate (CXR(Nc, NN2), CYR(Nc, NN2), CZR(Nc, NN2), stat= error)
  !Write(*,*) error

  Allocate (FX(Nc), FY(Nc), FZ(Nc), stat= error)
  !Write(*,*) error

  Allocate (CX(Nc, NN2), CY(Nc, NN2), CZ(Nc, NN2), stat= error)
  !Write(*,*) error

  Allocate (XR(Nc), YR(Nc), ZR(Nc), stat= error)
  !Write(*,*) error
  
  
  

  ! Llamamos a la configuracion inicial

  Call CONFIGINI

  Ls   = ((1.E0 * Nc)/ns)**(1.0/3.0)
  rcut = Ls/2.E0
  varianza = Sqrt(2.E0 * Dt)

  KI   = 0
  KI2  = 0

  L    = 1

  ! Calculo de la fuerza sobre cada particula en la configuracion
  ! inicial y la energia de la configuracion por medio de la subrutina
  ! FUERZAS

  Open (16, File = 'Termalizacion.txt')

  Call Fuerzas (L)

  OPEN (21, File = "ConFin.txt")

  ! **********************
  ! MOVIMIENTO BROWNIANO *
  ! **********************

  !En este ciclo construimos las configuraciones
  
  Do L = 1, NSTEP

     ! Aqui movemos a las particulas coloidales

     Do i = 1, Nc

        ! Aqui generamos 3 numeros aleatorios
        Call RanGauss(ranx)
        Call RanGauss(rany)
        Call RanGauss(ranz)


        ! Aqui movemos a las particulas en base a la ec. de Langevin sobre
        ! amortiguado o regimen difuso algoritmo de Ermak para el desplazamiento

        X(i) = X(i) + FX(i)* Dt + varianza*ranx
        Y(i) = Y(i) + FY(i)* Dt + varianza*rany
        Z(i) = Z(i) + FZ(i)* Dt + varianza*ranz

        XR(i) = XR(i) + FX(i) * Dt + varianza*ranx
        YR(i) = YR(i) + FY(i) * Dt + varianza*rany
        ZR(i) = ZR(i) + FZ(i) * Dt + varianza*ranz
        !f ( (i == 1 .OR. i == 2).AND. L==1  ) Write(*,*) XR(i)
        
        ! Aplicamos condiciones periodicas PACMAN :)

        X(i) = X(i) - Ls * Anint(X(i)/Ls)
        Y(i) = Y(i) - Ls * Anint(Y(i)/Ls)
        Z(i) = Z(i) - Ls * Anint(Z(i)/Ls)

     End Do


     ! Aqui se decide si se almacenan las configuraciones de equilibrio
     ! en las matrices CX, CY y CZ para calcular a g(r)


     xmod = MOD(L, NFREC)
    !If (MOD(L,100) .EQ. 0) !Write(*,*) L

     If (xmod .EQ. 0.0 .AND. L .GT. NENER) Then

        KI = KI +1

        Do i = 1, Nc

           CX(i, KI) = X(i)
           CY(i, KI) = Y(i)
           CZ(i, KI) = Z(i)

        END DO

     END IF


     !---> DECISION
     !-----------ALMACENAMIENTO DE LAS CONFIGURACIONES DE EQUILIBRIO
     !           EN LAS MATRICES CDX, CDY, Y CDZ PARA CALCULAR W(t) Y D(t)


     XMOD = MOD(L, NFREC2)
     IF (XMOD.EQ. 0.0 .AND. L .GT. NENER) THEN

        KI2 = KI2 +1

        DO i = 1, NC

           CXR(i, KI2) = XR(i)
           CYR(i, KI2) = YR(i)
           CZR(i, KI2) = ZR(i)

        End Do

     End If


     Call FUERZAS (L)

  End Do

  

  Do  i = 1, Nc

     Write(21,*) X(i), Y(i), Z(i)
  End Do
  Write(*,*) "Ya se cuenta con las configuraciones"
  !Deallocate( X, Y, Z, stat=error)
  !write(*,*) error
  !Deallocate(XR, YR, ZR, stat=error)
  !write(*,*) error
  write(*,*) "whatevs"
  Call GR

  Write(*,*) "Ya se calculo a g(r)"

  !Deallocate (CX, CY, CZ, stat =error)
  !write(*,*) error

  call WDT
  Write(*,*) "Ya se calculo a wt"
  !Deallocate(CXR, CYR, CZR)


End Program DBI
