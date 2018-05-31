!=========================================================================
! SUBRUTINA PARA EL CALCULO DE FUERZAS DE INTERACCION Y ENERGIA DE LA
! CONFIGURACION PARA LA SIMULACION DE DINAMICA BROWNIANA
!
! AUTOR : MARTIN ALEJANDRO PAREDES SOSA
!=========================================================================

Subroutine Fuerza(L)
  Use cte
  Implicit None
  Real :: EnePot, U, U2, U3                                            !ENERGIA
  Real :: FXI, FYI, FZI, fxij, fyij, fzij                          !FUERZAS TEMP
  Real :: xij, yij, zij, rij                                       !POSICIONES
  Real :: Pres, Pres1

  Integer :: i, j, L                                               !CONTADORES ("L" CONTADOR DE LA CONFIGURACION)
  Logical :: Ctrl1, Ctrl2

  !INICIALIZANDO
  EnePot = 0.0
  FX = 0.0
  FY = 0.0
  FZ = 0.0

  Pres1 = 0.0

  Parti1: Do i = 1, N - 1

     FXI = FX(i)
     FYI = FY(i)
     FZI = FZ(i)

     Parti2: Do j = i + 1, N

        !SEPARACION
        xij = X(i) - X(j)
        yij = Y(i) - Y(j)
        zij = Z(i) - Z(j)
        
        !CONDICION DE IMAGEN MINIMA
        xij = xij - BoxL * Anint( xij / BoxL )
        yij = yij - BoxL * Anint( yij / BoxL )
        zij = zij - BoxL * Anint( zij / BoxL )

        !DISTANCIA
        rij = sqrt( xij*xij + yij*yij + zij*zij )

        !TRASLAPES
        Ctrl1 = rij .LE. 1.0
        Traslape : If(Ctrl1) Then
           
           Write(*,*) "TRASLAPE", i, j
           
        End If Traslape

        !IMPLEMENTACION DEL POTENCIAL
        Ctrl2 = rij .LT. RCut
        Potencial: If(Ctrl2) Then 
           
           U = Exp( -YukZk * rij )
           U2 = YukA * U  * (YukZk * rij + 1.0 ) / (rij**3)
           U3 = U2 * rij * rij
           EnePot = (YukA * U) / rij + EnePot

           fxij = xij * U2
           fyij = yij * U2
           fzij = zij * U2

           FXI = FXI + fxij
           FYI = FYI + fyij
           FZI = FZI + fzij

           FX(j) = FX(j) - fxij
           FY(j) = FY(j) - fyij
           FZ(j) = FZ(j) - fzij

           !PRECALCULO DE PRESION
           Pres1 = Pres1 + U3 

        End If Potencial
        
     End Do Parti2

     !GUARDANDO FUERZA
     FX(i) = FXI
     FY(i) = FYI
     FZ(i) = FZI
     
  End Do Parti1

  !CALCULO DE PRESION
  Pres = dens + (dens / (3.0 * real(N) ) ) * Pres1



  !GUADANDO TERMALIZACION (ENERGIA POR PARTICULA)
  Write(3,*) L , EnePot / Real(N), Pres
  
  If( mod(L , iPrint) == 0 ) Then
     Write(*,*) L , EnePot / Real(N) , Pres                           !MONITOREO EN PANTALLA
  End If

End Subroutine Fuerza














!FUNCION DEL POTENCIAL         !FUTURA IMPLEMENTACION
!Real*8 Function U2(U, rij)

!  Use cte
!  Implicit None
!  Real :: U, rij
!  U2 = YukA * U  * (YukZk * rij + 1.0 ) / (rij**3)
!End Function U2
