!============================================================================
! CALCULO DE LA ENERGIA DE UNA DE LAS PARTICULAS DE LA CELDA
! POZO CUADRADO (SW)
!
! Autor: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine EnergyPart(Rx1, Ry1, i, V)
  Use cte
  Implicit None
  Real :: V, VNew, Dist, Rx1, Rxd, Ry1, Rz1, Rzd, Ryd
  Integer :: i, j
  !INICIAR ENERGIA EN 0
  V = 0

  BuscarPart: Do j=1, N

     NoLaMisma: If(i .NE. j) Then

        Rxd = Rx1 - X(j)
        Ryd = Ry1 - Y(j)
	Rzd = Rz1 - Z(j)

        !CONDICION DE IMAGEN MINIMA (LOCALIZAR PARTICULAS EN CELDAS CERCANAS)
        Rxd = Rxd - BoxL*Anint(Rxd/BoxL)
        Ryd = Ryd - BoxL*Anint(Ryd/BoxL)
	Rzd = Rzd - BoxL*Anint(Rzd/BoxL)

        !INGRESANDO MODELO DE INTERACCON (DISCOS DUROS)
        Dist = sqrt( Rxd*Rxd + Ryd*Ryd + Rzd*Rzd )
        !If(Dist .LE. 1.0) Write(*,*) Dist, i,j
        
        ChecarInter: If(Dist .LT. RCut)  Then
           
           ChecarCercania: If (Dist .LE. 1.0) Then                    ! DESPUES DEL POZO
              VNew = 1.0E+10
           Else If( (Dist .GT. 1.0) .AND. (Dist .LT. Lambda)  ) Then  ! EL POZO
              VNew = -1.0 / TP
           Else                                                       ! ANTES DEL POZO
              VNew = 0
           End If ChecarCercania
           V = V + VNew
        End If ChecarInter

        
     End If NoLaMisma
     
  End Do BuscarPart
  
End Subroutine EnergyPart
