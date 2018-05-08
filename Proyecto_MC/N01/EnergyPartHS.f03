!============================================================================
! CALCULO DE LA ENERGIA DE UNA DE LAS PARTICULAS DE LA CELDA
! ESFERA DURA (HS)
!
! Autor: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine EnergyPart(Rx1, Ry1, Rz1, i, V)
  Use cte
  Implicit None
  Real :: VNew, Dist, Rxd, Rzd, Ryd
  Real, Intent(In) :: Rx1, Ry1, Rz1
  Real, Intent(Out) :: V
  Integer, Intent(In) :: i
  Integer :: j
  !INICIAR ENERGIA EN 0
  V = 0.0

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
           
           ChecarCercania: If (Dist .LE. 1.0) Then
              VNew = 1.0E+10
           Else
              VNew = 0
           End If ChecarCercania
           V = V + VNew
        End If ChecarInter

        
     End If NoLaMisma
     
  End Do BuscarPart
  
End Subroutine EnergyPart
