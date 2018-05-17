!============================================================================
! CALCULO DE LA ENERGIA DE UNA DE LA CONFIGURACION DE LA CELDA
!
! Autor: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine EnergyConfig(V)
  Use cte
  Implicit None
  Real ::  V, Rx1, Rxd, Ry1, Ryd, Dist, VNew
  Integer :: i, j
  V = 0
  IterPart: Do i=1, N-1

     Rx1 = X(i)
     Ry1 = Y(i)

     IterPart2: Do j = i+1, N 
        Rxd = Rx1 - X(j)
        Ryd = Ry1 - Y(j)

        !CONDICION DE IMAGEN MINIMA (LOCALIZAR PARTICULAS EN CELDAS CERCANAS)
        Rxd = Rxd - BoxL*Anint(Rxd/BoxL)
        Ryd = Ryd - BoxL*Anint(Ryd/BoxL)

        !INGRESANDO MODELO DE INTERACCON (DISCOS DUROS)
        Dist = sqrt( Rxd*Rxd + Ryd*Ryd  )
        
        ChecarInter: If(Dist .LT. RCut)  Then
           
           ChecarCercania: If (Dist .LE. 1.0) Then
              VNew = 1.0E+10
           Else
              VNew = 0
           End If ChecarCercania

           V = V + VNew
        End If ChecarInter

     End Do IterPart2
  End Do IterPart

End Subroutine EnergyConfig
