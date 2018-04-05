!============================================================================
! CALCULO DE LA ENERGIA DE UNA DE LA CONFIGURACION DE LA CELDA
!
! Autor: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine EnergyConfig(V)
  Use cte
  Implicit None
  Integer :: i, j                                                     !CONTADORES
  Real :: U, U2
  Real ::  V, Rx1, Rxd, Ry1, Ryd, Rz1, Rzd, Dist, VNew                !PARAMTROS DE CALCULO DE ENERGIA
  Real, Parameter :: YukA = 556.0                                     !PARAMETROS DE YUKAWA
  Real, Parameter :: YukZ = 0.149                                     !PARAMETROS DE YUKAWA
  V = 0
  IterPart: Do i=1, N-1

     Rx1 = X(i)
     Ry1 = Y(i)
     Rz1 = Z(i)
     
     IterPart2: Do j = i+1, N 
        Rxd = Rx1 - X(j)
        Ryd = Ry1 - Y(j)
        Rzd = Rz1 - Z(j)
        
        !CONDICION DE IMAGEN MINIMA (LOCALIZAR PARTICULAS EN CELDAS CERCANAS)
        Rxd = Rxd - BoxL*Anint(Rxd/BoxL)
        Ryd = Ryd - BoxL*Anint(Ryd/BoxL)
        Rzd = Rzd - BoxL*Anint(Rzd/BoxL)
        
        !INGRESANDO MODELO DE INTERACCON (YUKAWA)
        Dist = sqrt( Rxd*Rxd + Ryd*Ryd + Rzd*Rzd )
        
        ChecarInter: If(Dist .LT. RCut)  Then

           U = Exp(-YukZ * Dist)
           V = (YukA * U) * Dist + V
           
        End If ChecarInter

     End Do IterPart2
  End Do IterPart

End Subroutine EnergyConfig
