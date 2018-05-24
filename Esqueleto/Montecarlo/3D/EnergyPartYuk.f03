!============================================================================
! CALCULO DE LA ENERGIA DE UNA DE LAS PARTICULAS DE LA CELDA
!
! Autor: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine EnergyPart(Rx1, Ry1, Rz1, i, V)
  Use cte
  Implicit None
  Integer :: i, j                                                     !CONTADORES
  Real :: U, U2
  Real ::  V, Rx1, Rxd, Ry1, Ryd, Rz1, Rzd, Dist, VNew                !PARAMTROS DE CALCULO DE ENERGIA
  Real :: YukA                                                        !PARAMETROS DE YUKAWA
  Real, Parameter :: YukZ = 0.149                                     !PARAMETROS DE YUKAWA
  

  !PARAMETROS DE YUKAWA
  YukA = 556.0
  YukA = YukA * Exp( YukZ )

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
                
        ChecarInter: If(Dist .LT. RCut)  Then
           
           U = Exp(-YukZ * Dist)
           V = (YukA * U) / Dist + V
           
        End If ChecarInter
        
     End If NoLaMisma
     
  End Do BuscarPart
  
End Subroutine EnergyPart
