!======================================================================
! SUBRUTINA GENERADORA DE VALORES ALEATORIOS CON UNA DISTRIBUCION
! GAUSSIANA.
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
! MODIFICACION DEL DE GABY
!======================================================================

Subroutine RanGauss(Gauss)
  Use cte
  Implicit None
  
  Real :: Rand1, Rand2 !VALORES ALEATORIOS CON DISTRIBUCION UNIFORME
  Real :: Gauss        !VALORES ALEATORIOS CON DISTRIBUCION GAUSSIANA
  
  !GENERANDO NUMEROS ALEATORIOS UNIFORMES
12 Call random_number(Rand1)
  Call random_number(Rand2)
  
  !PREVENIR QUE DIVERJA EL LOGARITMO AL EVALUARLO EN Rand1   
  Diver:If (rand1.le.1E-8) Then
     go to 12
  End If Diver
  
  !GENERANDO UN NUMERO ALEATORIOS CON DISTRIBUCION GAUSSIANA
  Gauss = Sqrt ( -2.0 * log(Rand1) ) * cos(2.0 * pi * Rand2)
  
  
End Subroutine RanGauss
