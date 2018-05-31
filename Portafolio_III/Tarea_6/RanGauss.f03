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
  
  !generando números aleatorios uniformes
12 call random_number(Rand1)
  call random_number(Rand2)
  
  !para evitar que diverja el logaritmo al evaluarlo en rand1
  if (rand1.le.1E-8) then
     go to 12
  end if
  
  !generando un número aleatorios con distribución gaussiana
  Gauss = Sqrt ( -2.0 * log(Rand1) ) * cos(2.0 * pi * Rand2)
  
  
end subroutine RanGauss
