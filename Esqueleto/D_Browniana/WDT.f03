!==========================================================================================
! SUBRUTINA DE CALCULO DE PROPIEDADES DE AUTO DIFUSION. DESPLAZAMIENTO CUADRATICO MEDIO Y
! COEFICIENTE DE DIFUSION DEPENDIENTE DEL TIEMPO
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!==========================================================================================

Subroutine WDT

  Use cte
  Implicit None

  Integer :: i                                             !CONTADORES
  Real :: Time                                             

  Open(96, File="wdt.dat")

  !TIEMPO ENTRE CONFIGURACIONES
  Time = Real(iSave2) * dt                                 

  !BARRIDO TEMPORAL
  TEMPO: Do i = 1, 


End Subroutine WDT
