Subroutine Conc(Dens2)
  Use cte
  Implicit None

  Integer :: i, k
  Logical :: Ctrl1, Ctrl2
  Real :: Dens2

  !CHECAR CONCENTRACION FINAL EN LA CELDA ORIGINAL
  k=0                                                       !CONTADOR DE PARTICULAS DENTRO DE LA CELDA ORIGINAL
  Partic: Do i=1 , N
     Ctrl1 = X(i) .LT. Boxl/2.0 .AND. X(i) .GT. -Boxl/2.0
     Ctrl2 = Y(i) .LT. Boxl/2.0 .AND. Y(i) .GT. -Boxl/2.0
     DENS_VERIFY: If (Ctrl1 .AND. Ctrl2) Then
        k = k + 1
     End If DENS_VERIFY
  End Do Partic
  Dens2 = k / BoxL**Dim                  !CALCULAR NUEVA DENSIDAD
  Write(*,*) "DENSIDAD FINAL DE LA CELDA ",Boxl," ES: ", Dens2
  
End Subroutine Conc
