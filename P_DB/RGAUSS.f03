Subroutine RanGauss (Gau)

  Use cte
  Implicit None

  Real :: Ran1, Ran2, Gau


1 Call Random_Number (Ran1)
  Call Random_Number (Ran2)

  ! Condicion para evitar que el logaritmo diverja

  If (Ran1 .LE. 1E-8) Then

     Go To 1

  End If

  ! Aqui se generan los numeros aleatorios con distribucion Gaussiana

  Gau =  sqrt(-2.0 * Log(Ran1) ) * Cos(2.0 * Pi * Ran2)


End Subroutine RanGauss
