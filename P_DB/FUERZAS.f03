Subroutine Fuerzas (L)

  Use cte

  Implicit None

  Real :: Enpot, Fxij, Fyij, Fzij
  Real :: Ps1, Ps2
  Real :: xij, yij, zij, Fxi, Fyi, Fzi, rij
  Integer :: i, j, L

  Enpot = 0.0

  Fx = 0.0
  Fy = 0.0
  Fz = 0.0

  Du2 = 0.0


  DO i = 1, Nc - 1

     Fxi = Fx(i)
     Fyi = Fy(i)
     Fzi = Fz(i)

     DO j = i + 1, Nc

        xij = X(i) - X(j)
        yij = Y(i) - Y(j)
        zij = Z(i) - Z(j)

        !EFECTO PACMAN
        xij = xij - Ls*Anint(xij/Ls)
        yij = yij - Ls*Anint(yij/Ls)
        zij = zij - Ls*Anint(zij/Ls)


        rij = sqrt(xij**2 + yij**2 + zij**2)


        ! Modelo de potencial 

        If (rij .LT. RCut) Then
           ! Yukawa
           !U   = exp(-ZK * rij)
           !U2  = A * U *(ZK * rij + 1)/(rij ** 3)
           !DOBLE GAUSSIANO
           U = (1.0/T)*exp(-(rij**2))-eta*exp(-(rij-ji)**2)
           U2 = 2.0*(U + ( (eta*ji) * exp(-(rij-ji)**2 ) ) / rij )

           Du2 = (2.0 * U * rij * rij) + Du2

           Enpot = U + Enpot

           Fxij = (xij)* U2
           Fyij = (yij)* U2
           Fzij = (zij)* U2

           Fxi = Fxi + Fxij
           Fyi = Fyi + Fyij
           Fzi = Fzi + Fzij

           FX(j) = FX(j) - Fxij
           FY(j) = FY(j) - Fyij
           FZ(j) = FZ(j) - Fzij

        End If

        Fx(i) = Fxi
        Fy(i) = Fyi
        Fz(i) = Fzi




     End Do


  End Do

  ! Calculo de la presion

  Ps1 = ns + (ns/ (3.0 * Real(Nc))) * Du2

  Write(16, *) L, Enpot/real(Nc)

  If (MOD(L, NFREC).EQ.0.0)   Then

    Write(*,*)  L, Enpot/real(Nc), Ps1

  End If


End Subroutine Fuerzas
