SUBROUTINE FORCE (SIGMA, RCUT, BOX, V, W )
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N = 250 )
  COMMON /BLOCK1/ RX, RY, RZ, VX, VY, VZ, FX, FY, FZ
  DIMENSION RX(N), RY(N), RZ(N), VX(N), VY(N), VZ(N), FX(N),FY(N), FZ(N)

  BOXINV = 1.0 / BOX
  RCUTSQ = RCUT ** 2
  SIGSQ = SIGMA ** 2
  EPS4 = 4.0
  EPS24 = 24.0

  Do  I = 1, N
     FX(I) = 0.0
     FY(I) = 0.0
     FZ(I) = 0.0
  End Do
  
  V = 0.0
  W = 0.0

  DO  I = 1, N - 1
     RXI = RX(I)
     RYI = RY(I)
     RZI = RZ(I)
     FXI = FX(I)
     FYI = FY(I)
     FZI = FZ(I)

     DO  J = I + 1, N
        RXIJ = RXI - RX(J)
        RYIJ = RYI - RY(J)
        RZIJ = RZI - RZ(J)
        RXIJ = RXIJ - ANINT ( RXIJ * BOXINV ) * BOX
        RYIJ = RYIJ - ANINT ( RYIJ * BOXINV ) * BOX
        RZIJ = RZIJ - ANINT ( RZIJ * BOXINV ) * BOX
        RIJSQ = RXIJ ** 2 + RYIJ ** 2 + RZIJ ** 2

        IF ( RIJSQ .LT. RCUTSQ ) THEN
           SR2 = SIGSQ / RIJSQ
           SR6 = SR2 * SR2 * SR2
           SR12 = SR6 ** 2

           VIJ = SR12 - SR6
           V = V + VIJ
           WIJ = VIJ + SR12
           W = W + WIJ 
           FIJ = WIJ / RIJSQ
           FXIJ = FIJ * RXIJ
           FYIJ = FIJ * RYIJ
           FZIJ = FIJ * RZIJ

           FXI = FXI + FXIJ
           FYI = FYI + FYIJ
           FZI = FZI + FZIJ
           FX(J) = FX(J) - FXIJ
           FY(J) = FY(J) - FYIJ
           FZ(J) = FZ(J) - FZIJ
        End If
     End Do
     FX(I) = FXI
     FY(I) = FYI
     FZ(I) = FZI
  End Do

  Do I = 1, N
     FX(I) = FX(I) * EPS24
     FY(I) = FY(I) * EPS24
     FZ(I) = FZ(I) * EPS24
  End Do
  
  V = V * EPS4
  W = W * EPS24 / 3.0
  
END SUBROUTINE FORCE
