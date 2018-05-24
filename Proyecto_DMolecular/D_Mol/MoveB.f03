SUBROUTINE MOVEB ( DT, XM, XK )
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N = 250 )
  COMMON /BLOCK1/ RX, RY, RZ, VX, VY, VZ, FX, FY, FZ
  DIMENSION RX(N), RY(N), RZ(N)
  DIMENSION VX(N), VY(N), VZ(N), FX(N), FY(N), FZ(N)

  DT2 = DT / 2.0
  XK = 0.0

  Do I = 1, N

     VX(I) = VX(I) + DT2 * FX(I) / XM
     VY(I) = VY(I) + DT2 * FY(I) / XM
     VZ(I) = VZ(I) + DT2 * FZ(I) / XM

     XK = XK + VX(I) ** 2 + VY(I) ** 2 + VZ(I) ** 2
  End Do

  XK = 0.5 * XM * XK

END SUBROUTINE MOVEB
