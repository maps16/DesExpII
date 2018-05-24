SUBROUTINE COMVEL ( TEMP )
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N = 250 )
  COMMON /BLOCK1/ RX, RY, RZ, VX, VY, VZ, FX, FY, FZ
  COMMON /semillas/iseed3,iseed2,iseed1
  DIMENSION RX(N), RY(N), RZ(N)
  DIMENSION VX(N), VY(N), VZ(N), FX(N), FY(N), FZ(N)

  ISEED = 43560
  ISEED1= 39467
  ISEED2= 148420
  ISEED3= 7845901

  CALL AZARG(iseed,AX)
  CALL AZARG(iseed,AY)
  CALL AZARG(iseed,AZ)

  RTEMP = SQRT ( TEMP )

  DO I = 1, N
     VX(I) = RTEMP * AX
     VY(I) = RTEMP * AY
     VZ(I) = RTEMP * AZ
  End DO

  !INICIALIZANDO EN CERO 
  SUMX = 0.0
  SUMY = 0.0
  SUMZ = 0.0

  DO I = 1, N
     SUMX = SUMX + VX(I)
     SUMY = SUMY + VY(I)
     SUMZ = SUMZ + VZ(I)
  End DO

  SUMX = SUMX / REAL ( N )
  SUMY = SUMY / REAL ( N )
  SUMZ = SUMZ / REAL ( N )

  DO I = 1, N
     VX(I) = VX(I) - SUMX
     VY(I) = VY(I) - SUMY
     VZ(I) = VZ(I) - SUMZ
  End DO

END SUBROUTINE COMVEL
