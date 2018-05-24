SUBROUTINE MOVEA ( DT, XM )
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N = 250 )
  COMMON /BLOCK1/ RX, RY, RZ, VX, VY, VZ, FX, FY, FZ
  COMMON /BLOCK2/ RXC, RYC, RZC
  COMMON /VALORES/ DENS,RCUT,BOX,NSTEP

  DIMENSION RX(N), RY(N), RZ(N), RXC(N), RYC(N), RZC(N)
  DIMENSION VX(N), VY(N), VZ(N), FX(N), FY(N), FZ(N)

  DT2 = DT / 2.0
  DTSQ2 = DT * DT2

  DO I = 1, N
     RX(I) = RX(I) + DT * VX(I) + DTSQ2 * FX(I) / XM
     RY(I) = RY(I) + DT * VY(I) + DTSQ2 * FY(I) / XM
     RZ(I) = RZ(I) + DT * VZ(I) + DTSQ2 * FZ(I) / XM
     
     RXC(I) = RXC(I) + DT * VX(I) + DTSQ2 * FX(I) / XM
     RYC(I) = RYC(I) + DT * VY(I) + DTSQ2 * FY(I) / XM
     RZC(I) = RZC(I) + DT * VZ(I) + DTSQ2 * FZ(I) / XM

     RX(I)=RX(I)-BOX*ANINT(RX(I)/BOX)
     RY(I)=RY(I)-BOX*ANINT(RY(I)/BOX)
     RZ(I)=RZ(I)-BOX*ANINT(RZ(I)/BOX)

     VX(I) = VX(I) + DT2 * FX(I) / XM
     VY(I) = VY(I) + DT2 * FY(I) / XM
     VZ(I) = VZ(I) + DT2 * FZ(I) / XM
  End Do
 
END SUBROUTINE MOVEA
