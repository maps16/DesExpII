SUBROUTINE WDT(CX,CY,CZ,KI,DT,NFREC)
  PARAMETER (N=250)
  PARAMETER (NN2=5000)
  IMPLICIT REAL*8 (A-H,O-Z)
  DIMENSION CX(N,NN2),CY(N,NN2),CZ(N,NN2)
  COMMON /VALORES/ DENS,RCUT,BOX,NSTEP

  TIM=REAL(NFREC)*DT

  open(50,file='wdt.dat',STATUS='UNKNOWN')

  DO I=1, KI-1
     
     NTMAX=KI-I
     WTX=0.d0
     WTY=0.d0
     WTZ=0.d0
     WT= 0.d0

     DO L=1,N
        DO J=1,NTMAX

           WTX=WTX+( CX(L,I+J)-CX(L,J) )**2
           WTY=WTY+( CY(L,I+J)-CY(L,J) )**2
           WTZ=WTZ+( CZ(L,I+J)-CZ(L,J) )**2
           
        END DO
     END DO

     TIME=TIM*REAL(I)
     WT=(WTX+WTY+WTZ)/REAL(NTMAX)/REAL(N)/6.D0
     DIF=WT/TIME
     WRITE(50,*)TIME,WT,DIF
     if(time.gt.10)goto 11

  END DO
11 CLOSE(50)

END SUBROUTINE WDT
