SUBROUTINE GDR(CX,CY,CZ,KI)
  IMPLICIT DOUBLE PRECISION(A-H,O-Z)
  PARAMETER ( N = 250 )
  PARAMETER(NN2=5000)
  PARAMETER(NN3=3500)
  INTEGER NHIST(NN3)
  COMMON /VALORES/ DENS,RCUT,BOX,NSTEP
  DIMENSION CX(n,nn2),CY(n,nn2),CZ(n,nn2)
  
  NP=N

  NHIST=0

  DELTAR=0.01E0
  MAXBIN=INT(RCUT/DELTAR)
  PI=3.141592
  NTMAX=KI

  DO L=1,NP

     DO M=1,NP

        IF (M /= L) Then

           DO J=1,NTMAX
              XL0=CX(L,J)
              XLT=CX(M,J)
              XL0T=XL0-XLT

              YL0=CY(L,J)
              YLT=CY(M,J)
              YL0T=YL0-YLT

              ZL0=CZ(L,J)
              ZLT=CZ(M,J)
              ZL0T=ZL0-ZLT

              XL0T=XL0T-BOX*ANINT(XL0T/BOX)
              YL0T=YL0T-BOX*ANINT(YL0T/BOX)
              ZL0T=ZL0T-BOX*ANINT(ZL0T/BOX)
              R0T=SQRT(XL0T**2+YL0T**2+ZL0T**2)
              NBIN=INT(R0T/DELTAR)+1

              IF (NBIN.LE.MAXBIN)THEN
                 NHIST(NBIN)=NHIST(NBIN)+1
              END IF
              
           End DO
           
        End IF

     End Do

  End DO

  C1=(4.0/3.0)*(PI*DENS)

  OPEN(60,FILE='grdm0.dat',STATUS='UNKNOWN')

  DO NBIN=1,MAXBIN
     
     RL=REAL(NBIN-1)*DELTAR
     RU=RL+DELTAR
     RT=RL+DELTAR/2.0
     C2=C1*(RU**3-RL**3)
     GDRTA=REAL(NHIST(NBIN))/REAL(NTMAX)/REAL(NP)/C2
     WRITE(60,*)SNGL(RT),SNGL(GDRTA)
     
  End DO

  CLOSE(60)
  RETURN
END SUBROUTINE GDR
