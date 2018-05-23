! Curso: Desarrollo Experimental II (2018-1)
! Proyecto III:

PROGRAM DMLJ
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N=250)
  PARAMETER ( NN2=5000)
  PARAMETER ( NENER=50000)
  PARAMETER ( FREE=3.0)
  PARAMETER ( PI=3.1415927)
  REAL, EXTERNAL ::ZRAN

  COMMON /BLOCK1/ RX, RY, RZ, VX, VY, VZ, FX, FY, FZ
  COMMON /VALORES/ DENS,RCUT,BOX,NSTEP
  COMMON /BLOCK2/ RXC, RYC, RZC

  DIMENSION CX(n,nn2),CY(n,nn2),CZ(n,nn2)
  DIMENSION CXR(n,nn2),CYR(n,nn2),CZR(n,nn2)
  DIMENSION FX(N), FY(N), FZ(N), VX(N), VY(N), VZ(N)
  DIMENSION RX(N), RY(N), RZ(N), RXC(N), RYC(N), RZC(N)

  NSTEP=150000
  IPRINT=10000
  NFREC=20
  DT=0.0001

  DENS=0.6
  TEMP=1.5

  XM=1.0
  SIGMA=1.0

  A=1.0/3.0
  BOX=(N/DENS)**A
  RCUT=BOX/2.0
  TEMPI=TEMP
  KI2=0

  WRITE(*,*)'LENNARD-JONES'
  WRITE(*,'('' NUMBER OF ATOMS = '',I10 )') N
  WRITE(*,'('' NUMBER OF STEPS = '',I10 )') NSTEP
  WRITE(*,'('' OUTPUT FREQUENCY = '',I10 )') IPRINT
  WRITE(*,'('' POTENTIAL CUTOFF = '',F10.4)') RCUT
  WRITE(*,'('' DENSITY = '',F10.4)') DENS
  WRITE(*,'('' RED. TEMPERATURE = '',F10.4)') TEMP
  WRITE(*,'('' MASS = '',F10.4)') XM
  WRITE(*,'('' TIME STEP = '',F10.6)') DT

  OPEN(15,FILE='cfdm0.dat',STATUS='UNKNOWN')
  OPEN(12,FILE='vfdm0.dat',STATUS='UNKNOWN')
  OPEN(13,FILE='vidm0.dat',STATUS='UNKNOWN')
  OPEN(14,FILE='tedm0.dat',STATUS='UNKNOWN')

  CALL CONFIGINI (BOX,RX,RY,RZ)
  CALL COMVEL (TEMP)

  DO INIV=1 , N

     WRITE(13,*)VX(INIV),VY(INIV),VZ(INIV)

  End Do

  ACV = 0.0
  ACE = 0.0
  ACP = 0.0
  ACT = 0.0
  ACVSQ = 0.0
  ACESQ = 0.0
  ACPSQ = 0.0
  ACTSQ = 0.0
  FLV = 0.0
  FLE = 0.0
  FLP = 0.0
  FLT = 0.0

  SR3 = ( SIGMA / RCUT ) ** 3
  SR9 = SR3 ** 3
  BOXCUB = 1.0/BOX** 3
  VLRC = ( 8.0 /9.0 ) * PI * DENS * REAL ( N )* ( SR9 - 3.0 * SR3)
  WLRC = ( 16.0 / 9.0 ) * PI * DENS * REAL ( N )* ( 2.0 * SR9 -3.0 * SR3 )
  
  CALL FORCE (SIGMA, RCUT, BOX, V, W )
  WRITE(*,*)'    STEP   ','   EN-MEC   ','   EN-CIN   ','   EN-POT   ','    PRES   ','     TEMP   '

  DO ISTEP = 1, NSTEP
     CALL MOVEA ( DT, XM )
     CALL FORCE ( SIGMA, RCUT, BOX, V, W )
     CALL MOVEB ( DT, XM, XK )

     V = V + VLRC
     W = (W + WLRC)*BOXCUB
     E = XK + V

     VN = V / REAL ( N )
     XKN = XK / REAL ( N )
     EN = E / REAL ( N )
     TEMP = 2.0 * XKN / FREE
     PRES = DENS * TEMP + W

     ! COMENTARIO LYR: TERMOSTATO PARA MANTENER LA TEMPERATURA DEL
     ! SISTEMA CONSISTENTE CON LA TEMPERATURA DEL BAÑO TÉRMICO PARA
     ! UNA DESCRIPCION NVT.
     ALFA=SQRT(TEMPI/TEMP)

     Do IS= 1, N , 1

        VX(IS)=ALFA*VX(IS)
        VY(IS)=ALFA*VY(IS)
        VZ(IS)=ALFA*VZ(IS)

     End Do

     ! CONCLUYE COMDENTARIO LYR.

     IF ( MOD( ISTEP, IPRINT ) .EQ. 0 ) THEN

        WRITE(*,'(1X,I8,6(2X,F10.4))') ISTEP, EN, XKN, VN, PRES, TEMP

     End If

     IF(ISTEP.EQ.NSTEP)THEN

        DO JFIN=1,N

           WRITE(15,*)RX(JFIN), RY(JFIN), RZ(JFIN)

        End DO

        DO JFIN=1,N

           WRITE(12,*)VX(JFIN), VY(JFIN), VZ(JFIN)

        End DO

     ENDIF

     WRITE(14,*)ISTEP,EN,XKN,VN,PRES,TEMP

     xmod=mod(ISTEP,nfrec)
     if(xmod.eq.0.0 .and.ISTEP.GT.NENER)then
        if(ISTEP.LE.NSTEP)then
           ki2=ki2+1

           ACE = ACE + EN
           ACK = ACK + XKN
           ACV = ACV + VN
           ACP = ACP + PRES

           ACESQ = ACESQ + EN ** 2
           ACKSQ = ACKSQ + XKN ** 2
           ACVSQ = ACVSQ + VN ** 2
           ACPSQ = ACPSQ + PRES ** 2

           do i=1,n
              CX(I,KI2)=RX(I)
              CY(I,KI2)=RY(I)
              CZ(I,KI2)=RZ(I)

              CXR(I,KI2)=RXC(I)
              CYR(I,KI2)=RYC(I)
              CZR(I,KI2)=RZC(I)
           End do
        End If
     End IF
  End DO

  XNORM = REAL ( KI2 )

  AVE = ACE / XNORM
  AVK = ACK / XNORM
  AVV = ACV / XNORM
  AVP = ACP / XNORM
  ACESQ = ( ACESQ / XNORM ) - AVE ** 2
  ACKSQ = ( ACKSQ / XNORM ) - AVK ** 2
  ACVSQ = ( ACVSQ / XNORM ) - AVV ** 2
  ACPSQ = ( ACPSQ / XNORM ) - AVP ** 2

  IF ( ACESQ .GT. 0.0 ) FLE = SQRT ( ACESQ )
  IF ( ACKSQ .GT. 0.0 ) FLK = SQRT ( ACKSQ )
  IF ( ACVSQ .GT. 0.0 ) FLV = SQRT ( ACVSQ )
  IF ( ACPSQ .GT. 0.0 ) FLP = SQRT ( ACPSQ )
  AVT = AVK * 2.0 / FREE
  FLT = FLK * 2.0 / FREE

  WRITE(*,'('' AVE = '',F10.4)') AVE
  WRITE(*,'('' FLE = '',F10.4)') FLE
  WRITE(*,'('' AVV = '',F10.4)') AVV
  WRITE(*,'('' FLV = '',F10.4)') FLV
  WRITE(*,'('' AVP = '',F10.4)') AVP
  WRITE(*,'('' FLP = '',F10.4)') FLP
  WRITE(*,'('' AVT = '',F10.4)') AVT
  WRITE(*,'('' FLT = '',F10.4)') FLT

  CALL GDR(CX,CY,CZ,KI2)
  CALL WDT(CXR,CYR,CZR,KI2,DT,NFREC)

END program DMLJ
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
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
  
  !RETURN
END SUBROUTINE FORCE
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
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
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
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
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
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

  DO 300 I = 1, N
     VX(I) = VX(I) - SUMX
     VY(I) = VY(I) - SUMY
     VZ(I) = VZ(I) - SUMZ
  End DO

END SUBROUTINE COMVEL
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
SUBROUTINE CONFIGINI (BOX,RX,RY,RZ)
  IMPLICIT REAL*8 (a-h,o-z)
  PARAMETER ( N = 250 )
  COMMON /BLOCK2/ RXC, RYC, RZC

  DIMENSION RX(N), RY(N), RZ(N), RXC(N), RYC(N), RZC(N)
  DIMENSION X(N), Y(N), Z(N)

  OPEN(11,FILE='cidm0.dat',STATUS='UNKNOWN')

  NP=N
  A=1.0/3.0
  ISEED1=456808
  ISEED2=780
  ISEED3=7598

  DO I=1,NP
2    R=zran(iseed1)-0.5d0
     S=zran(iseed2)-0.5D0
     T=zran(iseed3)-0.5D0
     
     X(I)=R*BOX
     Y(I)=S*BOX
     Z(I)=T*BOX

     DO J=1 , I-1

        xij=X(I)-X(J)
        yij=Y(I)-Y(J)
        zij=Z(I)-Z(J)

        RO=(xij)**2+(yij)**2+(zij)**2

        IF (RO.LE.1.0) THEN

           WRITE(*,*)'traslape',I,J
           GO TO 2

        ENDIF

     End DO

     RX(I)=X(I)
     RY(I)=Y(I)
     RZ(I)=Z(I)
     RXC(I)=X(I)
     RYC(I)=Y(I)
     RZC(I)=Z(I)
     WRITE(11,*)SNGL(RX(I)),SNGL(RY(I)),SNGL(RZ(I))

  End DO

END program
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
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

  DO 20 L=1,NP
     
     DO 25 M=1,NP
        
        IF (M.EQ.L) GOTO 25

        DO 40 J=1,NTMAX
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
           ENDIF

        End Do
     End Do
  End DO

  C1=(4.0/3.0)*(PI*DENS)

  OPEN(60,FILE='grdm0.dat',STATUS='UNKNOWN')

  DO 30 NBIN=1,MAXBIN
     
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

!===============================================================================================================
!===============================================================================================================
!===============================================================================================================

SUBROUTINE WDT(CX,CY,CZ,KI,DT,NFREC)
  PARAMETER (N=250)
  PARAMETER (NN2=5000)
  IMPLICIT REAL*8 (A-H,O-Z)
  DIMENSION CX(N,NN2),CY(N,NN2),CZ(N,NN2)
  COMMON /VALORES/ DENS,RCUT,BOX,NSTEP

  TIM=REAL(NFREC)*DT

  open(50,file='wtdm0.dat',STATUS='UNKNOWN')

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
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================

! GENERADOR DE NUMEROS ALEATORIOS CON DISTRIBUCION UNIFORME
! computers in physics
! vol. 8, No. 1 (1994) pag.117

FUNCTION ZRAN(ISEED)
  implicit real*8 (a-h,o-z)
  common/semillas/iseed3,iseed2,iseed1
  mzran=iseed3-iseed1

  if(mzran.lt.0) mzran=mzran+2147483579
  iseed3=iseed2
  iseed2=iseed1
  iseed1=mzran
  iseed=ishft(3533*ishft(iseed,-16)+iand(iseed,125535),16)+3533*iand(iseed,65535)
  mzran=mzran+iseed
  zran=.5+.2328306d-9*mzran

  RETURN
END FUNCTION ZRAN
!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
! GENERADOR DE NUMEROS ALEATORIOS CON DISTRIBUCION GAUSSIANA
SUBROUTINE AZARG( ISEED,X )
  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
  external zran
  common/semillas/iseed3,iseed2,iseed1

  pi=4.0*atan(1.0)
  R=zran(iseed)
  S=zran(iseed)
  X=SQRT(-2.0*LOG(R))*COS(2.0*PI*S)

  RETURN
END SUBROUTINE AZARG
