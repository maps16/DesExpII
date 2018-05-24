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
  Write(*,*) "GDR DONE"
  CALL WDT(CXR,CYR,CZR,KI2,DT,NFREC)
  Write(*,*) "WDT DONE"
END program DMLJ
