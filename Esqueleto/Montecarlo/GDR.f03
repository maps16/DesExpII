!============================================================================
! EL PROGRAMA REALIZA EL CALCULO DE LA GDR APARTIR DE LAS DIFERENTES 
! CONFIGURACIONES REALIZADAS EN EL PROGRAMA PRINCIPAL
!
! AUTOR: Martin Alejandro Paredes Sosa
!============================================================================

Subroutine GdrCalc

  Use cte
  Implicit None

  Integer, Allocatable, Dimension(:) :: Histo
  Real, Parameter :: delTar = 0.05
  Integer :: MBin, iBin
  Integer :: i, j, k                                              !CONTADORES
  Real :: xO, yO, xN, yN, xON, yON
  Real :: rD, rU, rL, rM, c1, c2, gdr, gdrm, press
  Integer :: istat1
  Character (len=80) :: err_msg1
  logical :: Ctrl1, Ctrl2
  

  Allocate( Histo(NNN) , STAT = istat1, ERRMSG = err_msg1)

  Histo = 0
    
  MBin = Int( RCut / delTar )
  
  PartiO : Do i = 1, N
     
     NextParti : Do j = 1, N
        
        NOTSAME : If (i /= j ) Then
           
           StepCnfg : Do k = 1, NN
              
              !PARTICULA i ORIGEN
              xO = CX( i , k )
              yO = CY( i , k )
              zO = CZ (i , k )
              !PARTICUAL j CERCANA
              xN = CX( j , k )
              yN = CY( j , k )
              zN = CZ) j , k )
              
              !DISTANCIA
              xON = xN - xO
              yON = yN - yO
              zON = zN - zO
              
              !CONDICION DE IMAGEN MINIMA
              xON = xON - BoxL*Anint( xON/BoxL )
              yON = yON - BoxL*Anint( yON/BoxL )
              zON = zON - BoxL*Anint( zON/BoxL )

              !DISTANCIA
              rD = sqrt( (xON * xON) + (yON * yON) +  (zON * zON) )
                            
              !CERCANIA CINTA
              iBin =  Int( rD / delTar ) + 1
              
              Guardar : If((iBin .LE. MBin)  ) Then
              
                 Histo(iBin) = Histo(iBin) + 1
              
              End If Guardar
              
           End Do StepCnfg
           
        End If NOTSAME
     End Do NextParti
  End Do PartiO

  !Write(*,*) Histo
  
  c1 = (4.0/3.0)* PI * Dens
  
  !ABRIENDO ARCHIVO PARA GDR
  Open( 5, file= "gdr.dat" )
  
  GdrCal: Do ibin = 1 , MBin
     
     rL = Real(iBin - 1) * delTar

     rU = rL + delTar
     rM = rL + ( delTar/2.0 )

     c2 = c1 * ((rU*rU) - (rL*rL))
     gdr = Real( Histo(iBin) )/ Real(NN) / Real(N) / c2
     Write(5,*) rM , gdr


     !Ctrl1 = gdrm == 0
     !Ctrl2 = gdr /= 0
     !PressCalc:If ( Ctrl1 .AND. Ctrl2 ) Then

        !Press = 1.0 + 0.5*PI*dens*gdr
        
     !End If PressCalc
     
  End Do GdrCal

  !Write(*,*) "Presion:",Press, "Concentracion:", dens
  !Write(120,*) Dens, Press
  Close(5)
  
  
  Deallocate( Histo )

  Write(*,*) "GDR DONE, SAVE"
  
End Subroutine GdrCalc
