!============================================================================
! EL PROGRAMA REALIZA EL CALCULO DE LA GDR APARTIR DE LAS DIFERENTES 
! CONFIGURACIONES REALIZADAS EN EL PROGRAMA PRINCIPAL (3D)
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!============================================================================

Subroutine GdrCalc

  Use cte
  Implicit None

  Integer, Allocatable, Dimension(:) :: Histo
  Real, Parameter :: delTar = 0.05
  Integer :: MBin, iBin, NNN
  Integer :: i, j, k                                              !CONTADORES
  Real :: xO, yO, zO, xN, yN, zN, xON, yON, zON
  Real :: rD, rU, rL, rM, c1, c2, gdr, gdrm, press, b
  Integer :: istat1
  Character (len=80) :: err_msg1
  Character (:), Allocatable :: Filename, chardens
  Logical :: Ctrl1, Ctrl2
  
  MBin = Int( RCut / delTar )
  NNN = Mbin + 1
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
              zO = CZ( i , k )
              
              !PARTICUAL j CERCANA
              xN = CX( j , k )
              yN = CY( j , k )
              zN = CZ( j , k )
              
              !DISTANCIA
              xON = xN - xO
              yON = yN - yO
              zON = zN - zO
              
              !CONDICION DE IMAGEN MINIMA
              xON = xON - BoxL*Anint( xON/BoxL )
              yON = yON - BoxL*Anint( yON/BoxL ) 
              zON = zON - BoxL*Anint( zON/BoxL )

              !DISTANCIA ENTRE LAS PARTICULAS
              rD = sqrt( (xON * xON) + (yON * yON) + (zON*zON) )
              
              !CERCANIA CINTA
              iBin =  Int( rD / delTar ) + 1
              
              Guardar : If((iBin .LE. MBin)  ) Then

                 Histo(iBin) = Histo(iBin) + 1
                 
              End If Guardar
              
           End Do StepCnfg
           
        End If NOTSAME
        
     End Do NextParti
     
  End Do PartiO

 
  
  c1 = ( 4.0 / 3.0 ) * PI * Dens
  Write(chardens,*) Dens
  Filename = "gdr"//chardens//".dat"
  
  !ABRIENDO ARCHIVO PARA GDR
  Open( 5, file= filename )
  
  GdrCal: Do ibin = 1 , MBin
     
     rL = Real(iBin - 1) * delTar

     rU = rL + delTar
     rM = rL + ( delTar/2.0 )

     c2 = c1 * ( ( rU**3 ) - ( rL**3 ) )
     gdrm = gdr
     gdr = Real( Histo(iBin) )/ Real(NN) / Real(N) / c2
     
     Write(5,*) rM , gdr


     Ctrl1 = gdrm == 0                                        !CALCULO DE GDR ESFERA DURA
     Ctrl2 = gdr /= 0
     PressCalc:If ( Ctrl1 .AND. Ctrl2 ) Then

        b = 2.0*pi/3.0*gdr                                    !PARAMETRO DE VAN DER WAALS
        Press = dens*(1.0 + dens*b)                            !PRESION DISCO DURO
        Write(27,*)  dens, press
        Write(*,*)  dens, press
        
     End If PressCalc
     
  End Do GdrCal

  Close(5)
  
  Deallocate( Histo )

  Write(*,*) "GDR DONE, SAVE"
  
End Subroutine GdrCalc
