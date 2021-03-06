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
  Character (len=12):: Filename, cons                            ! NOMBRE DE ARCHIVO

  MBin = Int( RCut / delTar ) ! CINTA MAXIMA

  Allocate( Histo(MBin+1) , STAT = istat1, ERRMSG = err_msg1)
  Histo = 0 ! ESTABLECER TODO EL ARREGLO EN 0
  
  PartiO : Do i = 1, N
     NextParti : Do j = 1, N
        NOTSAME : If (i /= j ) Then
           StepCnfg : Do k = 1, NN
              
              !PARTICULA i ORIGEN
              xO = CX( i , k )
              yO = CY( i , k )
              
              !PARTICUAL j CERCANA
              xN = CX( j , k )
              yN = CY( j , k )
              
              !DISTANCIA
              xON = xN - xO
              yON = yN - yO
              
              !CONDICION DE IMAGEN MINIMA
              xON = xON - BoxL*Anint( xON/BoxL )
              yON = yON - BoxL*Anint( yON/BoxL ) 
              rD = sqrt( (xON * xON) + (yON * yON) )
              If (rd .LE. 1.0 ) write(100,*) rD, i, j , k
              
              !CERCANIA CINTA
              iBin =  Int( rD / delTar ) + 1

              Guardar : If((iBin .LE. MBin)  ) Then
 
                 Histo(iBin) = Histo(iBin) + 1 !ACUMULANDO  PARTICULAS EN CINTAS
 
              End If Guardar
              
           End Do StepCnfg
           
        End If NOTSAME
     End Do NextParti
  End Do PartiO

  
  c1 = PI * Dens
  
  !ABRIENDO ARCHIVO PARA GDR
  Write(Cons,256) Int(100.0 * Dens)
     Filename = "gdr"//trim(Cons)//".dat"
  Open( 5, file= Filename )
  
  GdrCal: Do ibin = 1 , MBin
     
     rL = Real(iBin - 1) * delTar       !CINTA INFERIOR

     rU = rL + delTar                   !CINTA SUPERIOR
     rM = rL + ( delTar/2.0 )           !CINTA INTERMEDIA

     c2 = c1 * ((rU*rU) - (rL*rL))      !PRECALCULO G(r)
     gdr = Real( Histo(iBin) )/ Real(NN) / Real(N) / c2 !CALCULANDO G(r)
     Write(5,*) rM , gdr
     
  End Do GdrCal

  Close(5)
    
  Deallocate( Histo )

  Write(*,*) "GDR DONE, SAVE"

256 Format (I2.2)

End Subroutine GdrCalc
