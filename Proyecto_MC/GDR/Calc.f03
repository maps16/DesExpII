!==============================================================================
! CALCULO DE LOS COEFICIENTES DE VAN DER WAALS A PARTIR DE LOS ARCHIVOS DE
! LOS ARCHIVOS DE LA G(r) OBTENIDOS DE LA SIMULACION CON POTENCIAL SW 
!
! AUTOR : MARTIN ALEJANDRO PAREDES SOSA
!==============================================================================

!DECLARACION DE VARIABLES
Program Waals
  Use Basic
  Implicit None
  Integer :: DENS                                 ! PARA NOMBRE DE ARCHIVO
  Integer :: State                                ! ESTADO DE LECTURA
  Integer :: k, i, j                              ! CONTADOR
  Character (len=3), Parameter :: Start = "gdr"
  Character (len=4), Parameter :: En = ".dat"
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real, Parameter :: TP = 1.0                     ! TEMPERATURA REDUCIDA
  Real, Parameter :: Lambda = 1.25                ! FIN POZO
  Real, Dimension(:),Allocatable ::  R , G        ! RADIO | DISTRIBUCION RADIAL
  Real :: a, b                                    ! PARAMETROS VAN DER WAALS
  Real :: ctea , cteb, delta , gr1                ! PARAMETROS PARA CALCULO DE a Y b VAN DER WAALS
  Real :: Intg                                    ! ACUMULADOR PARA INTEGRACION

  Write(*,*) " ESCRIBE LA DENSIDAD *10 (DOS DIGITOS EJ: 01) !VALOR ENTERO  "
  Read(*,*) DENS

  !TAMANO DEL ARCHIVO POR LEER
  Write(Cons,256) Dens
  Filename = start//trim(Cons)//En
  Write(*,*) "Archivo: ",Filename
  
  Open( 1, File = Trim(Filename), action= "read", Status ="old" )

  Sizes: Do
     Read( 1,*, iostat = state  )
     k = k + 1
     If ( state .LT. 0 ) Exit
  End Do Sizes
  Write(*,*) "Tiene", k, "Renglones" !DEBUG LINE (SIZE OF FILE)

  Rewind 1
  Allocate ( R(k), G(k) )

  !SAVING FILE DATA
  Saves : Do i = 1, k+1

     Read( 1,*, iostat = state  ) R(i), G(i)
     If ( state .LT. 0 ) Exit

  End Do Saves

  Write(*,*) "DATOS GUARDADOS EN MEMORIA"
  
  !CALC DE a VAN DER WAALS
  Ctea = -(2.0*Pi) / TP
  Cteb = (2.0/3.0)*PI
  Delta = 0.05

  Locate: Do i = 1, k
     
     If (R(i) .GE. 1.0) Exit
          
  End Do Locate

  gr1 = G(i)
  b = cteb*gr1
  !  Write(*,*) i, R(i), G(i)     !DEBUG LINE

  ! ALGORITMO DE INTEGRACION POR TRAPECIO
  Intg = R(i)*R(i)*G(i)*0.5
  IntegrandoA: Do j = i+1 ,k

     If ( R(j+1) .GT. lambda) Exit
     Intg = Intg + R(j)*R(j) * G(j)
     
  End Do IntegrandoA
  !  Write(*,*) j, R(j) !DEBU LINE
  Intg = Intg + 0.5*R(j)*R(j)*G(j)
  a = Intg*delta*ctea
  Write(*,*) "a*= ", a
  Write(*,*) "b*= ", b
  
512 Format (I5.5)
256 Format (I2.2)
End Program Waals
