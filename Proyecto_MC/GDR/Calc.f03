!==============================================================================
! CALCULO DE LOS COEFICIENTES DE VAN DER WAALS A PARTIR DE LOS ARCHIVOS DE
! LOS ARCHIVOS DE LA G(r) OBTENIDOS DE LA SIMULACION
!
! AUTOR : MARTIN ALEJANDRO PAREDES SOSA
!==============================================================================

!DECLARACION DE VARIABLES
Program Waals
  Use Basic
  Implicit None
  Integer :: DENS
  Integer :: State
  Integer :: k                                    ! CONTADOR
  Character (len=3), Parameter :: Start = "gdr"
  Character (len=4), Parameter :: En = ".dat"
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real, Parameter :: TP = 1.0                     ! TEMPERATURA REDUCIDA
  Real, Parameter :: Lambda = 1.25

  Write(*,*) " ESCRIBE LA DENSIDAD *10 (DOS DIGITOS EJ: 01) !VALOR ENTERO  "
  Read(*,*) DENS

  !TAMANO DEL ARCHIVO POR LEER
  Write(Cons,256) Dens
  Filename = start//trim(Cons)//En
  Write(*,*) Filename
  Open( 1, File = Trim(Filename) )

  Sizes: Do
     Read( 1,*, iostat = state  )
     k = k + 1
     If ( state .LT. 0 ) Exit
  End Do Sizes
  Write(*,*) k

256 Format (I2.2)
End Program Waals
