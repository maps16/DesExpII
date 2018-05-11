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
  Integer :: DENS
  Integer :: State
  Integer :: k, i                                 ! CONTADOR
  Character (len=3), Parameter :: Start = "gdr"
  Character (len=4), Parameter :: En = ".dat"
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real, Parameter :: TP = 1.0                     ! TEMPERATURA REDUCIDA
  Real, Parameter :: Lambda = 1.25                ! FIN POZO
  Real, Dimension(:),Allocatable ::  R , G        ! RADIO | DISTRIBUCION RADIAL
  Real :: cte, delta                         

  Write(*,*) " ESCRIBE LA DENSIDAD *10 (DOS DIGITOS EJ: 01) !VALOR ENTERO  "
  Read(*,*) DENS

  !TAMANO DEL ARCHIVO POR LEER
  Write(Cons,256) Dens
  Filename = start//trim(Cons)//En
  Write(*,*) "Archivo: ",Filename
  
  Open( 1, File = Trim(Filename), action= "read" )

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

     Read( 1,*, iostat = state  ) R , G
     If ( state .LT. 0 ) Exit

  End Do Saves

  Write(*,*) "DATOS GUARDADOS EN MEMORIA"
  
  !CALC DE a VAN DER WAALS
  Cte = -(2.0*Pi) / TP
  Delta = 0.05

  Locate: Do i = 1, k
     
     If (R(i) .GT. 1.0) Exit
     
  End Do Locate
  Write(*,*) i, R(i), G(i)

  
512 Format (I5.5)
256 Format (I2.2)
End Program Waals
