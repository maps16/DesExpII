!==============================================================================
! CALCULO DE LOS COEFICIENTES DE VAN DER WAALS A PARTIR DE LOS ARCHIVOS DE
! LOS ARCHIVOS DE LA G(r) OBTENIDOS DE LA SIMULACION CON POTENCIAL SW 
!
! AUTOR : MARTIN ALEJANDRO PAREDES SOSA
!==============================================================================

!DECLARACION DE VARIABLES
Program Waals
  !Use Basic
  Implicit None
  Integer :: DENS                                 ! PARA NOMBRE DE ARCHIVO
  Integer :: State                                ! ESTADO DE LECTURA
  Integer :: k, i, j                              ! CONTADOR
  Character (len=3), Parameter :: Start = "gdr"   ! NOMBRE DE ARCHIVO DE ENTRADA
  Character (len=4), Parameter :: En = ".dat"     ! EXTENSION ARCHIVO DE ENTRADA
  Character (len=12):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real :: Des                                     ! CONCENTRACION
  Real, Dimension(:),Allocatable ::  R , G        ! RADIO | DISTRIBUCION RADIAL
  Real :: a, b, a74                               ! PARAMETROS VAN DER WAALS
  Real :: ctea, ctea74, cteb, delta , gr1         ! PARAMETROS PARA CALCULO DE a Y b VAN DER WAALS
  Real :: Intg                                    ! ACUMULADOR PARA INTEGRACION

  Write(*,*) "========================"
  !Open(8, File = "a_starT1.dat", Action= "write") 
  Open(9, File = "Pres.dat", Action= "write") !ARCHIVO DE SALIDA 

  Archivo:  Do Dens = 1, 9
     !TAMANO DEL ARCHIVO POR LEER
     Write(Cons,256) Dens*10
     Filename = start//trim(Cons)//En
     Write(*,*) "Archivo: ",Filename

     Open( 1, File = Trim(Filename), action= "read", Status ="old" ) !ARCHIVO DE ENTRADA
     k = 0 !REINICIA CONTADOR
     Sizes: Do                         !BUSCANDO TAMAÑO DE ARCHIVO (RENGLONES QUE QUE TIENE)
        
        Read( 1,*, iostat = state  )
        k = k + 1
        If ( state .LT. 0 ) Exit
        
     End Do Sizes
     
     Write(*,*) "Tiene", k, "Renglones" !DEBUG LINE (SIZE OF FILE)

     Rewind 1 !REINICIAR ARCHIVO DE ENTRADA
     Allocate ( R(k), G(k) )

     !SAVING FILE DATA
     Saves : Do i = 1, k+1

        Read( 1,*, iostat = state  ) R(i), G(i)
        If ( state .LT. 0 ) Exit

     End Do Saves

     Write(*,*) "DATOS GUARDADOS EN MEMORIA"

     !CALC DE a VAN DER WAALS
     Ctea = (2.0*Pi) / TP
     Ctea74 = (2.0*Pi)/ 0.74
     Cteb = (2.0/3.0)*PI
     Delta = 0.05                  ! CAMBIAR SEGUN EL ARCHIVO

     Locate: Do i = 1, k           ! BUSCANDO EL PRIMER DATO .GE. 1.0

        If (R(i) .GE. 1.0) Exit

     End Do Locate

     gr1 = G(i)                    ! Ghd(1+)
     Des = Real(Dens)*0.01
     Press = 1.0 + 0.5*Pi*Des*gr1

     Write(9,*) Des * pres
     
     Deallocate(R,G)
     Write(*,*) "========================"
  End Do Archivo
  
512 Format (I5.5)
256 Format (I2.2)
End Program Waals
