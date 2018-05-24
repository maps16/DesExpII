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
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real, Parameter :: TP = 1.0                     ! TEMPERATURA REDUCIDA
  Real, Parameter :: Lambda = 1.25                ! FIN POZO
  Real, Dimension(:),Allocatable ::  R , G        ! RADIO | DISTRIBUCION RADIAL
  Real :: a, b, a74                               ! PARAMETROS VAN DER WAALS
  Real :: ctea, ctea74, cteb, delta , gr1         ! PARAMETROS PARA CALCULO DE a Y b VAN DER WAALS
  Real :: Intg                                    ! ACUMULADOR PARA INTEGRACION

  Write(*,*) "========================"
  Open(8, File = "a_starT1.dat", Action= "write") !ARCHIVO DE SALIDA T*=1.0
  Open(9, File = "a_starT74.dat", Action= "write") !ARCHIVO DE SALIDA T*=0.74
  Archivo:  Do Dens = 1, 10
     !TAMANO DEL ARCHIVO POR LEER
     Write(Cons,256) Dens
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
     b = cteb*gr1                  ! b DE VAN DER WAALS
     
     ! ALGORITMO DE INTEGRACION POR TRAPECIO
     Intg = R(i)*R(i)*G(i)*0.5
     IntegrandoA: Do j = i+1 ,k

        If ( R(j+1) .GT. lambda) Exit
        Intg = Intg + R(j)*R(j) * G(j)

     End Do IntegrandoA
          
     Intg = Intg + 0.5*R(j)*R(j)*G(j)

     a = Intg*delta*ctea         ! a DE VAN DER WAALS
     a74 = Intg*delta*ctea74
     Write(*,*) "a*= ", a
      Write(*,*) "a74*= ", a74
     Write(*,*) "b*= ", b
     Write(8,*) Dens*0.1, a  , b
     Write(9,*) Dens*0.1, a74, b
     
     Deallocate(R,G)
     Write(*,*) "========================"
  End Do Archivo
  
512 Format (I5.5)
256 Format (I2.2)
End Program Waals
