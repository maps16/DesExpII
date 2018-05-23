!==============================================================================
! CALCULO DE LA PRESION PARA EL CASO DE DE DISCOS DUROS CON LOS ARCHIVOS DE 
! DE LA G(r) OBTENIDOS DE LA SIMULACION
!
! AUTOR : MARTIN ALEJANDRO PAREDES SOSA
!==============================================================================

!DECLARACION DE VARIABLES
Program Calc
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
  Real :: gr1                                     ! PARAMETROS PARA CALCULO DE a Y b VAN DER WAALS
  Real :: Press                                   ! ACUMULADOR PARA INTEGRACION

  Write(*,*) "========================"
  !Open(8, File = "a_starT1.dat", Action= "write") 
  Open(9, File = "Pres.dat", Action= "write") !ARCHIVO DE SALIDA 

  Archivo:  Do Dens = 1, 9

     !TAMANO DEL ARCHIVO POR LEER
     Write(Cons,256) Dens*10             ! SELECCION DEL ARCHIVO PARA LEER 
     Filename = start//trim(Cons)//En    ! VARIAR SEGUN  NOMBRO EL ARCHIVO; Start Y En TAMBIEN CAMBIAR
     Write(*,*) "Archivo: ",Filename     ! CHECAR QUE ARCHIVO VA A LEER (SI NO ES EL MISMO NOMBRE FALLA LA CORRIDA)

     Open( 1, File = Trim(Filename), action= "read", Status ="old" ) !ARCHIVO DE ENTRADA

     k = 0  !REINICIA CONTADOR PARA NUEVO ARCHIVO (RECUENTO DE FILAS)
     
     Sizes: Do                         !BUSCANDO TAMAÑO DE ARCHIVO (RENGLONES QUE QUE TIENE)
        
        Read( 1,*, iostat = state  )
        k = k + 1
        If ( state .LT. 0 ) Exit       !CONDICION DE SALIDA (YA NO HAY MAS REGLONES EN EL ARCHIVO)
        
     End Do Sizes
     
     Write(*,*) "Tiene", k, "Renglones" !DEBUG LINE (SIZE OF FILE)

     Rewind 1 !REINICIAR ARCHIVO DE ENTRADA
     Allocate ( R(k), G(k) ) !ALOJAR ESPACIO EN MEMORIA

     !SAVING FILE DATA
     Saves : Do i = 1, k+1

        Read( 1,*, iostat = state  ) R(i), G(i)   ! CONSIDERAR EL NUMERO DE COLUMNAS
        If ( state .LT. 0 ) Exit                  ! CONDICION DE SALIDA (YA NO HAY MAS REGLONES EN EL ARCHIVO) POR SEGURIDAD

     End Do Saves

     Write(*,*) "DATOS GUARDADOS EN MEMORIA"

     !CALC DE PRESION HD

     Locate: Do i = 1, k           ! BUSCANDO EL PRIMER DATO .GE. 0

        If (G(i) .GT. 0.0) Exit

     End Do Locate

     gr1 = G(i)                    ! Ghd(1+)
     Des = Real(Dens)*0.1          ! CONCENTRACION A PARTIR DEL NOMBRE DEL ARCHIVO
     Press = 1.0 + 0.5*Pi*Des*gr1  ! CALCULO PRESION HD

     Write(9,*) Des , press        ! ESCRITURA EN ARCHIVO DE SALIDA
     
     Deallocate(R,G)               ! LIBERANDO MEMORIA PARA SIGUIENTE ARCHIVO O FINAL
     Write(*,*) "========================"
  End Do Archivo
  
512 Format (I5.5)
256 Format (I2.2)
End Program Calc
