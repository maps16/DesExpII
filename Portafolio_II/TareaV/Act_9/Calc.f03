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
  Integer,Parameter :: Part = 600                 ! PARA NOMBRE DE ARCHIVO
  Real, Parameter :: Dens = 0.5                ! CONCENTRACION
  Integer :: State                                ! ESTADO DE LECTURA
  Integer :: k, i, j                              ! CONTADOR
  Character (len=3), Parameter :: Start = "gdr"   ! NOMBRE DE ARCHIVO DE ENTRADA
  Character (len=4), Parameter :: En = ".dat"     ! EXTENSION ARCHIVO DE ENTRADA
  Character (len=10):: Filename, cons             ! NOMBRE DE ARCHIVO
  Real, Parameter :: PI = 4.0 * ATAN(1.0)         ! VALOR DE PI
  Real, Parameter :: Rc = 17.3205090              ! RADIO DE CORTE
  Real, Dimension(:),Allocatable ::  R , G        ! RADIO | DISTRIBUCION RADIAL
  Real :: a                                       ! TOTAL
  Real :: cte, delta                              ! PARAMETROS PARA CALCULO CTE
  Real :: Intg                                    ! ACUMULADOR PARA INTEGRACION

  Write(*,*) "========================"
  
  !TAMANO DEL ARCHIVO POR LEER
  Write(Cons,256) Part
  Filename = start//trim(Cons)//En
  Write(*,*) "Archivo: ",Filename

  Open( 1, File = Trim(Filename), action= "read", Status ="old" ) !ARCHIVO DE ENTRADA
  k = 0 !REINICIA CONTADOR
  Sizes: Do                         !BUSCANDO TAMANO DE ARCHIVO (RENGLONES QUE QUE TIENE)

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
  Close(1)
  
  Write(*,*) "DATOS GUARDADOS EN MEMORIA"

  !NUMERO DE PARTICULAS
  Delta = 0.05                  ! CAMBIAR SEGUN EL ARCHIVO
  cte = 8.0*Dens
  
  ! ALGORITMO DE INTEGRACION POR TRAPECIO
  i=1
  Intg = R(i)*G(i)*0.5
  Integrando: Do j = i+1 ,k

     If ( R(j+1) .GT. RC) Exit
     Intg = Intg + R(j) * G(j)

  End Do Integrando

  Intg = Intg + 0.5*R(j)*G(j)

  a = Intg*delta*cte         ! TOTAL DE INTEGRACION
 
  Write(*,*) "a*= ", a
  !Write(8,*) Dens*0.1, a  , b
  !Write(9,*) Dens*0.1, a74, b

  Deallocate(R,G)
  Write(*,*) "========================"


512 Format (I5.5)
256 Format (I3.3)
End Program
