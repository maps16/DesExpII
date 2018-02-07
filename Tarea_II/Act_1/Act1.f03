!======================================================================================
! Realiza un calculo aproximado del valor de pi mediante la generacion aleatoria
!         de puntos que se localizan dentro de un circulo unitario
! Autor:  Martin Alejandro Paredes Sosa
!
!======================================================================================
Module Cte
  Implicit None
  Integer :: m , n                                   !Acertados : Intentos
  Real, Parameter :: Pi = 3.1415
  Integer,Parameter :: Sav = 5 !DEBUG
  Integer, Parameter :: Seed1 = 3321, seed2 = 1815   !Semillas Posibles
End Module Cte

!==========================================
Subroutine InOutCircle(x, y ,nNew)  !CHECAR SE ESTA DENTRO DEL CIRCULO UNITARIO
  Use Cte
  Implicit None
  Real, Dimension(n) :: x , y
  Logical :: logic
  Integer :: i, nNew !indice

  RunPos: Do i = 1, nNew

     logic = x(i)*x(i) + y(i)*y(i) < 1  !CONDICION A CUMPILR
     ContIn:If (logic) Then             !CUMPLIR CONDICION
        m = m+1                         !AVANZAR CONTADOR DE EXITO
     End If ContIn
     
  End Do RunPos
  
End Subroutine InOutCircle

!=========================================
Subroutine RPosc(x,y)               !GENERAR POSICIONES ALEATORIAS EN "X" Y "Y"

  Use Cte
  Implicit None
  Integer :: i
  Real, Dimension(n) :: x , y

  Call Random_Number(x)             !NUM ALEATORIO X
  Call Random_Number(y)             !NUM ALEATORIO Y

  !Debug Probando Generador de las posiciones
  !Writedo : Do i=1,n
  !   Write(*,*) x(i),y(i)
  !End Do Writedo
  
End Subroutine RPosc

!==================================================

Program PiCalculation

  Use Cte
  Implicit None
  Integer :: l                               !INDICE DE CONTADOR
  Integer, Allocatable, Dimension(:) :: mm
  Real, Allocatable, Dimension (:) :: x , y, PiCalc, DelPi, ErrRel

  Write(*,*) "Cuantos Intentos"     !INGRESANDO N
  Read(*,*) N

  Allocate( x(n),y(n) )             !ALOJANDO ESPACIO EN MEMORIA

  Call RPosc(x,y)                   !LLAMANDO SUBRUITNA PARA OBTENER POSICIONES
  Allocate( mm(n) )                 !ALOJANDO ESPACIO EN MEMORIA
  
  Check: Do l = 1, n                !CONSIDERACION DE DIFERENTES N (L, EVOLUCION DE N)
     
     Call InOutCircle(x,y,l)        !LLAMANDO SUBRUTINA PARA CONDICION DEL CIRCULO
     mm(l) = m                      !GUARDANDO VALOR M PARA DIFERENTES N (ESTE CASO L)
     m=0                            !REINICIANDO LA M
  End Do Check

  Deallocate (x,y)                 !DESALOJAR ESPCIO EN MEMORIA (SE BORRO LO GUARDADO)

  Allocate( PiCalc(n) )
  PiC: Do l=1, n                    !CALCULO DE PI EN SU EVOLUCION DE N
     PiCalc(l) = ( 4*real( mm(l) ) )/real(l) !CALCULANDO PI
  End Do PiC

  !CALCULANDO DELTA PI DESVIACION, ERROR RELATIVA
  ALLOCATE( DelPi(n), ErrRel(n) )
  DelPi = Pi - PiCalc
  ErrRel = DelPi / Pi

  Open(1,File= "Table.dat")
  
  WriteDo: Do l=1 , n
     Write(1,*) l,PiCalc(l), DelPi(l), ErrRel(l)
  End Do WriteDo

  DEALLOCATE(mm,PiCalc,DelPi,ErrRel) !DESALOJAR ESPACIO EN MEMORIA

End Program PiCalculation


