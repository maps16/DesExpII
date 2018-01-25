!================================================================================
! ActIII es un codigo para colocar particulas sobre una malla cuadrada separadas 
! uniformemente. 
! Autor: Martin Alejandro Paredes Sosa
!================================================================================


!MODULE Glob                     !Declaración de Constantes/Variables Globales
!  Integer, parameter :: N       !Numero de particulas
!  Real *8, parameter :: l       !Longitud/Lado de Celda
!END MODULE Glob


subroutine CalcPos(i, sep, Pos)

  Implicit None
  Integer :: i                                   !Contador
  Real *8 :: sep                                 !Separacion de las particulas
  Real *8 :: Pos                                 !Posiciones

  Pos = ((-1)**i)*((int((i-1)/2)*sep)+ (sep/2) ) !Calculo de Posicion
     
End Subroutine CalcPos


Program ActIII

  Implicit None
  Real *8 :: sep, l                              !Separacion, Longitud Malla
  Integer :: i, k, m                             !Contadores
  Integer :: N                                   !#Particulas 
  Real *8 :: Pos                                 !Posiciones Posibles Temp
  Real *8, Dimension (1:500) :: xPos, yPos       !Posiciones Posibles para X y Y

  !Entrada de Datos
  Write(*,*) "Numero de Particulas Por Lado de la Malla"
  Read(*,*) N
  Write(*,*) "Longitud del Lado de la Malla"
  Read(*,*) l

  !Calculo de la Separacion Entre Particulas
  sep = l/N

  Do i=1, N
     Call CalcPos(i, sep, Pos)                   !Llama
     xPos(i) = Pos
     yPos(i) = Pos
     !i = i+1
  End Do
  !Write(*,*) xPos(i-2), xPos(i-1) !DEBUG
  Open(1,File="SalidaGrafico.dat")
  Do k=1, N
     Do m=1, N
        Write(1,*) xPos(k), yPos(m)
     End do
  End Do
  
  Close(1)
  
End Program ActIII
