!================================================================================
! ActIII es un codigo para colocar particulas sobre una malla cuadrada separadas 
! uniformemente. 
! Autor: Martin Alejandro Paredes Sosa
!================================================================================

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
                                                 !Limitado a 500 Particulas
                                                 !Por Lado
  
  !Entrada de Datos
  Write(*,*) "Numero de Particulas Por Lado de la Malla"
  Read(*,*) N
  Write(*,*) "Longitud del Lado de la Malla"
  Read(*,*) l

  !Calculo de la Separacion Entre Particulas
  sep = l/N

  Do i=1, N
     Call CalcPos(i, sep, Pos)         !Llama Subrutina Para Posibles Posiciones
     xPos(i) = Pos                     !Guardando Valores para posiciones X
     yPos(i) = Pos                     !Guardando Valores para posiciones Y
  End Do
  
  Open(1,File="SalidaGrafico.dat")     !Abriendo Archivo de Salida
  Do k=1, N
     Do m=1, N
        Write(1,*) xPos(k), yPos(m)    !Escribiendo Salida
     End do
  End Do
  
  Close(1)
  
End Program ActIII
