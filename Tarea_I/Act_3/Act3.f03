!================================================================================
! ActIII es un codigo para colocar particulas sobre una malla cuadrada separadas 
! uniformemente. 
! Autor: Martin Alejandro Paredes Sosa
!================================================================================


MODULE Glob                     !Declaración de Constantes/Variables Globales
  Integer, parameter :: N       !Numero de particulas
  Real *8, parameter :: l       !Longitud/Lado de Celda
END MODULE Glob


subroutine CalcPosXY(N, l, sep, xPos, yPos)

  Use Glob                                       !Llamar a las Variables Globales
  Implicit None
  Integer :: i                                   !Contador
  Real *8, Dimension (1:N) :: xPos, yPos         !Posiciones en X y Y

  i=1 !Inicio Contador de la particula
  do while (i<=N)                                !Terminar al recorer cada particula
     pos=((-1)**i)*((int((i-1)/2)*sep)+ (sep/2) )!Calculo de Posicion
     write(1,*)i, pos                            !Escribir Valor En Archivo 1 (Out.dat)
     i = i+1                                     !Avance contador

  end do  
    
End Subroutine CalPos


Program ActIII
  Use Glob
  Implicit None
  Real *8, Dimension (1:N) :: 

End Program ActIII
