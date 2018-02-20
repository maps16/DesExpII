!================================================================================
! ActIII es un codigo para colocar particulas sobre una malla cuadrada separadas 
! uniformemente. 
! Autor: Martin Alejandro Paredes Sosa
!================================================================================

Module Control
  Logical :: Impar                               !Control Impar Logico(T/F)
  Real *8 :: sep                                 !Separacion de las particulas
End Module Control
  
subroutine CalcPos(i, Pos)

  Use Control
  Implicit None
  Integer :: i                                   !Contador
  Real *8 :: Pos                                 !Posiciones
  If (Impar) Then
     Pos = ((-1)**i)*((int((i-1)/2)*sep)+ (sep) )
  Else
     Pos = ((-1)**i)*((int((i-1)/2)*sep)+ (sep/2) ) !Calculo de Posicion Caso Par
  End If
End Subroutine CalcPos


Program ActIII

  Use Control
  Implicit None
  Real *8 :: l                                   !Longitud Malla
  Integer :: k, m                                !Contadores
  Integer :: N                                   !#Particulas 
  Real *8 :: xPos, yPos                          !Posiciones Posibles para X y Y, Limitado a 500 Particulas Por Lado
  
  !Entrada de Datos
  Write(*,*) "Numero de Particulas Por Lado de la Malla"
  Read(*,*) N
  Write(*,*) "Longitud del Lado de la Malla"
  Read(*,*) l
  
  !Calculo de la Separacion Entre Particulas
  sep = l/N

  !Dectectando N Impar
  Impar = mod(N,2) /= 0
  
  Open(1,File="Output.dat")                      !Abriendo Archivo de Salida
  
  k = 1
  m = 1
  !Calculo y Escritura de las posiciones de la malla cuadrada
  Do While(k <= N)
     If (k==1 .AND. Impar) Then                  !Control Para N Impar
        xPos = 0                                 !Primera Posicion en el centro (x=0)
     Else If (K/=1 .AND. Impar) Then
        call CalcPos(k-1, xPos)                  !Calculando Posicion sobre X (Impar)
     Else
        Call CalcPos(k, xPos)                    !Calculando posicion sobre X
     End If

     Do while(m <= N)

        If(m==1 .AND. Impar) Then                !Control Para N Impar
           yPos = 0                              !Primera Posicion en el centro (y=0)
        Else If (m/=1 .AND. Impar) Then
           call CalcPos(m-1,  yPos)              !Calculando Posicion sobre Y (Impar)
        Else
           Call CalcPos(m, yPos)                 !Calculando Posición Sobre Y
        End If

        Write(1,*) xPos, yPos                    !Escribiendo en Salida
        m = m+1                                  !Avanzar Contador
     End Do
     
     m=1                                         !Reiniciar el Contador
     k = k+1                                     !Avanzar Contador
  End Do
  
  Close(1)
  
End Program ActIII
