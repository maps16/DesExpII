Program ActII
  Implicit None
  Real *4 :: sep, pos, neg, l
  Integer :: N, i !Numero de particulas , Contador

  !Datos de Entrada
  Write(*,*) "Ingrese el numero de puntos"
  Read(*,*) N
  Write(*,*) "Ingrese la longitud dode poner los puntos"
  Read(*,*) l
  
  !Separación entre partículas
  sep = l/real(N)
	
  Open(1, file="Out.dat")   !Abrir Archivo de Salida



  i=1 !Inicio Contador de la particula
  do while (i<=N)                                !Terminar al recorer cada particula
     pos=((-1)**i)*((int((i-1)/2)*sep)+ (sep/2) )!Calculo de Posicion
     write(1,*)i, pos                            !Escribir Valor En Archivo 1 (Out.dat)
     i = i+1                                     !Avance contador

  end do
  close(1)

End Program ActII

!======================================================================
!============== EXTRAS / INTENTOS DIFERENTES ==========================
!======================================================================
  
  !INTENTO DE COLOCAR UNA PARTICULA CENTRAL !! NO SE LOGRO
  !i=1
  !do while (i<=N)
  !   pos=((-1)**i)*((int((i-1-mod(N,2))/2)*sep)+ (sep*mod(N,2)/2) )
  !   write(*,*)i, pos
  !   i = i+1
  !end do

  !PRIMER INTENTO SE LOGRO REDUCIR 
  !Incio de Contador
  !i=1
  !neg = (sep*(-1)) / 2  !Primera Posicion negativa
  !write(*,*) i,neg
  !i=i+1
  !pos = sep / 2         !Primera Posision positiva
  !write(*,*) i,pos 
  !do while (i<=N-1)
  !   i = i + 1
  !   neg = neg - sep    !Calculo de los puntos del lado negativo (impares) Posicion
  !   write(*,*) i,neg
  !   if (i==N) exit     !Salida de Emergencia para N impar
  !   i = i + 1
  !   pos = pos + sep    !Calculo de los puntos del lado positivo (pares) Posicion
  !   write(*,*) i,pos 
  !end do
