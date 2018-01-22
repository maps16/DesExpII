Program ActII
  Implicit None
  Real *4 :: sep, pos , neg
  Integer :: N, l, i !Numero de particulas y Longitud, Contador

  !Datos de Entrada
  Write(*,*) "Ingrese el numero de puntos"
  Read(*,*) N
  Write(*,*) "Ingrese la longitud dode poner los puntos"
  Read(*,*) l
  
  !Separación entre partículas
  sep = real(l)/real(N)
  !i=1
  !do while (i<=N)
  !   pos=((-1)**i)*((int((i-1-mod(N,2))/2)*sep)+ (sep*mod(N,2)/2) )
  !   write(*,*)i, pos
  !   i = i+1
  !end do

  
  !Incio de Contador
  i=1
  neg = (sep*(-1)) / 2
  write(*,*) i,neg
  i=i+1
  pos = sep / 2
  write(*,*) i,pos 
  do while (i<=N-1)
     i = i + 1
     neg = neg - sep    !Calculo de los puntos del lado negativo (impares) Posicion
     write(*,*) i,neg
     if (i==N) exit     !Salida de Emergencia para N impar
     i = i + 1
     pos = pos + sep    !Calculo de los puntos del lado positivo (pares) Posicion
     write(*,*) i,pos 
  end do

 ! write(*,*)  0**91 !DEBUG
End Program ActII
