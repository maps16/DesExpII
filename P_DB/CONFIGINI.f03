Subroutine CONFIGINI

  use cte

  Implicit None

  Real    :: XRan, YRan, ZRan
  Integer :: i


  Ls = (1.0 * Nc/ns)**(1.E0/3.E0)

  Write(*,*) "LONGITUD DE LA CELDA:", Ls
  Open (1, file = "ConfigIni.txt")

  !Posicion de las particulas
  Colocar: Do i = 1, Nc

     !GENERANDO VALORES ALEATORIOS PARA LA POSICION
     Call Random_Number(xRan)                   !VALOR ALEATORIO DE POSICION X \
     Call Random_Number(yRan)                   !VALOR ALEATORIO DE POSICION Y | TENTATIVO   
     Call Random_Number(zRan)                   !VALOR ALEATORIO DE POSICION Z /

     X(i) = (xRan-0.5)*(Ls-1)                   !\
     Y(i) = (yRan-0.5)*(Ls-1)                   !|   [-(Ls-1)/2 , (Ls-1)/2] AJUSTANDO A LA CELDA
     Z(i) = (zRan-0.5)*(Ls-1)                   !/

     Write(1,*) X(i), Y(i), Z(i) ! GUARDANDO CONFIGURACION INICIAL EN ARCHIVO

  End Do Colocar

  Close(1)

End Subroutine CONFIGINI
