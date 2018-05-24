Subroutine CONFIGINI

  use cte

Implicit None

   Real    :: XRan, YRan, ZRan, xij, yij, zij,  dist
   Integer :: i, j


         Ls = (1.E0 * Nc/ns)**(1.E0/3.E0)

   Write(*,*) "Tamaño de la Celda:", Ls
   Open (1, file = "ConfiginiIni.txt")

!Posicion de las particulas
Do i = 1, Nc

2  Call Random_Number(XRan)
   Call Random_Number(YRan)
   Call Random_Number(ZRan)

         X(i) = (XRan - 0.5) * (Ls - 1)
         Y(i) = (YRan - 0.5) * (Ls - 1)
         Z(i) = (ZRan - 0.5) * (Ls - 1)

     Do j = 1 , i - 1

        xij = X(i) - X(j)
        yij = Y(i) - Y(j)
        zij = Z(i) - Z(j)

        dist = xij*xij + yij*yij + zij*zij

        !Verificar traslapes
           If(dist .LE. sigma) then

              Go To 2

           End If

     End Do

     Write(1,*) X(i), Y(i), Z(i)

End Do

Close(1)

End Subroutine CONFIGINI
