! En esta subroutina calculamos las propiedades de autodifusion,
! el desplazamiento cuadratico medio
! y el coeficiente de difusion dependiente del tiempo


Subroutine WDT
  Use cte

  Implicit None

  Integer :: i, j, k,  L, ntmax
  Real    :: dif, wtx, wty, wtz, wt, time


  Open  (55, File = "wdt_D.txt")
  Open  (56, File = "wdt_W.txt")

  tim = Real(NFREC2) * Dt

  !Comenzamos el ciclo para dar la cadencia en el barrimiento temporal

  Do i = 1, KI - 1

     ntmax = KI - i

     wtx = 0
     wty = 0
     wtz = 0
     wt  = 0


     ! Iniciamos el barrimiento de particulas

     Do L = 1, Nc


        ! Ciclo para calcular el avance temporal
        Do j = 1, ntmax

           wtx = wtx + (CXR(L, i + j) - CXR(L, j))**2
           wty = wty + (CYR(L, i + j) - CYR(L, j))**2
           wtz = wtz + (CZR(L, i + j) - CZR(L, j))**2

        End Do

     End Do


     time = tim * Real(i)
     wt   = (wtx + wty + wtz)/(Real(ntmax)* Real(Nc)* 6)
     dif  = wt/time


     Write (55, *) time, dif
     Write (56, *) time, wt


  End Do

  Close (55)

  Close (56)

End Subroutine WDT

