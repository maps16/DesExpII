!==========================================================================================
! SUBRUTINA DE CALCULO DE PROPIEDADES DE AUTO DIFUSION. DESPLAZAMIENTO CUADRATICO MEDIO Y
! COEFICIENTE DE DIFUSION DEPENDIENTE DEL TIEMPO
!
! AUTOR: MARTIN ALEJANDRO PAREDES SOSA
!==========================================================================================

Subroutine WDT(k)

  Use cte
  Implicit None

  Integer :: k, i, j, p, nmax                              !CONTADORES
  Real :: Ti, Time                                         !CONTROL DE TIEMPO
  Real :: Wtx, Wty, Wtz, Wt                                !DESPLAZAMIENTO CUADRATICO MEDIO
  Real :: Dif                                              !DIFUSION

  Open(96, File="wdt.dat")

  !TIEMPO ENTRE CONFIGURACIONES
  Ti = Real(iSave2) * dt                                 

  !BARRIDO TEMPORAL
  TEMPO: Do i = 1 , k-1

     nmax = k - i

     Wtx = 0.0
     Wty = 0.0
     Wtz = 0.0
     Wt = 0.0

     !BARRIMIENTO DE PARTICULAS
     BPart: Do p = 1, N

        !BARRIDO EN TIEMPO
        Tiempo: Do j = 1, nmax
           
           Wtx = Wtx + ( CXD(p,i+j) - CXD(p,j)  ) * ( CXD(p,i+j) - CXD(p,j)  )
           wty = Wty + ( CYD(p,i+j) - CYD(p,j)  ) * ( CYD(p,i+j) - CYD(p,j)  )
           Wtz = Wtz + ( CZD(p,i+j) - CZD(p,j)  ) * ( CZD(p,i+j) - CZD(p,j)  )
           
        End Do Tiempo
        
     End Do BPart

     Time = Ti * Real(i)
     Wt = (Wtx + Wty + Wtz ) / ( Real(nmax) * Real(N) * 6.0 )
     Dif = Wt / Time

     Write(96,*) Time, Wt, Dif

  End Do TEMPO
  Close(96)

End Subroutine WDT
