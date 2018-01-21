!Definicion de la funcion a Evaluar
Real *8 Function f (x) !Declaracion
  Implicit None
  Real*8 :: x !Variables que se obtienen
  f =exp(-x*x/2) !funcion
end Function f
!
!Empieza Programa MAIN
Program ActI

  !Definicion de Variables
  Implicit None
  Real *8 :: f               !Funcion
  Real *8 :: x0              !Punto de interes
  Real *8 :: xi , xf, y, z   !Rango de valoración
  Real *4 :: k = 0.1         !Incrementos

  !Pedir Info a User
  Write(*,*) "==================================================="
  Write(*,*) "La función a Evaluar es exp(-x**2/2)"
  Write(*,*) "==================================================="
  Write(*,*) "Punto de Interes x0 a Evaluar"
  Read(*,*) x0

  !Evaluando Funcion en x0 e imprimeiendo en pantalla
  Write(*,*) "exp(-x**2/2) evaluado en", real(x0), "es" , f(x0)
 ! Write(*,*) "==================================================="

  !Calculo en un rango dado
  Write(*,*) "Rango de Medicion, Limite Inferior-Limite Superior"
  Read(*,*)  xi , xf
  
  if (xi > xf) then !Checar el orden de los limites
     Write(*,*) "Escribiste los limites en el orden incorrecto. Corrigiendo"
     z=xi
     xi=xf       !Reacomodo de los Limites
     xf=z
  end if

  !Abriendo documento para escribir salida
  Open(1, file="Dat.dat")
  
  do while (xi <= xf+k)
     y = f(xi)
     Write(1,*) xi, y
     xi =xi + k
  end do

  Close(1)
  Write(*,*) "Fin del Calculo"
  
End Program ActI    
  
