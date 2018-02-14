!========================================================================
! PROGRAMA PRINCIPAL DEL LA SIMULACION, LLAMA SUBRUTINAS PARA REALIZAR 
! LA SIMULACION
!Autor: Martin Alejandro Paredes Sosa
!========================================================================
Program Main
  Use cte
  Implicit None
  Integer :: i, j                                     !Contadores

  !PEDIR DENSIDAD Y NUMERO DE PARTICULAS
  Write(*,*) "INGRESE EL NUMERO DE PARTICULAS"
  Read(*,*) N
  Write(*,*) "INGRESE CONCENTRACION REDUCIDA"
  Read(*,*) Dens
  
  Call ConfigIni
End Program Main
