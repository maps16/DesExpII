#!/bin/sh
##
## mv.sh
## 
## Made by Martin Alejandro Paredes Sosa
## Login   <maparedes@acf00.fisica.uson.mx>
## 
## Started on  Wed May  9 10:50:21 2018 Martin Alejandro Paredes Sosa
## Last update Time-stamp: <2010-oct-11.lunes 17:30:06 (calcaneo)>
##
/usr/bin/time ./SimularHS01 >&Salida01&
/bin/sleep 2
/usr/bin/time ./SimularHS02 >&Salida02&
/bin/sleep 2
/usr/bin/time ./SimularHS03 >&Salida03&
/bin/sleep 2
/usr/bin/time ./SimularHS04 >&Salida04&
/bin/sleep 2
/usr/bin/time ./SimularHS05 >&Salida05&
/bin/sleep 2
/usr/bin/time ./SimularHS06 >&Salida06&
/bin/sleep 2
/usr/bin/time ./SimularHS07 >&Salida07&
/bin/sleep 2
/usr/bin/time ./SimularHS08 >&Salida08&
/bin/sleep 2
/usr/bin/time ./SimularHS09 >&Salida09&
/bin/sleep 2
#usr/bin/time ./SimularHS10 >&Salida10&
#bin/sleep 2
