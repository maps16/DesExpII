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
IFS=":"
LIST="01:02:03:04:05:06:07:08:09"

for i in $LIST ; do
    /usr/bin/time ./SimularHS$i >&Salida$i&
    /bin/sleep 2
done
