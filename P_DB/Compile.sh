#!/bin/sh
##
## Compile.sh
## 
## Made by Martin Alejandro Paredes Sosa
## Login   <maparedes@acf00.fisica.uson.mx>
## 
## Started on  Tue May 29 16:42:13 2018 Martin Alejandro Paredes Sosa
## Last update Time-stamp: <2010-oct-11.lunes 17:30:06 (calcaneo)>
##
clear
gfortran VARIABLES.f03 MAIN.f03 CONFIGINI.f03 FUERZAS.f03 GDR.f03 RGAUSS.f03 WDT.f03 -o Simular
