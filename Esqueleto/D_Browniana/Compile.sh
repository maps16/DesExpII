#!/bin/bash
clear
gfortran Mod.f03 ConfigIni.f03 Main.f03 Fuerzas.f03 RanGauss.f03 GDR.f03  -o Simular
./clean.sh
