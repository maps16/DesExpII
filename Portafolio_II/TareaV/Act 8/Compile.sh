#!/bin/bash
clear
gfortran Mod.f03 ConfigIni.f03 ConfigIniReg.f03 Act8.f03 EnergyConfigHD.f03 EnergyPartHD.f03 GDR.f03  -o Simular
#./clean.sh
