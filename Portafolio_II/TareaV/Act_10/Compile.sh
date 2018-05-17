#!/bin/bash
clear
gfortran Mod.f03 ConfigIni.f03 ConfigIniReg.f03 Act10.f03 EnergyConfigHD.f03 EnergyPartHD.f03 -o Simular
./clean.sh
