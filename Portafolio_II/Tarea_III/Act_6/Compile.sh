#!/bin/bash
clear
gfortran Mod.f03 Act6.f03 ConfigIniReg.f03 EnergyPartHD.f03 EnergyConfigHD.f03 -o Simular
./clean.sh
