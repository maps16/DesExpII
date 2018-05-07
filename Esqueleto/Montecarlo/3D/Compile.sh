#!/bin/bash
clear
gfortran Mod.f03 ConfigIni.f03 Main.f03 EnergyConfigHS.f03 EnergyPartHS.f03 GDR.f03  -o Simular
./clean.sh
