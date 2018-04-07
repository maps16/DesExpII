#!/bin/bash
clear
gfortran Mod.f03 ConfigIni.f03 Main.f03 EnergyConfigYuk.f03 EnergyPartYuk.f03 GDR.f03  -o Simular
./clean.sh
