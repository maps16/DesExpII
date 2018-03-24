#!/bin/bash
clear
gfortran Mod.f03 Act5.f03 ConfigIni.f03 Conc.f03 -o Simular
./clean.sh
