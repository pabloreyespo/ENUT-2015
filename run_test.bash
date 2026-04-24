#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J test
#SBATCH -p debug
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 2
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 0-0:5:0
#SBATCH -o enut-i/test_%A_%a.err.out
#SBATCH -e enut-i/test_%A_%a.err.out

#-----------------Toolchain---------------------------
# ----------------Modulos----------------------------
ml python/3.12.3
# ----------------Comando--------------------------

cd enut-i
source enut-env/bin/activate
python data_processing/hola_mundo.py
