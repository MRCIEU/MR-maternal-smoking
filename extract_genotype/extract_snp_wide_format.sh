#!/bin/bash
#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
ls -l .bashrc

#require qctool
module add apps/qctool-2.0

#extract the snp using qctool
qctool -g ${UKB_DATA}/_latest/UKBIOBANK_Array_Genotypes_500k_HRC_Imputation/data/dosage_bgen/data.chr#.bgen -og ${HOME}/mini_project3/data/check/smoke.bgen -incl-rsids ${HOME}/mini_project3/snp1.txt


