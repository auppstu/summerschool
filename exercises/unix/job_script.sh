#!/bin/bash
#SBATCH -J my_job_name_A
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 24
#SBATCH -ptest
#SBATCH -t 5

aprun ./prog
