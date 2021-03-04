#!/bin/bash
#SBATCH -n 48
#SBATCH -N 1
#SBATCH --partition=general
#SBATCH --job-name=mc1jags
#SBATCH --output=log-%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --time=99:00:00
#SBATCH --mail-type=END  	  # Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=jdeleeuw@vassar.edu

Rscript ~/many-classes-1/run-all-models-hpc.R 
