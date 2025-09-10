#!/bin/bash
#SBATCH --account=pmiu0199
#SBATCH --time=24:00:00
#SBATCH --ntasks=8
#SBATCH --job-name=latent_protest
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=myjob.out.%j
#SBATCH --mem=260gb

module load gcc/12.3.0
module load R/4.4.0

Rscript --vanilla outputs.R

