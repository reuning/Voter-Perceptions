#!/bin/bash
#SBATCH --account=pmiu0199
#SBATCH --time=168:00:00
#SBATCH --nodes=1 --gpus-per-node=1 --ntasks-per-node=6
#SBATCH --gpu_cmode=shared
#SBATCH --job-name=candidate
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=myjob.out.%j
#SBATCH --mem=100gb


module load gcc/12.3.0
module load cuda/12.8.1
module load R/4.4.0

Rscript --vanilla estimation.R

