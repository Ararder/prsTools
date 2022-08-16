#!/bin/bash
SBATCH ntasks=16
SBATCH mem=15000
SBATCH time=120
plink2 --bfile /nfs/projects/S3/genotypes --score /arvhar/sumstats.snpRes --out /home/folder/for/prs --threads 16 --variance-standardize 
