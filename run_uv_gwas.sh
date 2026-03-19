#!/bin/bash

# Creates a job on Savio

#SBATCH --job-name=GWAS_fullRange_134
#SBATCH --account=fc_blackman
#SBATCH --partition=savio3_htc
#SBATCH --qos=savio_lowprio
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --time=12:00:00
#SBATCH --error=/global/home/users/radha/GWAS134/err_out/GWAS_fullRange_134.err
#SBATCH --output=/global/home/users/radha/GWAS134/err_out/GWAS_fullRange_134.out
#SBATCH --mail-user=radha@berkeley.edu
#SBATCH --mail-type=All

# Exports function names and paths to a global variable

export MODULEPATH=/clusterfs/vector/home/groups/software/sl-7.x86_64/modfiles:$MODULEPATH
module load gcc gsl r python openblas gemma vcftools plink
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/global/home/groups/consultsw/sl-7.x86_64/modules/gsl/2.0/lib/
export LC_CTYPE=en_US.UTF-8

# Loads vcftools and tells it to only look for 14 chromosomes (Mimulus genome is 14 chromosomes) 

#vcftools --vcf new2.14chrs.altallele9indv-max318.recode.chrRename.vcf --chr 1 --chr 2 --chr 3 #--chr 4 --chr 5 --chr 6 --chr 7 --chr 8 --chr 9 --chr 10 --chr 11 --chr 12 --chr 13 --chr 14 --recode #--out new2.14chrs.altallele9indv-max318.recode.chrRename

# Use PLINK to format genotype and phenotype information for GEMMA inputs. Converts .vcf format files to binary PLINK format (.bed/.fam), then reassigns all genotype files with a value of -9 (which PLINK reads as ‘missing’/NA) to 1. 

cd /global/home/users/radha/GWAS134/
plink --vcf new2.14chrs.altallele9indv-max318.recode.vcf --const-fid --make-bed --out new2.14chrs.altallele9indv-max318.recode.vcf.imputed --allow-extra-chr
sed -i -e 's/-9/1/g' new2.14chrs.altallele9indv-max318.recode.vcf.imputed.fam


# Computes a kinship matrix

gemma -bfile new2.14chrs.altallele9indv-max318.recode.vcf.imputed -gk 2 -o new2.14chrs.altallele9indv-max318.imputed_kinship   

cd /global/home/users/radha/GWAS134/UV_phenotype_files

# Run GWAS

for f in *fullRange.txt; do v_name=${f%.txt}; gemma -bfile /global/home/users/radha/GWAS134/new2.14chrs.altallele9indv-max318.recode.vcf.imputed -k /global/home/users/radha/GWAS134/output/new2.14chrs.altallele9indv-max318.imputed_kinship.sXX.txt -p /global/home/users/radha/GWAS134/UV_phenotype_files/$f -lmm 4 -o ../fullRange_$v_name ; done
