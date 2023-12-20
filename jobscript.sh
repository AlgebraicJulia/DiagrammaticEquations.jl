#!/bin/bash
#SBATCH --job-name=daemon_job_test    # Job name
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=cuffaro.m@ufl.edu # Where to send mail	
#SBATCH --ntasks=1                    # Run on a single CPU
#SBATCH --mem=8gb                     # Job memory request
#SBATCH --time=00:05:00               # Time limit hrs:min:sec
#SBATCH --output=daemon_test_%j.log   # Standard output and error log
pwd; hostname; date

module load julia

echo "Installing Packages..."

julia --project=docs/ -e 'using Pkg; Pkg.status()'

echo "Running Tests..."
julia --project -e 'using Pkg; Pkg.status; Pkg.test()'

echo "Building Documentation..."
julia --project=docs/ -e 'using Pkg; Pkg.instantiate(); include("docs/make.jl")'
