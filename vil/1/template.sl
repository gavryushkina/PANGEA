#!/bin/bash
# FLEXPART­WRF SubmitScript

#SBATCH -J BEAST2
#SBATCH -A uoa00180 
#SBATCH --time=6:00:00
#SBATCH --mem-per-cpu=16384
#SBATCH -o FILE.out 
#SBATCH -e FILE.err #SBATCH ­C sb


module load beagle-lib/20140322-goolf-1.5.14
module load Java/1.8.0_5
srun java -Xmx10g -Djava.library.path=$BEAGLE_LIB_PATH -jar ~/beast2.jar -resume -beagle_SSE ./FILE
