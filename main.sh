#!/bin/bash

module load intel
module load R

#######################################
# Experiment monitor
#######################################

INIDATE_="20181108 00"
CYCLES_=24
EXP_=E6_long

echo "Starting..." $INIDATE_ $CYCLES_ $EXP_

## Convert diag_files to csv

cd /glade/u/home/pcorrales/GSI-LETKF-WRF/util/read_diag

# ./read_diag_conv_mean.sh "20181120 18" 24 E10_long
./read_diag_conv_mean.sh "${INIDATE_}" ${CYCLES_} ${EXP_}

./read_diag_rad_mean.sh "${INIDATE_}" ${CYCLES_} ${EXP_}

## Read csv files and plor some data

cd /glade/scratch/pcorrales/monitoring/
Rscript count_obs.R ${EXP_}
