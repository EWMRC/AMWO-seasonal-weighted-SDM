#!/bin/bash
# script to run the foo program on asax
cd home/aublab001/Penn_migration_model/
apptainer exec docker://rocker/geospatial Rscript hpc_run_all_batches_mig.R