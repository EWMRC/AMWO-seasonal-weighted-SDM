#!/bin/bash
# script to run the foo program on asax
apptainer exec docker://rocker/geospatial Rscript hpc_run_all_batches.R