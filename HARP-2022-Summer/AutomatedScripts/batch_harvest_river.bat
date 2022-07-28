#!/bin/bash

#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the river directories
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/hydr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/hydr; fi

echo 'CBP_ROOT:' $CBP_ROOT
echo 'CBP_EXPORT_DIR:' $CBP_EXPORT_DIR

# file paths
h5_file_path=$CBP_EXPORT_DIR/river/$scenario_name/h5/$basin'.h5'

output_path=$CBP_EXPORT_DIR/river/$scenario_name/hydr/$basin

data_source_riv=/RESULTS/RCHRES_R001/HYDR

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path'_hydr.csv' $data_source_riv'/table'
  echo 'hydr csv created'

done