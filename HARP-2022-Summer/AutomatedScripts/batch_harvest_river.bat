#!/bin/bash

#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the river directories
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/hydr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/hydr; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/hydr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/divr; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/hydr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/ps_flow; fi

echo 'CBP_ROOT:' $CBP_ROOT
echo 'CBP_EXPORT_DIR:' $CBP_EXPORT_DIR

# file paths
h5_file_path=$CBP_EXPORT_DIR/river/$scenario_name/h5/$basin'.h5'

output_path=$CBP_EXPORT_DIR/river/$scenario_name/

data_source_hydr=/RESULTS/RCHRES_R001/HYDR
data_source_divr=/TIMESERIES/TS3007
data_source_ps=/TIMESERIES/TS3000

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/hydr/$basin'_hydr.csv' $data_source_hydr'/table'
  echo 'hydr csv created'

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/divr/$basin'_divr.csv' $data_source_divr'/table'
  echo 'divr csv created'

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/ps_flow/$basin'_psflow.csv' $data_source_ps'/table'
  echo 'ps csv created'