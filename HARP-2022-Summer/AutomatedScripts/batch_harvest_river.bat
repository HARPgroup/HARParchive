#!/bin/bash

#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the river directories
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/hydr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/hydr; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/divr ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/divr; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/ps_flow ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/ps_flow; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/ovol3 ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/ovol3; fi

echo 'CBP_ROOT:' $CBP_ROOT
echo 'CBP_EXPORT_DIR:' $CBP_EXPORT_DIR

# file paths
h5_file_path=$CBP_EXPORT_DIR/river/$scenario_name/h5/$basin'.h5'

output_path=$CBP_EXPORT_DIR/river/$scenario_name

data_source_hydr=/RESULTS/RCHRES_R001/HYDR
data_source_divr=/TIMESERIES/TS3007
data_source_ps=/TIMESERIES/TS3000

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/hydr/$basin'_hydr.csv' $data_source_hydr'/table'
  echo 'hydr csv created'

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/divr/$basin'_divr.csv' $data_source_divr'/table'
  echo 'divr csv created'

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path/ps_flow/$basin'_psflow.csv' $data_source_ps'/table'
  echo 'ps csv created'


# converting OVOL3 in hydr csv to cfs 
## can be later modified within the Rscript to convert ps_flow and divr to mgd!

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_hydr_conversion.R $output_path/hydr/$basin'_hydr.csv'
  echo 'Qout converted to cfs in hydr.csv'

# exporting a csv in the wdm format from hydr (year, month, day, hour, [wanted col])

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/csv_export_wdm_format.R $output_path/hydr/$basin'_hydr.csv' $output_path/ovol/$basin'_ovol3.csv' 'Qout'
  echo 'wdm format csv with OVOL3 exported'

