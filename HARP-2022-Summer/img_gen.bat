#!/bin/bash
if [ $# -lt 2 ]; then
  echo "Usage: cbp img_gen scenario basin"
    exit 1
fi
# load paths
. hspf_config
scenario_name=$1
basin=$2

# check the directories
#if [ ! -d "$CBP_EXPORT_DIR" ] ; then mkdir $CBP_EXPORT_DIR; fi
#if [ ! -d "$CBP_EXPORT_DIR/$scenario" ] ; then mkdir $CBP_EXPORT_DIR/$scenario_name; fi
#if [ ! -d "$CBP_EXPORT_DIR/$scenario/eos" ] ; then mkdir $CBP_EXPORT_DIR/$scenario_name/eos; fi

segments=`cbp get_landsegs $basin`
for landseg in $segments; do
  
  for landuse in landuses do
  h5_file_path=${landuse}${landseg}.h5
  echo h5_file_path

  SET output_file_path=${landuse}${landseg}_pwater.csv
  echo output_file_path

  echo "Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_path $data_source_table"
  
  echo "Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/hsp_pwater.R $landseg $scenario $landuse $output_file_path $image_file_path "
   
  #remove old .h5 file
  del h5_file_path
  done
done