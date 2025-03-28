#!/bin/bash
#performs analysis for pwater csvs harvested from a river segment and exports figures, metrics, and stats to VAHydro
#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the directory where figures from analysis will be stored
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name/images ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/images; fi

image_file_path=$CBP_EXPORT_DIR/land/$scenario_name/images
echo 'image file path:' $image_file_path

# image file paths
image_file_path=$CBP_EXPORT_DIR/land/$scenario_name/images
 echo 'image file path:' $image_file_path

land_use_list=$(ls $CBP_ROOT/output/hspf/land/out)
echo 'land use list:' $land_use_list

segments=`cbp get_landsegs $basin`
 echo 'land segments in basin:' $segments

  for landseg in $segments; do
    for landuse in $land_use_list; do

  output_file_path=$CBP_EXPORT_DIR/land/$scenario_name/pwater/$landuse$landseg'_pwater.csv' #list of all possible pwater csv file paths


 if [ -f $output_file_path ]  ; then #executes next commands if the pwater csv exists

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater.R $landseg $scenario_name $landuse $output_file_path $image_file_path

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater_stats.R $landseg $scenario_name $landuse $output_file_path $image_file_path

echo 'R analysis scripts were run for' $landuse$landseg'_pwater.csv'


elif [ ! -f $output_file_path ]  ; then #executes next command if pwater csv doesn't exist
echo 'No analysis performed, no groundwater data for' $landuse$landseg

fi
done
done




