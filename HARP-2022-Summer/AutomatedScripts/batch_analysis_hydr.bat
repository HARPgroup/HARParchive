#!/bin/bash
#performs analysis for hydr csvs harvested from river segments and exports figures, metrics, and stats to VAHydro
#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2
model_version=$3

# check the directory where figures from analysis will be stored
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/images ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/images; fi

image_file_path=$CBP_EXPORT_DIR/river/$scenario_name/images
echo 'image file path:' $image_file_path

segments=`cbp get_riversegs $basin`
 echo 'river segments in basin:' $segments

  for riverseg in $segments; do

output_file_path=$CBP_EXPORT_DIR/river/$scenario_name/hydr/$riverseg'_hydr.csv' #list of all possible pwater csv file paths

 if [ -f $output_file_path ]  ; then #executes next commands if the hydr csv exists

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_hydr_analysis.R $riverseg $scenario_name $CBP_EXPORT_DIR/river/$scenario_name/hydr/ $CBP_EXPORT_DIR/river/$scenario_name/images/ $model_version

echo 'R analysis scripts were run for' $riverseg'_hydr.csv'

 elif [ ! -f $output_file_path ]  ; then #executes next command if hydr csv doesn't exist

echo 'No analysis performed, no flow data for' $riverseg

fi
done