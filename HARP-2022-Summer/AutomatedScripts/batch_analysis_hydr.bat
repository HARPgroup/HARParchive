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

output_file_path=$CBP_EXPORT_DIR/river/$scenario_name/hydr/$riverseg'_hydr.csv' 

 if [ -f $output_file_path ]  ; then #executes next commands if the hydr csv exists

line=$(head -n 1 $output_file_path) #
var=$(echo "$line" | grep -o Qout) 

if [ $var == "Qout" ] ; then  
echo 'Qout already exists for' $riverseg'_hydr.csv' ', no conversion needed'
else 
Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_hydr_conversion.R $output_file_path
echo 'Conversion ran for' $riverseg'_hydr.csv'
fi

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_hydr_analysis.R $riverseg $scenario_name $CBP_EXPORT_DIR/river/$scenario_name/hydr/ $image_file_path $model_version

echo 'R analysis scripts were run for' $riverseg'_hydr.csv'

 elif [ ! -f $output_file_path ]  ; then #executes next command if hydr csv doesn't exist

echo 'No analysis performed, no flow data for' $riverseg


fi
done
