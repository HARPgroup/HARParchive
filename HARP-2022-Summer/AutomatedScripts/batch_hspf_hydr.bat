#!/bin/bash
#generates hydr files from hspf 0111.csv files

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the stream directory
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/river/$scenario_name/stream ] ; then mkdir $CBP_EXPORT_DIR/river/$scenario_name/stream; fi

segments=`cbp get_riversegs $basin`
 echo 'river segments in basin:' $segments

  for riverseg in $segments; do

input_file_path=$CBP_EXPORT_DIR/river/$scenario_name/stream/$riverseg'_0111.csv'
output_file_path=$CBP_EXPORT_DIR/river/$scenario_name/stream/$riverseg'_hydr.csv'

 if [ -f $input_file_path ]  ; then #executes next commands if the csv exists

Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/create_hspf_hydr.R $input_file_path $output_file_path

echo 'csv was created:' $riverseg'_hydr.csv'

 elif [ ! -f $input_file_path ]  ; then #executes next command if csv doesn't exist

echo 'no hspf csv file exists for' $riverseg

fi
done