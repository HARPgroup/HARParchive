#!/bin/bash

#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the directories
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name/pwater ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/pwater; fi

echo 'CBP_ROOT:' $CBP_ROOT
echo 'CBP_EXPORT_DIR:' $CBP_EXPORT_DIR

data_source_per=/RESULTS/PERLND_P001/PWATER/table
data_source_imp=/RESULTS/IMPLND_I001/IWATER/table

image_file_path=$CBP_EXPORT_DIR/land/$scenario_name/images
echo 'image file path:' $image_file_path

land_use_list=$(ls $CBP_ROOT/output/hspf/land/out)
echo 'land use list:' $land_use_list

segments=`cbp get_landsegs $basin`
 echo 'land segments in basin:' $segments

  for landseg in $segments; do
    for landuse in $land_use_list; do
      h5_file_path=$CBP_ROOT/output/hspf/land/out/$landuse/$scenario_name/$landuse$landseg'.h5'
      echo $h5_file_path

      output_path_list=$CBP_EXPORT_DIR/land/$scenario_name/pwater/$landuse$landseg


landvar=($(Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/detect_per_imp.R $h5_file_path))
echo 'landvar=' $landvar

if [[ $landvar -eq 1 ]]  ; then
echo 'Pervious'
  Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path_list'_pwater.csv' $data_source_per

elif  [[ $landvar -eq 0 ]]; then
echo 'Impervious'
  Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_path_list'_iwater.csv' $data_source_imp
fi
done
done



