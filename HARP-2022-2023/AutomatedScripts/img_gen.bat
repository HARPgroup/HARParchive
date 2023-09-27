#!/bin/bash

. hspf_config #Loads version-specific cbp variables
scenario_name=$1 # for example hsp2_2022
basin=$2

data_source_table='RESULTS/PERLND_P001/PWATER/table'. #This is the same for all land seg h5s

# check the directories
if [ ! -d "$CBP_EXPORT_DIR" ] ; then mkdir $CBP_EXPORT_DIR; fi
if [ ! -d "$CBP_EXPORT_DIR/land/$scenario_name" ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name; fi
if [ ! -d "$CBP_EXPORT_DIR/land/$scenario_name"/eos ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/eos; fi
if [ ! -d "$CBP_EXPORT_DIR/land/$scenario_name"/images ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/images; >


land_use_list=$(ls $CBP_ROOT/output/hspf/land/out)
echo $land_use_list

segments=`cbp get_landsegs $basin`
echo $segments

  for landseg in $segments; do
    for landuse in $land_use_list; do
      h5_file_path="$CBP_ROOT/output/hspf/land/out/$landuse/$scenario_name/$landuse$landseg.h5"
      #echo $h5_file_path

      output_file_path="$CBP_EXPORT_DIR/land/$scenario_name/eos/$landuse$landseg"_pwater.csv
      #echo $output_file_path

      image_file_path="$CBP_EXPORT_DIR/land/$scenario_name/images"
      #echo $image_file_path

      #echo "Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_pa>
      Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_path $dat>

      #remove old .h5 file
      #del h5_file_path
      #echo "Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater.R $landseg $scenario_name $landuse>
      Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/hsp_pwater.R $landseg $scenario $landuse $image_file>

      done
  done


