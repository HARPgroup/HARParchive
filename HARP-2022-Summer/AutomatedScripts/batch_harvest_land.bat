#!/bin/bash

#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
basin=$2

# check the land/pwater directories
 if [ ! -d $CBP_EXPORT_DIR ] ; then mkdir $CBP_EXPORT_DIR; fi
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name; fi
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name/pwater ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/pwater; fi

# check the land/iwater directories
 if [ ! -d $CBP_EXPORT_DIR/land/$scenario_name/iwater ] ; then mkdir $CBP_EXPORT_DIR/land/$scenario_name/iwater; fi


echo 'CBP_ROOT:' $CBP_ROOT
echo 'CBP_EXPORT_DIR:' $CBP_EXPORT_DIR

# detecting the data source
data_source_per=/RESULTS/PERLND_P001/PWATER
data_source_imp=/RESULTS/IMPLND_I001/IWATER

# extract land segments and -uses from directory

land_use_list=$(ls $CBP_ROOT/output/hspf/land/out)
echo 'land use list:' $land_use_list

segments=`cbp get_landsegs $basin`
 echo 'land segments in basin:' $segments

  for landseg in $segments; do
    for landuse in $land_use_list; do
      h5_file_path_land=$CBP_EXPORT_DIR/land/h5/$landuse/$scenario_name/$landuse$landseg'.h5'
         
        output_path_pwater=$CBP_EXPORT_DIR/land/$scenario_name/pwater/$landuse$landseg
        output_path_iwater=$CBP_EXPORT_DIR/land/$scenario_name/iwater/$landuse$landseg

# creating the output csv with appropriate naming conventions

landvar_per=($(Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/detect_data_source.R $h5_file_path_land $data_source_per))

landvar_imp=($(Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/detect_data_source.R $h5_file_path_land $data_source_imp))


if  [ $landvar_per -eq 1 ]; then
  Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path_land    $output_path_pwater'_pwater.csv' $data_source_per'/table'
    echo 'pwater csv created'
fi
if  [ $landvar_imp -eq 1 ]; then
  Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/export_hsp_h5.R $h5_file_path_land$output_path_iwater'_iwater.csv' $data_source_imp'/table'
  echo 'iwater csv created'
fi


done
done




