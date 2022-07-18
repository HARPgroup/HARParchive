#!/bin/bash
if [ $# -lt 2 ]; then. #Counts number of input args
  echo "Usage: cbp img_gen scenario basin "
    exit 1
fi
# load paths
. hspf_config #Loads version-specific cbp variables 
scenario_name=$1 # for example hsp2_2022
basin=$2
data_source_table='RESULTS/PERLND_P001/PWATER/table'. #This is the same for all land seg h5s


# check the directories
if [ ! -d "$CBP_EXPORT_DIR" ] ; then mkdir $CBP_EXPORT_DIR; fi
if [ ! -d "$CBP_EXPORT_DIR/$scenario_name" ] ; then mkdir $CBP_EXPORT_DIR/$scenario_name; fi
if [ ! -d "$CBP_EXPORT_DIR/$scenario_name/eos" ] ; then mkdir $CBP_EXPORT_DIR/$scenario_name/eos; fi
# do we need to add a line that creates an 'images' directory if one doesn't already exist?

land_use_list=$(ls /opt/model/p53/p532c-sova/output/hspf/land/out)
echo $land_use_list

segments=`cbp get_landsegs $basin`

  for landseg in $segments; do
    for landuse in $land_use_list; do
      h5_file_path=${landuse}${landseg}.h5 #Modify using CBP_ROOT ? or CBP_EXPORT_DIR ?
      echo $h5_file_path

      output_file_path=${landuse}${landseg}_pwater.csv
      echo $output_file_path

      #TO DO:  test calling of R scripts. Also verify that old h5's are deleted once converted.
  image_file_path=${CBP_EXPORT_DIR}${scenario_name}'/images' # We think this is right - glenn & nicole

      echo "Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_path $data_source_table"
      Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/export_hsp_h5.R $h5_file_path $output_file_path $data_source_table

      #remove old .h5 file
      del h5_file_path

      echo "Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/hsp_pwater.R $landseg $scenario_name $landuse $output_file_path $image_file_path"
      Rscript HARParchive/HARP-Summer-2022/AutomatedScripts/hsp_pwater.R $landseg $scenario $landuse $output_file_path $image_file_path

      done
  done