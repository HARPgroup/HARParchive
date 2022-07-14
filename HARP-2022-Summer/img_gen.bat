#!/bin/bash
if [ $# -lt 2 ]; then
  echo "Usage: cbp img_gen scenario basin"
    exit 1
fi
# load paths
. hspf_config
scenario=$1
basin=$2

# check the directories
#if [ ! -d "$CBP_EXPORT_DIR" ] ; then mkdir $CBP_EXPORT_DIR; fi
#if [ ! -d "$CBP_EXPORT_DIR/$scenario" ] ; then mkdir $CBP_EXPORT_DIR/$scenario; fi
#if [ ! -d "$CBP_EXPORT_DIR/$scenario/eos" ] ; then mkdir $CBP_EXPORT_DIR/$scenario/eos; fi

segments=`cbp get_landsegs $basin`
for landseg in $segments; do
  
  for landuse in landuses do
  h5_source = %landuse% + %landseg%  .h5
  echo h5_source

  SET csv_output = %landuse% + %landseg%  _pwater.h5
  echo csv_output

  echo "Rscript /media/model/p6/out/land/hsp2_2022/eos/export_hsp_h5.R $h5_source $csv_output $data_source_table"
  Rscript /media/model/p6/out/land/hsp2_2022/eos/export_hsp_h5.R $h5_source $csv_output $data_source_table
  
  echo "Rscript /media/model/p6/out/land/hsp2_2022/eos/hsp_pwater.R $landseg $scenario $csv_output $landuse"
    Rscript /media/model/p6/out/land/hsp2_2022/eos/hsp_pwater.R $landseg $scenario $csv_output $landuse

  #remove old .h5 file
  del h5_file_path
  done
done