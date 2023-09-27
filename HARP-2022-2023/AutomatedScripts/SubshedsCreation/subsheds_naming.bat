#!/bin/bash
#generates new subshed names and adds them to a master list of rivernames

. hspf_config #Loads version-specific cbp variables

subsheds_path=$1     #/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/subshed_riversegs.txt
list=$2
model_version=$3     #cbp-6.0

dataset=`cbp get_config subsheds river GEO`     #"vahydro"

echo 'CBP_ROOT:' $CBP_ROOT

master_list=$CBP_ROOT/config/catalog/geo/$dataset/$list     #/opt/model/p6/vadeq/config/catalog/geo/vahydro/rivernames.csv
  echo 'list of rivernames from:' $master_list

while read -r line; do
    Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/subsheds_naming.R $line $master_list $model_version
    echo 'renamed subshed:' $line
done < "$subsheds_path"