#!/bin/bash
#generates new subshed names and adds them to a master list of rivernames

. hspf_config #Loads version-specific cbp variables

subsheds_path=$1
	#/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/subshed_riversegs.txt
master_list=$2
	#/opt/model/p6/vadeq/config/catalog/geo/vahydro/rivernames.csv

dataset=`cbp get_config subsheds river GEO`
	#results as "vahydro"
  echo 'dataset from GEO:' $dataset

for /F "tokens=*" %%G in ($subsheds_path) do [Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/subsheds_naming.R %%G $master_list] echo 'subshed %%G was renamed'

	