#!/bin/bash

. hspf_config #Loads version-specific cbp variables

riverseg_list=$1 #a list of riversegments (e.g. 'OR1_7700_7980 JL2_6850_6890') -OR- 'subsheds' for all subsheds
channel=$2 # '0. River Channel' for now
scenario=$3 # 'subsheds' for now

cd /opt/model/p6/vadeq #so that this script can be run from wherever

parameters=`cbp get_config $scenario river PARAMETERS`

output_path="$CBP_ROOT/input/param/river/$parameters/ftables/"


if [[ ${riverseg_list} == subsheds ]]
then
  subshed_rsegs= `cat ${output_path}subshed_riversegs.txt`
  for i in ${subshed_rsegs} ; do
    #Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/ftable_creation.R "${i}" "${channel}" "${output_path}"
    echo "${i}"
  done

else
  for i in ${riverseg_list} ; do
    #Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/ftable_creation.R "${i}" "${channel}" "${output_path}"
    echo "${i}"
    # for debugging purposes:
    #echo 'riversegs:' $riverseg_list
    #echo 'channel:' $channel
    #echo 'scenario:' $scenario
    #echo 'parameters:' $parameters
    #echo 'CBP_ROOT:' $CBP_ROOT
    #echo 'ftable outputs here:' $output_path
  done

fi