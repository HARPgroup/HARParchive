#!/bin/bash

. hspf_config #Loads version-specific cbp variables

riverseg_list=$1 #input a list of riversegments ?
channel=$2
scenario=$3 #subsheds

cd /opt/model/p6/vadeq #or just run batch from this Dir.

parameters=`cbp get_config $scenario river PARAMETERS`

# running the R script: 
for i in ${riverseg_list} ; do
 
  output_path="$CBP_ROOT/input/param/river/$parameters/ftables/"

  Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/ftable_creation.R "${i}" "${channel}" "${output_path}"

  echo "${i}"
done

# for debugging purposes: 
echo 'riversegs:' $riverseg_list
echo 'channel:' $channel
echo 'scenario:' $scenario
echo 'parameters:' $parameters
echo 'CBP_ROOT:' $CBP_ROOT
echo 'ftable outputs:' $output_path
