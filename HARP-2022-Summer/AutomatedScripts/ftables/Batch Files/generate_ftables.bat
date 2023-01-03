#!/bin/bash

. hspf_config #Loads version-specific cbp variables

riverseg_list=$1 #a list of riversegments (e.g. 'OR1_7700_7980 JL2_6850_6890') -OR- 'subsheds' for all subsheds
channel=$2 # '0. River Channel' for now
scenario=$3 # 'subsheds' for now

cd /opt/model/p6/vadeq #so that this script can be run from wherever

parameters=`cbp get_config $scenario river PARAMETERS`

output_path="$CBP_ROOT/input/param/river/$parameters/ftables/"


#for ALL subsheds:
if riverseg_list== subsheds
  @ECHO OFF
  :choice
  set /P c=Would you like to refresh the list of subsheds? This is time-consuming. [Y/N]?
  if /I "%c%" EQU "Y" goto :regen
  if /I "%c%" EQU "N" goto :use
  goto :choice

  :regen
    echo "Re-generating subshed_riversegs.csv..."
    pause
    #Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/get_subshed_riversegs.R "${output_path}"
    goto :use
  
  :use
    echo "Using last redition of subshed_riversegs.csv ..."
    pause
    for /F "tokens=1 delims= " %i in (${output_path}subshed_riversegs.csv) ; do 
      @echo %i
    done
  exit
  
  
#for a select list of riversegs:
else 
# running the R script: 
for i in ${riverseg_list} ; do

  #Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/ftable_creation.R "${i}" "${channel}" "${output_path}"

  echo "${i}"
done

# for debugging purposes: 
#echo 'riversegs:' $riverseg_list
#echo 'channel:' $channel
#echo 'scenario:' $scenario
#echo 'parameters:' $parameters
#echo 'CBP_ROOT:' $CBP_ROOT
#echo 'ftable outputs here:' $output_path



