#!/bin/bash

. hspf_config #Loads version-specific cbp variables

riverseg_list=$1 #input a list of riversegments ?
channel=$2
scenario=$3

cd /opt/model/p6/vadeq #or just run batch from this Dir.

parameters=`cbp get_config vadeq_2021 river PARAMETERS`

# check the directory where ftables will be stored
#if [ ! -d $CBP_ROOT/input/param/river/$scenario ] ; then mkdir $CBP_ROOT/input/param/river/$scenario; fi
#if [ ! -d $CBP_ROOT/input/param/river/$scenario/$parameters ] ; then mkdir $CBP_ROOT/input/param/river/$scenario/$parameters; fi
#if [ ! -d $CBP_ROOT/input/param/river/$scenario/$parameters/ftables ] ; then mkdir $CBP_ROOT/input/param/river/$scenario/$parameters/ftables; fi

#trying home dir. for debugging because of permission errors:
if [ ! -d ~/$scenario ] ; then mkdir ~/$scenario; fi
if [ ! -d ~/$scenario/$parameters ] ; then mkdir ~/$scenario/$parameters; fi
if [ ! -d ~/$scenario/$parameters/ftables ] ; then mkdir ~/$scenario/$parameters/ftables; fi

# running the R script: 
for i in ${riverseg_list} ; do
 
#output_path="$CBP_ROOT/input/param/river/$scenario/$parameters/ftables/"
output_path="~/$scenario/$parameters/ftables/"

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
