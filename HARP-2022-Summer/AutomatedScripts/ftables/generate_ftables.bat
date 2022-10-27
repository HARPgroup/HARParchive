#!/bin/bash

riverseg_list=$1 #input a list of riversegments ?
channel=$2

cd /opt/model/p6/vadeq

scenario = cbp get_config vadeq_2021 river PARAMETERS 

# check the directory where ftables will be stored
if [ ! -d $CBP_ROOT] ; then mkdir $CBP_ROOT; fi
if [ ! -d $CBP_ROOT/input/param/river/$scenario] ; then mkdir $CBP_ROOT/input/param/river/$scenario; fi
if [ ! -d $CBP_ROOT/input/param/river/$scenario/ftables ] ; then mkdir $CBP_ROOT/input/param/river/$scenario/ftables; fi


# running the R script: 
for %%i in ($riverseg_list) do
 
 output_path= $CBP_ROOT/input/param/river/$scenario/ftables/%%i.ftable
 
  ftable= ($(Rscript ~/HARParchive/HARP-2022-Summer/AutomatedScripts/ftable_creation.R %%i $channel $output_path))
  
done