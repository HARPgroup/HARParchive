#!/bin/bash
#this script should be run from directory: /opt/model/p53/p532c-sova
if [ $# -ne 4 ] && [ $# -ne 3 ]; then
      echo ' '
      echo 'usage:  locate_h5.sh river scenario riverseg'
      echo ' or     locate_h5.sh land scenario landseg landuse'
      echo ' '
exit
fi

segvar=$1

if [ "$segvar" == "land" ]; then
scenario_name=$2
landseg=$3
landuse=$4
fi

if [ "$segvar" == "river" ]; then
scenario_name=$2
riverseg=$3
fi

. hspf_config #Loads version-specific cbp variables


if [ "$segvar" == "river" ]; then
h5=$CBP_EXPORT_DIR/river/$scenario_name/h5/$riverseg'.h5'
if  [ -f $h5 ]; then
echo 'River seg h5:' $h5
fi
if  [ ! -f $h5 ]; then
echo 'River seg h5 does not exists at' $CBP_EXPORT_DIR/river/$scenario_name/h5 ; fi
fi

if [ "$segvar" == "land" ]; then
h5_land=$CBP_EXPORT_DIR/land/$scenario_name/h5/$landseg'.h5'
if  [ -f $h5_land ]; then
echo 'Land seg h5:' $h5_land
fi
h5_land_root=$CBP_ROOT/output/hspf/land/out/$landuse/$scenario_name/$landuse$landseg'.h5'
if  [ -f $h5_land_root ]; then
echo 'Land seg h5:' $h5_land_root
fi
if  [ ! -f $h5_land ] && [ ! -f $h5_land_root ]; then
echo 'Land seg h5 does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/h5 'or' $CBP_ROOT/output/hspf/land/out/$landuse/$scenario_name ;
fi
fi





