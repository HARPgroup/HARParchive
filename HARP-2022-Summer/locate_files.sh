#!/bin/bash
#this script should be run from directory: /opt/model/p53/p532c-sova
echo -n 'River or Land segment? (river/land) '
read segvar

if [ "$segvar" == "land" ]; then
echo -n 'scenario: '
read scenario_name
echo -n 'landseg: '
read landseg
echo -n 'landuse: '
read landuse
fi

if [ "$segvar" == "river" ]; then
echo -n 'scenario: '
read scenario_name
echo -n 'riverseg: '
read riverseg
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




