# function for locating h5 files for river or land segments
# cbp variables must be generated using hspf_config before using this function

function h5_file_retrieve {
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

if [ "$segvar" == "river" ]; then
h5=$CBP_EXPORT_DIR/river/$scenario_name/h5/$riverseg'.h5'
if  [ -f $h5 ]; then
echo 'River seg h5:' $h5
fi
if  [ ! -f $h5 ]; then
echo 'River seg h5 does not exist at' $CBP_EXPORT_DIR/river/$scenario_name/h5 ; fi
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
echo 'Land seg h5 does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/h5 'or' 
$CBP_ROOT/output/hspf/land/out/$landuse/$scenario_name ;
fi
fi
}
 echo 'Loading h5_file_retrieve function'
