#!/bin/bash
#this script should be run from directory: /opt/model/p53/p532c-sova

. hspf_config #Loads version-specific cbp variables

scenario_name=$1
segment=$2  #can be land or river segment, should include landuse if land seg 
dataset=$3 #hydr,pwater,iwater

echo 'CBP export directory:'
echo $CBP_EXPORT_DIR


if [ "$dataset" == "hydr" ]; then
echo 'files in hydr folder:'
ls $CBP_EXPORT_DIR/river/$scenario_name/$dataset
h5=$CBP_EXPORT_DIR/river/$scenario_name/h5/$segment'.h5'
if  [ -f $h5 ]; then
echo 'River seg h5:' $h5
fi
if  [ ! -f $h5 ]; then
echo 'River seg h5 does not exists at' $CBP_EXPORT_DIR/river/$scenario_name/h5
fi
hydr=$CBP_EXPORT_DIR/river/$scenario_name/$dataset/$segment'_'$dataset'.csv'
if  [ -f $hydr ]; then
echo 'River seg hydr csv:' $hydr
fi
if  [ ! -f $hydr ]; then
echo 'hydr csv does not exist at' $CBP_EXPORT_DIR/river/$scenario_name/$dataset
fi
fi

if [ "$dataset" == "pwater" ]; then
echo 'files in pwater folder:'
ls $CBP_EXPORT_DIR/land/$scenario_name/$dataset
h5_land=$CBP_EXPORT_DIR/land/$scenario_name/h5/$segment'.h5'
if  [ -f $h5_land ]; then
echo 'Land seg h5:' $h5_land
fi
if  [ ! -f $h5_land ]; then
echo 'Land seg h5 does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/h5
fi
pwater=$CBP_EXPORT_DIR/land/$scenario_name/$dataset/$segment'_pwater.csv'
if  [ -f $pwater ]; then
echo 'Pwater csv:' $pwater
fi
if  [ ! -f $pwater ]; then
echo 'Pwater csv does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/$dataset
fi
fi

if [ "$dataset" == "iwater" ]; then
echo 'files in iwater folder:'
ls $CBP_EXPORT_DIR/land/$scenario_name/$dataset
h5_land=$CBP_EXPORT_DIR/land/$scenario_name/h5/$segment'.h5'
if  [ -f $h5_land ]; then
echo 'Land seg h5:' $h5_land
fi
if  [ ! -f $h5_land ]; then
echo 'Land seg h5 does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/h5
fi
iwater=$CBP_EXPORT_DIR/land/$scenario_name/$dataset/$segment'_iwater.csv'
if  [ -f $iwater ]; then
echo 'Iwater csv:' $iwater
fi
if  [ ! -f $iwater ]; then
echo 'Iwater csv does not exist at' $CBP_EXPORT_DIR/land/$scenario_name/$dataset
fi
fi

