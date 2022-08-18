#!/bin/bash
#this script should be run from directory: /opt/model/p53/p532c-sova
#support for arguments:
if [ $# -ne 4 ] && [ $# -ne 3 ]; then
      echo ' '
      echo 'usage:  locate_uci.sh river scenario riverseg'
      echo ' or     locate_uci.sh land scenario landseg landuse'
      echo ' '
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
uci=$CBP_ROOT/tmp/uci/$segvar/$scenario_name/$riverseg'.uci'
if  [ -f $uci ]; then
echo 'River seg uci:' $uci
fi
if  [ ! -f $uci ]; then
echo 'River seg uci does not exist at' $CBP_ROOT/tmp/uci/$segvar/$scenario_name/ ; fi
fi

if [ "$segvar" == "land" ]; then
uci_land=$CBP_ROOT/tmp/uci/$segvar/$landuse/$scenario_name/$landuse$landseg'.uci'
if  [ -f $uci_land ]; then
echo 'Land seg uci:' $uci_land
fi
if  [ ! -f $uci_land ]; then
echo 'Land seg uci does not exist at' $CBP_ROOT/tmp/uci/$segvar/$landuse/$scenario_name/ ; fi
fi
