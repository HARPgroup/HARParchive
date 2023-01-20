#!/bin/bash
if [ $# -lt 5 ]; then
  echo "Usage: add_sub_watershed.bat hydrocode downstream_seg model_version scenario drainage_area"
  exit
fi
. hspf_config
# arguments:
# - hydrocode
# - receiving stream segment
# - model version (cbp-6.0)
# - cbp scenario (subsheds)
# - drainage area of subshed
# load name, rseg and area info

hydrocode=$1
downstream=$2
model_version=$3
scenario=$4
darea=$5

# get info
GEO=`cbp get_config $scenario river GEO`
PARAMS=`cbp get_config $scenario river PARAMETERS`
LANDUSE=`cbp get_config $scenario river 'LAND USE'`
# name subshed or retrieve the name if it already exists
read -r subshed downstream <<< "$(Rscript $CBP_ROOT/run/resegment/subsheds_naming.R $hydrocode $CBP_ROOT/config/catalog/geo/${GEO}/rivernames.csv cbp-6.0)"
echo 'new subshed:' $subshed

# set and proportion watershed area
# temp commented out waiting for fix
#Rscript $CBP_ROOT/run/resegment/area_propor.R $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv $subshed $downstream $darea
# use in place temporarily, note argument order is different
echo "Rscript $CBP_ROOT/run/resegment/proportion_landuse.R $downstream $subshed $darea $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv "
Rscript $CBP_ROOT/run/resegment/proportion_landuse.R $downstream $subshed $darea $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv 
echo 'land_water_area.csv proportioned'

# set land use area; iterate through multiple files and proportion them all
cnt=0
for i in $LANDUSE; do
  ((cnt++))
  if [[ $cnt -eq 1 ]]; then
    yr=$i
  fi
  if [[ $cnt -eq 4 ]]; then
    lu_file="input/scenario/river/land_use/land_use_${i}.csv"
    echo "LU file for $yr = " $lu_file
    # until this is fixed:
    #Rscript $CBP_ROOT/run/resegment/area_propor.R $lu_file $subshed $downstream $darea
    # use this instead:
    echo "Rscript $CBP_ROOT/run/resegment/proportion_landuse.R $downstream $subshed $darea $lu_file"
    Rscript $CBP_ROOT/run/resegment/proportion_landuse.R $downstream $subshed $darea $lu_file

    # reset our counter
    cnt=0
  fi
done
echo 'land use files proportioned'

# duplicate information from downstream to new subshed
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/wF180615RXAPXXXW_l2w.csv $subshed $downstream
echo 'wF180615RXAPXXXW_l2w.csv duplicated'
# How many transport files need to be duplicated - do we need a loop like above, or just run them one-by-one?

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv $subshed $downstream
echo 'gen_info_rseg.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_met_wdm.csv $subshed $downstream
echo 'river_met_wdm.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_prad_wdm.csv $subshed $downstream
echo 'river_prad_wdm.csv duplicated'
