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
Rscript $CBP_ROOT/run/resegment/area_propor.R $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv $subshed $downstream $darea
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
    Rscript $CBP_ROOT/run/resegment/area_propor.R $lu_file $subshed $downstream $darea

    # reset our counter
    cnt=0
  fi
done
echo 'land use files proportioned'

# duplicate information from downstream to new subshed:

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/wF180615RXAPXXXW_l2w.csv $subshed $downstream
echo 'wF180615RXAPXXXW_l2w.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/wF180615RXAPXXXW_s2r.csv $subshed $downstream
echo 'wF180615RXAPXXXW_s2r.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/wF180615RXAPXXXW_res.csv $subshed $downstream
echo 'wF180615RXAPXXXW_res.csv duplicated'

	#there may be more transport files that need to be duplicated

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv $subshed $downstream
echo 'gen_info_rseg.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_met_wdm.csv $subshed $downstream
echo 'river_met_wdm.csv duplicated'

Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_prad_wdm.csv $subshed $downstream
echo 'river_prad_wdm.csv duplicated'
