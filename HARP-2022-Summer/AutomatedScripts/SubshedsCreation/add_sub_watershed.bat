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
TRANSPORT=`cbp get_config $scenario river TRANSPORT`
PARAMS=`cbp get_config $scenario river PARAMETERS`
LANDUSE=`cbp get_config $scenario river 'LAND USE'`
DIV=`cbp get_config $scenario river DIV`
SEPTIC=`cbp get_config $scenario river SEPTIC`
RIB=`cbp get_config $scenario river 'RIB LOADS'`
RPA=`cbp get_config $scenario river 'RPA LOADS'`
# name subshed or retrieve the name if it already exists
read -r subshed downstream <<< "$(Rscript $CBP_ROOT/run/resegment/subsheds_naming.R $hydrocode $CBP_ROOT/config/catalog/geo/${GEO}/rivernames.csv $model_version)"
echo 'new subshed:' $subshed

# set and proportion watershed area
Rscript $CBP_ROOT/run/resegment/area_propor.R $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv $subshed $downstream $darea
echo 'land_water_area.csv proportioned'

# now get a list of subwatersheds for later use
read -r -a ss_pieces <<< $(echo $subshed | tr "_" " ")
seg_id="${ss_pieces[1]}"
cbp basingen.csh $scenario $seg_id
landsegs=`cbp get_landsegs $subshed`

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

# Transport
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/${TRANSPORT}_l2w.csv $subshed $downstream
echo '${TRANSPORT}_l2w.csv duplicated'
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/${TRANSPORT}_s2r.csv $subshed $downstream
echo '${TRANSPORT}_s2r.csv duplicated'

# Hydro PARAMETERS
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv $subshed $downstream
echo 'gen_info_rseg.csv duplicated'

# we need to save the header for HYDR, 'ADCALC and SCRORG cause they're real screwed up with duplicate col names
# not only that, but SCRORN has 2 header columns, and the others have 1
for fname in "ADCALC" "HYDR"; do
  head -n 1 "$CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv" > tmp.header.txt
  echo "Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv $subshed $downstream"
  Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv $subshed $downstream
  ( head -1 tmp.header.txt; tail -n +2 $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv ) > ${fname}.tmp.csv
  mv  ${fname}.tmp.csv $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv
  echo '${fname}.csv duplicated'
done
for fname in "SCRORG"; do
  head -n 2 "$CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv" > tmp.header.txt
  Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv $subshed $downstream
  ( head -2 tmp.header.txt; tail -n +2 $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv ) > ${fname}.tmp.csv
  mv  ${fname}.tmp.csv $CBP_ROOT/input/param/river/${PARAMS}/${fname}.csv
  echo '${fname}.csv duplicated'
done


# WDM CSVs
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_met_wdm.csv $subshed $downstream
echo 'river_met_wdm.csv duplicated'
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/config/catalog/geo/${GEO}/river_prad_wdm.csv $subshed $downstream
echo 'river_prad_wdm.csv duplicated'

# WDM files 
cp config/blank_wdm/blank_ps_sep_div.wdm input/scenario/river/div/${DIV}/DIV_${subshed}.wdm

# this may not be needed, as these don't appear in the UCI but might be used in the ETM?
# make this var so it fits on one line and is readable
xpath="$CBP_ROOT/input/scenario/river"
for lseg in $landsegs; do
  cp $xpath/septic/${SEPTIC}/sep_${lseg}_to_${downstream}.wdm $xpath/septic/${SEPTIC}/sep_${lseg}_to_${subshed}.wdm
  cp $xpath/rib/${RIB}/rib_${lseg}_to_${downstream}.wdm $xpath/rib/${RIB}/rib_${lseg}_to_${subshed}.wdm
  cp $xpath/rpaload/${RPA}/rpa_${lseg}_to_${downstream}.wdm $xpath/rpaload/${RPA}/rpa_${lseg}_to_${subshed}.wdm
done
