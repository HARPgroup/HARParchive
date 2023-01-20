# arguments:
# - hydrocode
# - receiving stream segment
# - drainage area of subshed
# - model version (cbp-6.0)
# - cbp scenario (subsheds)
# load name, rseg and area info

hydrocode=$1
downstream=$2
model_version=$3
scenario=$4

# get info
GEO=`cbp get_config subsheds river GEO`

# retrieve drainage area for our subshed
read -r darea <<< "$(Rscript /opt/model/p6/vadeq/run/resegment/get_subsheds_riverseg.R /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/)"
  echo'subshed drainage area:' $darea

# name subshed or retrieve the name if it already exists
read -r subshed downstream <<< "$(Rscript /opt/model/p6/vadeq/run/resegment/subsheds_naming.R $hydrocode /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/rivernames.csv cbp-6.0)"
  echo'new subshed:' $subshed

# set and proportion watershed area
Rscript /opt/model/p6/vadeq/run/resegment/area_propor.R /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/land_water_area.csv $subshed $downstream $darea
  echo'land_water_area.csv proportioned'

# set land use area; iterate through multiple files and proportion them all
cnt=0
for i in $LANDUSE; do
  ((cnt++))
  if [ $cnt eq 4 ]; then
    lu_file="land_use_${i}.csv"
    echo $lu_file
    Rscript /opt/model/p6/vadeq/run/resegment/area_propor.R /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/$lu_file $subshed $downstream $darea
    # reset our counter
    cnt=0
  fi
end
  echo'land use files proportioned'

# duplicate information from downstream to new subshed
Rscript /opt/model/p6/vadeq/run/resegment/copy_parent.R /opt/model/p6/vadeq/input/param/transport/wF180615RXAPXXXW_l2w.csv $subshed $downstream
  echo'wF180615RXAPXXXW_l2w.csv duplicated'

Rscript /opt/model/p6/vadeq/run/resegment/copy_parent.R /opt/model/p6/vadeq/input/param/river/vahydro_2022/gen_info_rseg.csv $subshed $downstream
 echo'gen_info_rseg.csv duplicated'

Rscript /opt/model/p6/vadeq/run/resegment/copy_parent.R /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/river_met_wdm.csv $subshed $downstream
 echo'river_met_wdm.csv duplicated'

Rscript /opt/model/p6/vadeq/run/resegment/copy_parent.R /opt/model/p6/vadeq/config/catalog/${GEO}/vahydro/river_prad_wdm.csv $subshed $downstream
 echo'river_prad_wdm.csv duplicated'