library(data.table)
library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R') 
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_download_read.R"),local = TRUE) #load function for downloading & reading zip/shp files by url
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fns_spatial.R"),local = TRUE) #load functions for dealing with spatial data

############################################################### #
# inputs
############################################################### #
crs_default <- 4326
runid_list <- c("runid_11","runid_13")
model_version <- c("vahydro-1.0")

#rivseg <- "PU6_3730_3750"  << these are now combined into one variable 'origin' to describe what
#locality <- "Fauquier"         area the map will be centered around, and the variable 'origin_type'
#region <- "NA"                 will denote whether this origin is a rivseg, locality, or region
origin <- "JL7_7070_0001"
origin_type <- "basin"
featr_type <- "facility"
limit_featrs_to_origin <- FALSE #if TRUE -> featrs will be cutoff at the region/locality specified
                                  #if FALSE --> all featrs in the associated basins will be plotted
metric_mod <- c("wd_mgd")
metric_feat <- c("wsp2020_2040_mgy")
rivseg_metric <- c("l30_Qout","7q10")

base_layer_data <- FALSE #if FALSE -> will only generate the origin/metric-dependent data for mapping (rsegs, featrs)
                           #if TRUE -> will also re-generate map base-layer data (regions, counties, cities, roads)

############################################################### #

############################################################### #

#----Pull Data----
#---Facility Data---
#note: the variable "github_location" should be in config.local and provides easy access to these resources
foundatn_mp <- fread(paste0(github_location, "/Foundational_Data/2023/foundation_dataset_mgy_1982-2022_expanded.csv")) #foundational measuring pt(mp)/sources data

if (featr_type=="facility") { #specified model metrics will be pulled @ the facility-level for every specified runid using om_vahydro_metric_grid()
  df <- data.frame(runid=runid_list, model_version, metric=metric_mod) #create df of model run specifications for om_vahydro_metric_grid()
  for(i in 1:length(runid_list)){ #add column to df containing 'runlabel' which will become the metric column names in featrs$model
    df$runlabel[i] <- paste0(runid_list[i], '_', metric_mod)
  }
  #pull facilities w/ metric of interest from vahydro:
  f_model <- om_vahydro_metric_grid(
    metric=FALSE, runids=df, featureid='all', 
    entity_type='dh_feature', bundle='facility',
    ftype='all', model_version=model_version,
    base_url=paste(site,"/entity-model-prop-level-export",sep=''), #http://deq1.bse.vt.edu/d.dh
    ds=ds
  )
  #pull facility-level geometry to join with model data:
  f_geo <- fread(paste0(github_location, "/Foundational_Data/2023/facilities_all_geom.csv"))
} 
#---Watershed/Riverseg Data---
#rsegs <- ds$get('dh_feature', config=list(ftype='vahydro',bundle='watershed')) ## VAhydro gives errors
rsegs <- fn_download_read(url=paste(site,"/vahydro_riversegs_export",sep=""), filetype="csv", zip=FALSE) 
  #pulls csv with All vahydro watershed features
  #note: potential NULLs for newly carved data^

#---County Data---
cnty_fips <- ds$get('dh_feature', config=list(bundle='usafips')) #pull all counties from VAhydro
cnty_regions <- fread('https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/Regions_ProposedReg_053122.csv') #csv connects county names to their planning regions

#---Cities & Roads---
if(base_layer_data==TRUE){
  roads <- fn_download_read(
    url="https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/tl_2022_51_prisecroads.zip", 
    filetype="shp", zip=TRUE) #.shp file for US states & primary roads
  
  cities <- fread('https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/USA_Major_Cities_GIS.csv')
}

#----Filter/Process Data----
#---Counties---
cnty_fips$name <- sub(" County", "", cnty_fips$name) # fix any names followed by " County" to match the names of counties in the regions df
cnty_fips <- sqldf("SELECT a.*, b.VMDWA_Reg2 as Region
                FROM cnty_fips as a
                LEFT OUTER JOIN cnty_regions as b
                WHERE (a.name = b.County)
                ") #add region column to county data; WHERE instead of ON means only counties in regional planning areas are kept
counties <- cnty_fips[grep(51,cnty_fips$dh_fips),] #remove counties outside of VA using the fips code
counties <- sf::st_as_sf(counties, wkt = fn_geoCol(counties), crs=crs_default) #convert to sf based on geometry column found by fn_geoCol() (developer-defined fn from fns_spatial.R)

#---Regions---
regions_split <- split(counties, counties$Region) #creates a list containing a sf data frame per each region, which all contain the county polygons corresponding to that region
for(i in 1:length(regions_split)){ #merge counties into one polygon for each region
  if(i==1){
    regions <- st_union(regions_split[[i]])
  }
  if(i!=1){
    regions <- rbind( regions, st_union(regions_split[[i]]) )
  } #note: had to do this in IF statements instead of resetting regions_split[i] <- regions_split[[i]] because keeping it in list form causes st_union() to put geom into sfg format, which causes problems with st_filter() in the rsegs filtering chunk
}
regions <- sf::st_as_sf(data.frame(region=names(regions_split), geo=regions, row.names=NULL), crs=crs_default)
rm(regions_split)

if(base_layer_data==TRUE){
#---Cities---
  statemt <- paste("SELECT cities.X, cities.Y, cities.NAME, cities.POPULATION,",
                   "CASE WHEN POPULATION BETWEEN",quantile(cities$POPULATION, 0.1),"AND", quantile(cities$POPULATION, 0.5),
                   "THEN 'smallTown' 
                  WHEN POPULATION BETWEEN",quantile(cities$POPULATION, 0.5),"AND",quantile(cities$POPULATION, 0.8),
                   "THEN 'town'
                  WHEN POPULATION BETWEEN",quantile(cities$POPULATION, 0.8),"AND",quantile(cities$POPULATION, 1.0),
                   "THEN 'city'
                  ELSE CLASS", 
                   "END as CLASS",
                   "FROM cities
                  WHERE NAME 
                  NOT IN (select counties.name from counties)",
                   "AND (cities.ST == 'VA') ", 
                   "ORDER BY POPULATION DESC", sep=" ") 
  cities <- fn_sqldf_sf(statemt)
#---Roads---
  roads <- subset(roads, MTFCC=="S1100" & (RTTYP=="I"|RTTYP=="U"|RTTYP=="S") #primary roads & interstate, US Hwy, or State Rte only
                  & FULLNAME %in% grep("([0-9]+).$", roads$FULLNAME, value=TRUE)) #finds where last char is a number (thus omitting names followed by Byp, Alt, etc.)
  roads$FULLNAME <- gsub(".* ", "", roads$FULLNAME) #removes text and spaces before route number
  names(roads) <- gsub("RTTYP", "CLASS", names(roads)) #re-name class column -> needed for fn_labelprep() in the RMD
}

#---Facilities/Rsegs---
if(featr_type == "source"){
  featrs <- foundatn_mp[foundatn_mp$Latitude!="" & foundatn_mp$Longitude!="" 
                          & !is.na(foundatn_mp$Latitude) & !is.na(foundatn_mp$Longitude) 
                      ,] #omits facilities with blank or NA geometry
  featrs <- sf::st_as_sf(featrs, coords=c("Longitude","Latitude"), crs=crs_default) #convert to sf
}
if(featr_type == "facility"){ #get facility-level fiveyr_avg_mgy from foundatn_mp by summing all mp values for each facility:
  f_foundatn <- sqldf("select foundatn_mp.*,
                    sum(five_yr_avg) as sum
                    from foundatn_mp
                    group by Facility_hydroid
                  ") #all source-related data now only applies to 1 source (random) within the facil 
  f_model <- sqldf("SELECT * FROM f_model WHERE hydrocode not like 'wsp_%'") #filter out WSP entries from facility-level model metric data
  f_merge <- merge(x=f_model, #merge/full join foundational & modeled facil data
                   y=f_foundatn[names(f_foundatn)!="Hydrocode"], #all but the Hydrocode column bc it's a duplicate. Needed for SQLDF
                   by.x="featureid", by.y="Facility_hydroid", all=T
                   )
  #join facility geometry to merged data frame:
  featrs <- sqldf(paste("SELECT", #selects/renames desired columns from f_merge, matches them with geometry from f_geo based on featureid
                        " a.Facility, a.featureid as Facility_hydroid, a.Use_Type, a.Locality, 
                      a.sum as five_yr_avg, a.wsp_2040_mgy, a.Permit_Limit, a.",df$runlabel[1],", a.",df$runlabel[2],",
                      b.",fn_geoCol(f_geo), #robust for varying geometry column names
                        " FROM f_merge as a
                      LEFT OUTER JOIN f_geo as b
                      ON (a.featureid = b.Facility_hydroid)", 
                    sep="")
                  )
  featrs <- featrs[featrs[,fn_geoCol(featrs)]!="" & !is.na(featrs[,fn_geoCol(featrs)]),] #omits facilities with blank or NA geometry
  featrs <- sf::st_as_sf(featrs, wkt = fn_geoCol(featrs), crs=crs_default) #convert to sf
}
#connect facilities to the watersheds they are in:
rsegs$riverseg <- gsub(pattern="vahydrosw_wshed_", replacement="", rsegs$hydrocode) #prereq. for fn_extract_basin() & desired for featrs/table riverseg column
rsegs <- rsegs[ rsegs[,fn_geoCol(rsegs)]!="" & !is.na(rsegs[,fn_geoCol(rsegs)]) ,] #finds geom column & omits rsegs with blank or NA geometry
rsegs <- sf::st_as_sf(rsegs, wkt=fn_geoCol(rsegs), crs=crs_default) #convert to sf
sf::sf_use_s2(FALSE) # switch off Spherical geometry ; some functions (eg. st_join, st_filter) give errors without this
featrs <- sf::st_join(featrs, rsegs[ ,c("riverseg","hydroid",fn_geoCol(rsegs)) ]) #pairs riverseg column from rsegs w/ featrs based on geometry
#note: we add an riverseg column via geometry in both source & facility cases b/c even when facilities come in with a riverseg column, many are blank

#filter by boundary type:
#---Rsegs---
if (origin_type=="basin") { #finding upstream riversegs for basin maps 
  for(i in origin){
    if(i==origin[1]){
      basin <- fn_extract_basin(st_drop_geometry(rsegs), i) #fn_extract_basin runs sqldf which can't handle sf geometry
    }
    if(i!=origin[1]){
      basin <- rbind(basin, fn_extract_basin(st_drop_geometry(rsegs), i))
    }
  }
  rsegs <- st_as_sf( merge(x=basin,y=rsegs), crs=crs_default) #add the geometries back on
  rsegs <- unique(rsegs) #don't duplicate riversegs for overlapping basins
  rm(basin)
} else if (origin_type=="locality") { #for locality level
  rsegs <- st_filter(rsegs, counties[counties$name==origin, ]) #filter basins by locality (if REST is working)
} else if (origin_type=="region") { #for region level
  rsegs <- st_filter(rsegs, regions[regions$region==origin,]) #filter basins by the region given in params
}
#---Sources/featrs---
#filtering data to only points within the extent of interest, either locality/region boundary or the basins/riversegs intersecting the extent
if (limit_featrs_to_origin==FALSE | origin_type=="basin") {
  featrs <- st_filter(featrs, rsegs)
} else if (limit_featrs_to_origin==TRUE){
  if(origin_type=="locality"){
    featrs <- st_filter(featrs, counties[counties$name==origin, ])
  } else if (origin_type=="region") {
    featrs <- st_filter(featrs, regions[regions$region==origin,])
  }
}

#----Additional Pulling Post-Filtering----
#---User-Input Non-Modeled Feature Metrics---
for (i in 1:nrow(featrs)) {
  featrs[i,metric_feat] <- RomProperty$new(ds,list(
    featureid = featrs$Facility_hydroid[[1]][i], #[[1]] b/c we want column value w/out attached geometry
    propname = metric_feat),
    TRUE)$propvalue #pull feature & directly assign metric propvalue to facility i
}

#---Permitted Capacity--- #taken out b/c foundational data has this now ?
fac_model_data <- data.frame()
for (i in unique(featrs$hydroid) ){
  #i is a rseg hydroid corresponding to each facility, but it won't pull for the same hydroid twice b/c that is redundant and time-consuming
  model_props <- c("vwp_max_mgy","permit_status")
  fac_model_data <- rbind( fac_model_data , read.csv(paste(site,"/model-summary-users-export-all-cols/",
                                                           gsub(" ","",toString( i )),"/",
                                                           runid_list[1],"/", #perm.cap doesn't change w/ runid ?
                                                           gsub(" ","",toString( metric_mod )),"/", #user's model run metric
                                                           gsub(" ","",toString( model_props )),sep="")
  ))
}
statemt <- paste("SELECT a.*, z.vwp_max_mgy, z.permit_status
                  FROM featrs as a
                  LEFT OUTER JOIN
              (   SELECT c.facility_hydroid, b.permit_status,
                  CASE WHEN b.exempts == 'exempt'
                    THEN b.exempts
                  ELSE c.permcaps
                  END as vwp_max_mgy ", #when status is exempt, makes permitted capacity exempt too
                 "FROM
                 ( (SELECT facility_hydroid, model_prop_propcode as permit_status,
                      CASE WHEN model_prop_propcode == 'exempt'
                        THEN 'exempt'
                      ELSE 'switch'
                      END as exempts
                    FROM fac_model_data
                    WHERE (model_prop_propname == 'permit_status')
                    ) as b
                  LEFT OUTER JOIN (
                    SELECT facility_hydroid, model_prop_propcode,
                      CASE WHEN model_prop_propcode IS NULL OR model_prop_propcode == 0
                        THEN 'No Permit' ", #changes null/0 permit capacity to 'No Permit'
                 "ELSE model_prop_propcode
                      END as permcaps
                    FROM fac_model_data
                    WHERE (model_prop_propname == 'vwp_max_mgy')
                    ) as c
                  ON (b.facility_hydroid = c.facility_hydroid)
                 )
              ) as z
              ON (a.Facility_hydroid = z.facility_hydroid)
                 ", sep='')
featrs <- fn_sqldf_sf(statemt, geomback="featrs")
featrs <- unique(featrs) #remove duplicated rows
featrs$vwp_max_mgy[is.na(featrs$vwp_max_mgy)] <- "No Permit" #replace remaining NA w/ 'No Permit'; !! figure out why NAs still exist
rm(fac_model_data)

#---Pull Rseg Drought Metrics---
for (k in 1:length(rivseg_metric)) {
  for (j in 1:length(runid_list)) {
    for (i in 1:nrow(rsegs)) {
      riverseg <- RomFeature$new(ds,list( #get riverseg feature from vahydro
        hydrocode = paste('vahydrosw_wshed_',rsegs$riverseg[i],sep=''),
        ftype = 'vahydro',
        bundle = 'watershed'
      ),TRUE)
      
      if (!is.na(riverseg$hydroid)) { #only continue if rivseg feature was found
        model <- RomProperty$new(ds,list( #get vahydro-1.0 model feature from vahydro
          featureid = riverseg$hydroid,
          propcode = 'vahydro-1.0'
        ),TRUE)
        
        model_scenario <- RomProperty$new(ds,list( #get scenario/runid from vahydro
          varkey = "om_scenario",
          featureid = model$pid,
          propname = runid_list[j]
        ),TRUE)
        
        if (!is.na(model_scenario$pid)) { #only continue if runid was found (scenario pid!=NA)
          rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- RomProperty$new(ds,list( #get metric from vahydro
            featureid = model_scenario$pid,
            entity_type = 'dh_properties',
            propname = rivseg_metric[k]
          ),TRUE)$propvalue #directly assign metric propvalue
        } else { #the scenario/runid wasn't found
          rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- NA
        }
      } else { #the rivseg feature wasn't found
        rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- NA
      }
    }
  }
}
rm(riverseg)
rm(model)
rm(model_scenario)

#----Calculate Rseg Metric % Diff----
for (k in 1:length(rivseg_metric)){
  colname1 <- paste0(runid_list[1],'_',rivseg_metric[k])
  colname2 <- paste0(runid_list[2],'_',rivseg_metric[k])
  
  statemt <- paste("SELECT rsegs.*,
                  CASE WHEN (",colname2," - ",colname1,")==0
                    THEN 0 ", # 0/0 is NA so when difference is 0, %diff is 0
                   "ELSE ( (",colname2," - ",colname1,") / ",colname1," * 100) ", #calculate %diff as usual
                   "END as percentDiff_",rivseg_metric[k], #creates % diff. column
                   " FROM rsegs
                 ",sep="") #!! need a case for when colname1 is zero but colname2 isn't ?
  rsegs <- fn_sqldf_sf(statemt, geomback="rsegs")
}

#----Write Files----
st_write(rsegs, paste0(export_path,origin,"_rsegs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
if(featr_type=="facility"){
  st_write(featrs, paste0(export_path,origin,"_featrs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
}
if(featr_type=="source"){
  featrs <- featrs[names(featrs) %in% grep("^([0-9]+).$", names(featrs), value=TRUE, invert=TRUE)] #get rid of all those year columns
  st_write(featrs, paste0(export_path,origin,"_mp_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
}
if(base_layer_data==TRUE){
  st_write(counties, paste0(export_path,"counties_sf.csv"), layer_options="GEOMETRY=AS_WKT")
  st_write(regions, paste0(export_path,"regions_sf.csv"), layer_options="GEOMETRY=AS_WKT")
  st_write(roads, paste0(export_path,"roads_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
  write.csv(cities, paste0(export_path,"cities.csv"))
}
