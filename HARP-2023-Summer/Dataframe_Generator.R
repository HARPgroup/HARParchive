library(data.table)
library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R') 
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_download_read.R"),local = TRUE)
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_process_geom.R"),local = TRUE) 
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_mapgen_est.R"),local = TRUE) #load mapping function
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/mapstyle_config.R"),local = TRUE) #load mapping aesthetics
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/rivsegmaps_config.R"),local = TRUE) #load rivseg-specific mapping aesthetics
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_labelprep.R"),local = TRUE) #load labeling function

################################################################
# inputs
################################################################
crs_default <- 4326
runid_list <- c("runid_11","runid_13")
model_version <- c("vahydro-1.0")
metric_mod <- c("wd_mgd")
metric_feat <- c("fiveyr_avg_mgy")
rivseg <- "PU6_3730_3750"
rivseg_metric <- c("l30_Qout","7q10")
locality <- "Fauquier"
region <- "NA"   
map_type <- "locality"
limit <- "basins"
#type <- "facility"
type <- "source"
################################################################
################################################################

# returns the name of the geometry column in the data:
geoCol <- function(data){
  colname <- grep("geo",colnames(data),value=TRUE)
  if(length(colname) > 1){
    colname <- grep("geom",colname,value=TRUE) #for the case of "dh_geofield.geom" "dh_geofield.geo_type" "dh_geofield.lat" "dh_geofield.lon", etc.
  }
  return(colname)
}

# allows the usage of SQLDF with sf data frames:
sqldf_sf <- function(statemt, geomback="NA"){
  #statement is any SQLDF string; reference the data frames per usual
  #geomback is the character name of the data.frame you are filtering, where, if applicable, the sf geometry column will need to be added back from
  dfs <- as.environment(as.list(.GlobalEnv, all.names=TRUE)) #make a copy of the global environment
  for(i in names(dfs)){
    if("sf" %in% class(dfs[[i]])){ #drop the geometry on any sf objects so they can go through SQLDF
      dfs[[i]] <- sf::st_drop_geometry( dfs[[i]] )
    }
  }
  dfs[["result"]] <- sqldf(statemt, envir=dfs) #SQLDF the non-sf dataframes in the new environment (so that the global envir. retains the sf objects)
  if(geomback!="NA"){
    output <- merge(x=dfs[["result"]], y=.GlobalEnv[[geomback]], all.x=TRUE, all.y=FALSE) #match the newly filtered obs. to their geometries
    output <- st_as_sf(output, crs=crs_default) #convert back to sf
  } else {
    output <- dfs[["result"]]
  }
}

#```
##############################################################################
##############################################################################
# the following will no longer needed, since the input dataframe will either be facility or MP-level data
#determining which type/level of map is being created (either 'source' or 'facility')
#type <- "facility"
#type <- "source"

## Pull Data (Cleanup)
#```{r Pull Data, echo=FALSE, message=FALSE, warning=FALSE}
#----Facility Data----
facils <- list() #create empty list to store dfs
# note: the variable "github_location" should be in config.local and provides easy access to these resources

# foundational MP(measuring pt)/sources data:
facils$foundatn_mp <- fread(paste0(github_location, "/Foundational_Data/2023/foundation_dataset_mgy_2018-2022_5ya.csv"))
# write.csv(facils$foundatn_mp, paste0(export_path,"00_foundatn_mp.csv"))

if (type=="facility") { 
  #specified model metrics will be pulled @ the facility-level for every specified runid using om_vahydro_metric_grid()
  #create df of model run specifications for om_vahydro_metric_grid():
  df <- data.frame(runid=runid_list, model_version, metric=metric_mod) 
  # write.csv(df, paste0(export_path,"01_df.csv"))
  #add column to df containing 'runlabel' which will become the metric column names in facils$model:
  for(i in 1:length(runid_list)){ 
    df$runlabel[i] <- paste0(runid_list[i], '_', metric_mod)
  }
  # write.csv(df, paste0(export_path,"02_df.csv"))
  
  #pull facilities w/ metric of interest from vahydro:
  facils$model <- om_vahydro_metric_grid(
    metric=FALSE, runids=df, featureid='all', 
    entity_type='dh_feature', bundle='facility',
    ftype='all', model_version=model_version,
    base_url=paste(site,"/entity-model-prop-level-export",sep=''), #http://deq1.bse.vt.edu/d.dh
    ds=ds
  )
  # write.csv(facils$model, paste0(export_path,"03_model.csv"))
  #pull facility-level geometry to join with model data:
  facils$fac_geo <- fread(paste0(github_location, "/Foundational_Data/2023/facilities_all_geom.csv"))
  # write.csv(facils$fac_geo, paste0(export_path,"04_fac_geo.csv"))
} 
#----Watershed/Riverseg Data----
#rsegs <- ds$get('dh_feature', config=list(ftype='vahydro',bundle='watershed')) ## VAhydro gives errors
if(!exists("rsegs")){
  rsegs <- fn_download_read(url=paste(site,"/vahydro_riversegs_export",sep=""), filetype="csv", zip=FALSE) 
  # pulls csv with All vahydro watershed features
  # note: potential NULLs for newly carved data^
}
# write.csv(rsegs, paste0(export_path,"05_rsegs.csv"))
#----County Data----
counties <- list()
counties$fips <- ds$get('dh_feature', config=list(bundle='usafips')) #pull all counties from VAhydro
counties$regions <- fread('https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/Regions_ProposedReg_053122.csv') #csv connects county names to their planning regions
# write.csv(counties$fips, paste0(export_path,"06_fips.csv"))
# write.csv(counties$regions, paste0(export_path,"07_regions.csv"))

#----Cities & Roads----
roads <- fn_download_read(
  url="https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/tl_2022_51_prisecroads.zip", 
  filetype="shp", zip=TRUE) # (shp) for US states & primary roads
# st_write(roads, paste0(export_path,"08_roads_sf.csv"), layer_options = "GEOMETRY=AS_WKT")

cities <- fread('https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/USA_Major_Cities_GIS.csv')
# write.csv(cities, paste0(export_path,"09_cities.csv"))
#```

## Filter/Process Data (Cleanup)
#```{r Processing Counties & Regions, echo=FALSE, message=FALSE, warning=FALSE}
#----Counties----
counties$fips$name <- sub(" County", "", counties$fips$name) # fix any names followed by " County" to match the names in counties$regions
counties$fips <- with(counties, sqldf("SELECT a.*, b.VMDWA_Reg2 as Region
                                      FROM fips as a
                                      LEFT OUTER JOIN regions as b
                                      WHERE (a.name = b.County)
                                      ")) #add region column to county data; WHERE instead of ON means only counties in regional planning areas are kept
# write.csv(counties$fips, paste0(export_path,"10_fips.csv"))

#remove counties outside of VA using the fips code; save over as "counties" to simplify data handling:
counties <- counties$fips[grep(51,counties$fips$dh_fips),]
counties <- st_as_sf(counties, wkt = geoCol(counties), crs=crs_default) #convert to sf based on geometry column found by geoCol() (developer-defined fn above)
# st_write(counties, paste0(export_path,"11_counties_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
st_write(counties, paste0(export_path,rivseg,"_counties_sf.csv"), layer_options = "GEOMETRY=AS_WKT")


#----Regions----
regions_split <- split(counties, counties$Region) #creates a list containing a sf data frame per each region, which all contain the county polygons corresponding to that region
for(i in 1:length(regions_split)){ #merge counties into one polygon for each region
  if(i==1){
    regions <- st_union(regions_split[[i]])
  }
  if(i!=1){
    regions <- rbind( regions, st_union(regions_split[[i]]) )
  } #note: had to do this in IF statements instead of resetting regions_split[i] <- regions_split[[i]] because keeping it in list form causes st_union() to put geom into sfg format, which causes problems with st_filter() in the rsegs filtering chunk
}
regions <- st_as_sf(data.frame(region=names(regions_split),geo=regions,row.names=NULL), crs=crs_default)
# write.csv(regions, paste0(export_path,"12_regions.csv"))
# st_write(regions, paste0(export_path,"12_regions_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
 st_write(regions, paste0(export_path,"regions_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
rm(regions_split)
#```

#```{r Merging Facility Data, echo=FALSE, message=FALSE, warning=FALSE}
if(type == "source"){
  facils <- with(facils, 
                 foundatn_mp[foundatn_mp$Latitude!="" & foundatn_mp$Longitude!="" 
                             & !is.na(foundatn_mp$Latitude) & !is.na(foundatn_mp$Longitude) ,] #omits facilities with blank or NA geometry
  )
  facils <- st_as_sf(facils, coords=c("Longitude","Latitude"), crs=crs_default) #convert to sf
  # write.csv(facils, paste0(export_path,"13_facils.csv"))
}
if(type == "facility"){
  #get facility-level fiveyr_avg_mgy from foundatn_mp by summing all mp values for each facility:
  facils$foundatn <- with(facils, sqldf("select foundatn_mp.*,
                    sum(fiveyr_avg_mgy) as sum
                    from foundatn_mp
                    group by Facility_hydroid")) #all source-related data now only applies to 1 source (random) within the facil 
  # write.csv(facils$foundatn, paste0(export_path,"13_foundatn.csv"))
  
  #filter out WSP entries from facility-level metric data:
  facils$model <- with(facils, sqldf("SELECT * 
                                      FROM model 
                                      WHERE hydrocode not like 'wsp_%'"))
  # write.csv(facils$model, paste0(export_path,"14_model.csv"))
  
  #merge/full join foundational & modeled facil data:
  facils$merge <- with(facils,merge(x=model, 
                                    y=foundatn[names(foundatn)!="Hydrocode"], #all but the Hydrocode column bc it's a duplicate. Needed for SQLDF
                                    by.x="featureid", by.y="Facility_hydroid", all=T)) 
  # write.csv(facils$merge, paste0(export_path,"15_merge.csv"))
  
  #join facility geometry to merged data frame:
  statemt <- paste("SELECT 
                      a.Facility, a.featureid as Facility_hydroid, a.'Use Type', a.Locality, 
                      a.sum as fiveyr_avg_mgy, a.",df$runlabel[1],", a.",df$runlabel[2],",
                      b.",geoCol(facils$fac_geo), #robust for varying geometry column names
                   " FROM merge as a
                      LEFT OUTER JOIN fac_geo as b
                      ON (a.featureid = b.Facility_hydroid)", sep="")
  facils <- with(facils, sqldf(statemt)) #selects/renames desired columns from facils$merge, matches them with geometry from facils$fac_geo based on featureid ; saves over facils to simplify data access/storage
  facils <- facils[facils[,geoCol(facils)]!="" & !is.na(facils[,geoCol(facils)]),] #omits facilities with blank or NA geometry
  facils <- st_as_sf(facils, wkt = geoCol(facils), crs=crs_default) #convert to sf
  # st_write(facils, paste0(export_path,"16_facils_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
}
#connect facilities to the watersheds they are in:
rsegs$riverseg <- gsub(pattern="vahydrosw_wshed_", replacement="", rsegs$hydrocode) #prereq. for fn_extract_basin() & desired for facils/table riverseg column
rsegs <- rsegs[ rsegs[,geoCol(rsegs)]!="" & !is.na(rsegs[,geoCol(rsegs)]) ,] #finds geom column & omits rsegs with blank or NA geometry
rsegs <- sf::st_as_sf(rsegs, wkt=geoCol(rsegs), crs=crs_default) #convert to sf
# st_write(rsegs, paste0(export_path,"17_rsegs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")

sf::sf_use_s2(FALSE) # switch off Spherical geometry ; some functions (eg. st_join, st_filter) give errors without this
facils <- sf::st_join(facils, rsegs[ ,c("riverseg","hydroid",geoCol(rsegs)) ]) #pairs riverseg column from rsegs w/ facils based on geometry
# st_write(facils, paste0(export_path,"18_facils_sf.csv"), layer_options = "GEOMETRY=AS_WKT")

#we add an riverseg column via goemetry in both source & facility cases b/c even when facilities come in with a riverseg column, many are blank
#```

#```{r Filter by Boundary Type, echo=FALSE, message=FALSE, warning=FALSE}
#----Rsegs----
if (map_type=="basin") { #finding upstream riversegs for basin maps 
  for(i in rivseg){
    if(i==rivseg[1]){
      basin <- fn_extract_basin(st_drop_geometry(rsegs), i) #fn_extract_basin runs sqldf which can't handle sf geometry
    }
    if(i!=rivseg[1]){
      basin <- rbind(basin, fn_extract_basin(st_drop_geometry(rsegs), i))
    }
  }
  # write.csv(basin, paste0(export_path,"19_basin.csv"))
  
  rsegs <- st_as_sf( merge(x=basin,y=rsegs), crs=crs_default) #add the geometries back on
  rsegs <- unique(rsegs) #don't duplicate riversegs for overlapping basins
  # st_write(rsegs, paste0(export_path,"20_rsegs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
  
  rm(basin)
} else if (map_type=="locality") { #for locality level
  rsegs <- st_filter(rsegs, counties[counties$name==locality, ]) #filter basins by locality (if REST is working)
} else if (map_type=="region") { #for region level
  region_OI <- regions[regions$region==region,] #extract region of interest for darker border around region 
  region_sp <- as_Spatial(region_OI, cast=FALSE)
  rsegs <- st_filter(rsegs, region_OI) #filter basins by the region given in params
}
#----Sources/Facils----
#Filtering data to only points within the extent of interest, either locality/region boundary or the basins/riversegs intersecting the extent
if (limit=="basins" | map_type=="basin") {
  facils <- st_filter(facils, rsegs)
  # st_write(facils, paste0(export_path,"21_facils_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
} else if (limit=="boundary"){
  if(map_type=="locality"){
    facils <- st_filter(facils, counties[counties$name==locality, ])
  } else if (map_type=="region") {
    facils <- st_filter(facils, region_OI)
  }
}
#```

#```{r Filter Cities & Roads, echo=FALSE, message=FALSE, warning=FALSE}
#----Cities----
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
cities <- sqldf_sf(statemt)
# write.csv(cities, paste0(export_path,"22_cities.csv"))
write.csv(cities, paste0(export_path,rivseg,"_cities.csv"))

#----Roads----
roads <- subset(roads, MTFCC=="S1100" & (RTTYP=="I"|RTTYP=="U"|RTTYP=="S") #primary roads & interstate, US Hwy, or State Rte only
                & FULLNAME %in% grep("([0-9]+).$", roads$FULLNAME, value=TRUE)) #finds where last char is a number -> Omits names followed by Byp, Alt, etc.
roads$FULLNAME <- gsub(".* ", "", roads$FULLNAME) #removes text and spaces before route number
names(roads) <- gsub("RTTYP", "CLASS", names(roads)) #re-name class column -> needed for fn_labelprep()
# st_write(roads, paste0(export_path,"23_roads_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
st_write(roads, paste0(export_path,rivseg,"_roads_sf.csv"), layer_options = "GEOMETRY=AS_WKT")

#```

## Additional Pulling Post-Filtering (Cleanup)
#```{r Pull Feature Metrics & Permitted Capacity, echo=FALSE, message=FALSE, warning=FALSE}
#----User-Input Non-Modeled Feature Metrics----
for (i in 1:nrow(facils)) {
  facils[i,metric_feat] <- RomProperty$new(ds,list(
    featureid = facils$Facility_hydroid[i],
    propname = metric_feat),
    TRUE)$propvalue #pull feature & directly assign metric propvalue to facility i
}
# st_write(facils, paste0(export_path,"24_facils_sf.csv"), layer_options = "GEOMETRY=AS_WKT")

#----Permitted Capacity----
fac_model_data <- data.frame()
for (i in unique(facils$hydroid) ){
  #i is a rseg hydroid corresponding to each facility, but it won't pull for the same hydroid twice b/c that is redundant and time-consuming
  model_props <- c("vwp_max_mgy","permit_status")
  fac_model_data <- rbind( fac_model_data , read.csv(paste(site,"/model-summary-users-export-all-cols/",
                                                           gsub(" ","",toString( i )),"/",
                                                           runid_list[1],"/", #perm.cap doesn't change w/ runid ?
                                                           gsub(" ","",toString( metric_mod )),"/", #user's model run metric
                                                           gsub(" ","",toString( model_props )),sep="")
  ))
}
# write.csv(fac_model_data, paste0(export_path,"25_fac_model_data.csv"))

#statemt <- paste("SELECT a.*, b.permit_status, c.vwp_max_mgy
#                  FROM facils as a
#                  LEFT OUTER JOIN (
#                    SELECT facility_hydroid, model_prop_propcode as permit_status
#                    FROM fac_model_data
#                    WHERE (model_prop_propname == 'permit_status')
#                    ) as b
#                  ON (a.Facility_hydroid = b.facility_hydroid)
#                  LEFT OUTER JOIN (
#                    SELECT facility_hydroid, model_prop_propcode as vwp_max_mgy
#                    FROM fac_model_data
#                    WHERE (model_prop_propname == 'vwp_max_mgy')
#                    ) as c
#                  ON (a.Facility_hydroid = c.facility_hydroid)
#                 ")
#test <- sqldf_sf(statemt, geomback="facils")
#test <- unique(test) #remove duplicated rows 

statemt <- paste("SELECT a.*, z.vwp_max_mgy, z.permit_status
                  FROM facils as a
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
facils <- sqldf_sf(statemt, geomback="facils")
facils <- unique(facils) #remove duplicated rows 
facils$vwp_max_mgy[is.na(facils$vwp_max_mgy)] <- "No Permit" #replace remaining NA w/ 'No Permit'; !! figure out why exactly NAs still exist
# st_write(facils, paste0(export_path,"26_facils_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
if(type=="facility"){
  st_write(facils, paste0(export_path,rivseg,"_facils_sf4.csv"), layer_options = "GEOMETRY=AS_WKT")
}
if(type=="source"){
  facils <- facils[names(facils) %in% grep("^([0-9]+).$", names(facils), value=TRUE, invert=TRUE)] #get rid of all those year columns
  st_write(facils, paste0(export_path,rivseg,"_mp_sf4.csv"), layer_options = "GEOMETRY=AS_WKT")
}
rm(fac_model_data)
#```

#```{r Pull Rseg Drought Metrics, echo=FALSE, message=FALSE, warnin;g=FALSE}
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
# st_write(rsegs, paste0(export_path,"27_rsegs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
rm(riverseg)
rm(model)
rm(model_scenario)
#```

## Calculations & Map Prep (Cleanup)
#```{r Calculate Rseg Metric % Diff, echo=FALSE, message=FALSE, warning=FALSE}
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
  rsegs <- sqldf_sf(statemt, geomback="rsegs")
}
# st_write(rsegs, paste0(export_path,"28_rsegs_sf.csv"), layer_options = "GEOMETRY=AS_WKT")
st_write(rsegs, paste0(export_path,rivseg,"_rsegs_sf4.csv"), layer_options = "GEOMETRY=AS_WKT")
rm(colname1)
rm(colname2)
#```
