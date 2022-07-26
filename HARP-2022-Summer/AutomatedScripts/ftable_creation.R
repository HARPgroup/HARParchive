
# Remember to REMOVE!: 
# From Github: Work flow (for existing river segments):
#     Get stream info (drainage area, phys province) from vahydro-1.0 (or csv/cmd line?)
#     Calculate ftable
#     Can check work with already made ftables
#     Save ftable in vahydro
#     save ftable in CBP scenario csv (maybe in a week or so when we are sure of what we're doing)
#     add/update any other aspects in cbp hspf csv files
#----------------------------------------------------------------------------------------------------

# script that generates FTABLEs from existing river segments in VAHydro

#setup
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory

omsite = "http://deq1.bse.vt.edu:81" #establishing location on server for storage


# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
feature_name <- argst[1] # 68308 for Sugar Hollow
# idk what else yet...

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Pulling from VAHydro


# Calculating Channel Geometry


# Calculating FTABLE


# Exporting to VAHydro
#    (basically took code from other scripts & changed "land" to "river")
riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=rseg_name, 
    ftype=rseg_ftype,
    bundle='riverunit'
  ), 
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_water_model_node", #got here http://deq1.bse.vt.edu:81/d.dh/om-model-info/68308/dh_feature
    propname=riverseg$name,
    featureid=riverseg$hydroid, 
    entity_type="dh_feature", 
    propcode="vahydro-1.0"        #also got this from that^
  ), 
  TRUE
)
model$save(TRUE)

# no specific scenario or land use so skipping those sections...

local_channel<- RomProperty$new(
  ds, list(
    varkey="om_USGSChannelGeomObject_sub",    #found here http://deq1.bse.vt.edu:81/d.dh/admin/content/dh_properties/manage/4712896/dh_properties?items_per_page=40&page=1
    featureid=model$pid, #model is the section above
    entity_type='dh_properties',
    propname = 'local_channel'
  ),
  TRUE
)
local_channel$save(TRUE)

ftable<- RomProperty$new(
  ds, list(
    varkey="om_class_Constant", # what do we say when it's a table??
    featureid=local_channel$pid,
    entity_type='dh_properties',
    propname = 'ftable'
  ),
  TRUE
)
ftable$propvalue <- #the ftable of data
ftable$save(TRUE)

