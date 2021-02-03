# This script is meant to show how to download formatted data from USGS, CBP model, or VA Hydro sources,
# trim the data to the proper study period, calculate all the comparison metrics, and generate the
# data frame containing all data and the percent differences between them.

# INPUTS -----

basepath <- '/var/www/R' # must contain "config.local.private" file with directions to github repositories
site <- 'http://deq2.bse.vt.edu/d.dh' # vahydro site to download from -- d.dh or d.bet
riv.seg <- 'PL2_4810_0000' # river segment of interest
gage.num <- '01671020' # USGS gage associated with river segment
start.date <- '1984-01-01'
end.date <- '2014-12-31'
hydrocode <-'vahydrosw_wshed_PL2_4810_0000'
inputs <- list(
  hydrocode = hydrocode,
  bundle = 'watershed',
  ftype = 'vahydro'
)
dat.source <- 'vahydro'
run.id<-'runid_11'

# SETUP -----

setwd(basepath) # sets working directory to /var/www/R directory -- the output location

source(paste(basepath,"config.local.private", sep = "/")) # loads in repository directories
source(paste0(cbp6_location,"/code/cbp6_functions.R")) # loads in functions of cbp6 package
source(paste(github_location,"auth.private", sep = "/")) # loads in VA Hydro authorization creds
source(paste(cbp6_location, "/code/fn_vahydro-1.0.R", sep = '')) # loads in essential VA Hydro fns
token <- rest_token(site, token, rest_uname, rest_pw); # gets token used to download VA Hydro data
options(timeout=1200); # set timeout to twice default level to avoid abort due to high traffic

#
# # OBTAINING DATA -----
# gage.dat <- gage_import_data_cfs(gage.num, start.date, end.date)
# model.dat <- model_import_data_cfs(riv.seg, mod.phase = 'p6/p6_gb604', mod.scenario = 'CFBASE30Y20180615',
#                                    start.date, end.date)
# vahydro.dat <- vahydro_import_data_cfs(riv.seg, run.id = 11, token, site, start.date, end.date)
# 
# 
# # TRIMMING DATA -----
# gage.dat <- water_year_trim(gage.dat)
# model.dat <- water_year_trim(model.dat)
# vahydro.dat <- water_year_trim(vahydro.dat)
# 
# # CALCULATING METRICS -----
# gage.metrics <- metrics_calc_all(gage.dat)
# model.metrics <- metrics_calc_all(model.dat)
# vahydro.metrics <- metrics_calc_all(vahydro.dat)
# 
# # VAHYDRO METRIC PUSH/PULL -----
# # METRICS WOULD USUALLY BE POSTED TO VAHYDRO CONTAINERS AT THIS POINT
# # METRICS WOULD THEN BE PULLED FROM VAHYDRO CONTAINERS IN A DIFFERENT SCRIPT, HERE.
# 
# # COMPARING METRICS -----
# gage.v.model <- metrics_compare(gage.metrics, model.metrics, riv.seg)
# gage.v.vahydro <- metrics_compare(gage.metrics, vahydro.metrics, riv.seg)
# 
# # EXPORTING METRICS AS .CSV
# write.csv(gage.v.model, paste0(basepath, "/gage.v.model.csv"))
# write.csv(gage.v.vahydro, paste0(basepath, "/gage.v.vahydro.csv"))
# 



#Part 2

# River Segment: Anacostia River at Potomac Confluence
# Feature ID: 67720
# vahydro ID: vahydrosw_wshed_PL2_4810_0000

vahydro.pid <- get.overall.vahydro.prop(riv.seg, site, token) 
scen.prop.id <- get.scen.prop(riv.seg, mod.scenario = 'vahydro-1.0', dat.source, run.id, start.date, end.date, site, token)
#need to go back and find 7Q10 property







#Part 3


# SETTING UP BASEPATH AND SOURCING FUNCTIONS

basepath <- '/var/www/R'
setwd(basepath)
source(paste(basepath,"config.local.private", sep = "/"))
source(paste0(cbp6_location,"/code/cbp6_functions.R"))
source(paste0(github_location, "/auth.private"));
source(paste(cbp6_location, "/code/fn_vahydro-1.0.R", sep = ''))

# ESSENTIAL INPUTS
riv.seg <- 'JU2_7140_7330'
dat.source1 <- 'cbp_model'
dat.source2 <- 'vahydro'
site <- "http://deq2.bse.vt.edu/d.dh"
token <- rest_token(site, token, rest_uname, rest_pw)
# If a gage is used -- all data is trimmed to gage timeframe.  Otherwise, start/end date defaults
# can be found in the gage.timespan.trimmed loop.

# Inputs if using CBP Model -- otherwise, can ignore
mod.phase <- 'p6/p6_gb604' #or "p532c-sova" (phase 5)
mod.scenario1 <- 'CFBASE30Y20180615' #or 'CBASE1808L55CY55R45P50R45P50Y' (climate change) 'CFBASE30Y20180615' (base) 'CBASE1808L55CY55R45P10R45P10Y' (climate change 10%) 'CBASE1808L55CY55R45P90R45P90Y' (climate change 90%)
mod.scenario2 <- mod.scenario1
site.or.server <- 'site'

# Inputs if using VA Hydro -- otherwise, can ignore
run.id1 <- '11'
run.id2 <- run.id1

# Inputs if using USGS gage -- otherwise, can ignore
gage_number <- '02013000'
gage_timespan <- get.gage.timespan(gage_number)
gage.title <- 'USGS 02013000 DUNLAP CREEK NEAR COVINGTON, VA'

if (dat.source1 == 'gage' || dat.source2 == 'gage') {
  gage.timespan.trimmed <- TRUE #or FALSE
  post.gage.scen.prop(riv.seg, gage.title, site, token)
  
}

if (gage.timespan.trimmed == TRUE) {
  start.date <- as.character(gage_timespan[[1]]) #1984-01-01
  end.date <- as.character(gage_timespan[[2]]) #1984-12-31
} else {
  start.date <- '1984-01-01' #1984-01-01
  end.date <- '2014-12-31' #1984-12-31
}

# Changes graph labels automatically
if (dat.source1 == 'vahydro') {
  cn1 <- paste('VAhydro_runid_', run.id1, sep = '')
} else if (dat.source1 == 'gage') {
  cn1 <- paste('USGS_', gage_number, sep = '')
} else if (dat.source1 == 'cbp_model') {
  cn1 <- paste('CBP_scen_ ', mod.scenario1, sep = '')
}

if (dat.source2 == 'vahydro') {
  cn2 <- paste('VAhydro_runid_', run.id2, sep = '')
} else if (dat.source2 == 'gage') {
  cn2 <- paste('USGS_', gage_number, sep = '')
} else if (dat.source2 == 'cbp_model') {
  cn2 <- paste('CBP_scen_ ', mod.scenario2, sep = '')
}

# POSTING METRICS
automated_metric_2_vahydro(dat.source = dat.source1, riv.seg = riv.seg, gage_number = gage_number, run.id = run.id1, gage.timespan.trimmed = gage.timespan.trimmed, mod.phase = mod.phase, mod.scenario = mod.scenario1, start.date = start.date, end.date = end.date, github_link = github_location, site = site, site.or.server = 'site', token = token)
automated_metric_2_vahydro(dat.source = dat.source2, riv.seg = riv.seg, gage_number = gage_number, run.id = run.id2, gage.timespan.trimmed = gage.timespan.trimmed, mod.phase = mod.phase, mod.scenario = mod.scenario2, start.date = start.date, end.date = end.date, github_link = github_location, site = site, site.or.server = 'site', token = token)

# CREATING DASHBOARD (outputted in /var/www/R)
rmarkdown::render(paste0(cbp6_location, '/code/Modularized_Dashboard_VAHydro.Rmd'), 
                  output_dir = basepath, output_file = paste0(riv.seg, '.pdf'), 
                  params = list(riv.seg = riv.seg, dat.source1 = dat.source1, 
                                dat.source2 = dat.source2, start.date = start.date, 
                                end.date = end.date, github_location = github_location, site = site, 
                                site.or.server = site.or.server, run.id1 = run.id1, 
                                run.id2 = run.id2, gage_number = gage_number, 
                                mod.phase1 = mod.phase, mod.scenario1 = mod.scenario1, 
                                mod.phase2 = mod.phase, mod.scenario2 = mod.scenario2, 
                                gage.timespan.trimmed = gage.timespan.trimmed, 
                                cn1 = cn1, cn2 = cn2))
