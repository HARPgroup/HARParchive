# set up a caching function for NWIS
Sys.setenv(USGS_cache_dir = "/media/model/usgs")

hydro_tools <- 'https://raw.githubusercontent.com/HARPgroup/hydro-tools/master'
cbp6_location <- 'https://raw.githubusercontent.com/HARPgroup/cbp6/master'
hydro_tools_location <- hydro_tools
om_location <- "https://raw.githubusercontent.com/HARPgroup/om/master"
elfgen_location <- "https://raw.githubusercontent.com/HARPgroup/elfgen/master"
openmi_om_location <- "https://raw.githubusercontent.com/HARPgroup/openmi-om/master"
vahydro_location <- "https://github.com/HARPgroup/vahydro/blob/master"
github_location <- "C:/usr/local/home/git"
HARParchive_location <- paste0(github_location, "/HARParchive")
# foundation_location <- "C:\\Users\\faw18626\\OneDrive - Commonwealth of Virginia"
foundation_location <- paste0(Sys.getenv("USERPROFILE"),"\\OneDrive - Commonwealth of Virginia\\OWS\\foundation_datasets\\wsp\\wsp2020")
onedrive_location <- paste0(Sys.getenv("USERPROFILE"),"\\OneDrive - Commonwealth of Virginia")

folder <- '/Workspace/tmp/'
export_path <- '/Workspace/tmp/'
base_url <- 'http://deq1.bse.vt.edu:81/d.dh' #needed for REST functions
save_url <- "http://deq1.bse.vt.edu:81/data/proj3/out";
noaa_api_key <- 'OdnQxMwsNwbNTPiFJRygEwxflkOIrxhh'
options(noaakey = noaa_api_key)

#### Old Stuff No Longer Needed?
vahydro_directory <- "/usr/local/home/git/r-dh-ecohydro/Analysis/fn_vahydro-2.0"
auth_directory <- "/usr/local/home/git/r-dh-ecohydro/ELFGEN/internal"
file_directory <- "/Workspace/tmp"
save_directory <- "/Workspace/tmp"
fxn_locations <- "/usr/local/home/git/r-dh-ecohydro/ELFGEN/internal/"
habitat_files <- "/usr/local/home/git/r-dh-ecohydro/Analysis/Habitat/"
fxn_vahydro <- "/usr/local/home/git/hydro-tools"
base_directory <- "/usr/local/home/git/r-dh-ecohydro"

