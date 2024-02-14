# set up a caching function for NWIS
##install.packages("memoise")
library("memoise")

# identify location of caching / cached items
dir <- Sys.getenv("USGS_cache_dir")
if (dir == "") {
  dir <- tools::R_user_dir(package="dataRetrieval", which="cache")
  dir <- paste0(dir,"/USGS_cache_dir")
}
if (!dir.exists(dir)) {
  dir.create(dir, recursive=TRUE)
}

# set up the caching
db <- memoise::cache_filesystem(dir)
one_day <- 24*60^2
one_year <- 365 * one_day

memo_readNWISdv <- memoise::memoise(dataRetrieval::readNWISdv, ~memoise::timeout(one_day), cache = db)
memo_readNWISsite <- memoise::memoise(dataRetrieval::readNWISsite, ~memoise::timeout(one_day), cache = db)

memo_get_nhdplus <- memoise::memoise(nhdplusTools::get_nhdplus, ~memoise::timeout(one_year), cache = db)
memo_get_UT <- memoise::memoise(nhdplusTools::get_UT, ~memoise::timeout(one_year), cache = db)
memo_plot_nhdplus <- memoise::memoise(nhdplusTools::plot_nhdplus, ~memoise::timeout(one_year), cache = db)
memo_navigate_nldi <- memoise::memoise(nhdplusTools::navigate_nldi, ~memoise::timeout(one_year), cache = db)
