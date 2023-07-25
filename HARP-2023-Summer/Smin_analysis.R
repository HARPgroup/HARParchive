#Analyzing droughts and storage for a better understanding/estimation of Smin for WA & CU calc.

basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# get impoundment feature 
mountAltoImp <- RomProperty$new(ds,list(
  featureid = 72575,
  propcode = "vahydro-1.0",
  varkey = "om_hydroimpoundment"),
  TRUE)
