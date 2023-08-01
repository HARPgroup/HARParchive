#Analyzing droughts and storage for a better understanding/estimation of Smin for WA & CU calc.

basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# get impoundment feature 
mountAltoImp <- RomProperty$new(ds,list( #test case: Rob's vineyard 
  featureid = 72575,
  propcode = "vahydro-1.0",
  varkey = "om_hydroimpoundment"),
  TRUE)

#Lake Anna Dam
AnnaDam <- RomProperty$new(ds,list( #test case: Rob's vineyard 
  featureid = 166269,
  propcode = "vahydro-1.0",
  varkey = "om_hydroimpoundment"),
  TRUE)

#check imp_off
imp_off <- RomProperty$new(ds,list( #test case: Rob's vineyard -- unsuccessful 
  featureid = AnnaDam$pid,
  entity_type = "dh_feature",
  propname = "imp_off"),
  TRUE)

# pull impoundment var
imp_use <- RomProperty$new(ds,list( #test case: Rob's vineyard -- unsuccessful 
  featureid = mountAltoImp$pid,
  entity_type = "dh_timeseries",
  propname = "impoundment_use_remain_mg"),
  TRUE)
