# Code no longer used in Dataframe_Generator.R

### Pulling modeled riverseg metrics
# for (k in 1:length(rivseg_metric)) {
#   for (j in 1:length(runid_list)) {
#     for (i in 1:nrow(rsegs)) {
#       riverseg <- RomFeature$new(ds,list( #get riverseg feature from vahydro
#         hydrocode = paste('vahydrosw_wshed_',rsegs$riverseg[i],sep=''),
#         ftype = 'vahydro',
#         bundle = 'watershed'
#       ),TRUE)
#       
#       if (!is.na(riverseg$hydroid)) { #only continue if rivseg feature was found
#         model <- RomProperty$new(ds,list( #get vahydro-1.0 model feature from vahydro
#           featureid = riverseg$hydroid,
#           propcode = 'vahydro-1.0'
#         ),TRUE)
#         
#         model_scenario <- RomProperty$new(ds,list( #get scenario/runid from vahydro
#           varkey = "om_scenario",
#           featureid = model$pid,
#           propname = runid_list[j]
#         ),TRUE)
#         
#         if (!is.na(model_scenario$pid)) { #only continue if runid was found (scenario pid!=NA)
#           rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- RomProperty$new(ds,list( #get metric from vahydro
#             featureid = model_scenario$pid,
#             entity_type = 'dh_properties',
#             propname = rivseg_metric[k]
#           ),TRUE)$propvalue #directly assign metric propvalue
#         } else { #the scenario/runid wasn't found
#           rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- NA
#         }
#       } else { #the rivseg feature wasn't found
#         rsegs[i, paste0(runid_list[j],'_',rivseg_metric[k]) ] <- NA
#       }
#     }
#   }
# }
# rm(riverseg)
# rm(model)
# rm(model_scenario)




#---Permitted Capacity--- 
# fac_model_data <- data.frame()
# for (i in unique(featrs$hydroid) ){
#   #i is a rseg hydroid corresponding to each facility, but it won't pull for the same hydroid twice b/c that is redundant and time-consuming
#   model_props <- c("vwp_max_mgy","permit_status")
#   fac_model_data <- rbind( fac_model_data , read.csv(paste(site,"/model-summary-users-export-all-cols/",
#                                                            gsub(" ","",toString( i )),"/",
#                                                            runid_list[1],"/", #perm.cap doesn't change w/ runid ?
#                                                            gsub(" ","",toString( metric_mod )),"/", #user's model run metric
#                                                            gsub(" ","",toString( model_props )),sep="")
#   ))
# }
# 
# 
# statemt <- paste("SELECT a.*, z.vwp_max_mgy, z.permit_status
#                   FROM featrs as a
#                   LEFT OUTER JOIN
#               (   SELECT c.facility_hydroid, b.permit_status,
#                   CASE WHEN b.exempts == 'exempt'
#                     THEN b.exempts
#                   ELSE c.permcaps
#                   END as vwp_max_mgy ", #when status is exempt, makes permitted capacity exempt too
#                  "FROM
#                  ( (SELECT facility_hydroid, model_prop_propcode as permit_status,
#                       CASE WHEN model_prop_propcode == 'exempt'
#                         THEN 'exempt'
#                       ELSE 'switch'
#                       END as exempts
#                     FROM fac_model_data
#                     WHERE (model_prop_propname == 'permit_status')
#                     ) as b
#                   LEFT OUTER JOIN (
#                     SELECT facility_hydroid, model_prop_propcode,
#                       CASE WHEN model_prop_propcode IS NULL OR model_prop_propcode == 0
#                         THEN 'No Permit' ", #changes null/0 permit capacity to 'No Permit'
#                  "ELSE model_prop_propcode
#                       END as permcaps
#                     FROM fac_model_data
#                     WHERE (model_prop_propname == 'vwp_max_mgy')
#                     ) as c
#                   ON (b.facility_hydroid = c.facility_hydroid)
#                  )
#               ) as z
#               ON (a.Facility_hydroid = z.facility_hydroid)
#                  ", sep='')
# featrs <- fn_sqldf_sf(statemt, geomback="featrs")
# featrs <- unique(featrs) #remove duplicated rows
# featrs$vwp_max_mgy[is.na(featrs$vwp_max_mgy)] <- "No Permit" #replace remaining NA w/ 'No Permit'; !! figure out why NAs still exist
# rm(fac_model_data)

