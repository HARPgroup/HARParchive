source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/GIS_functions/model_geoprocessor.R")
export_path <- 'C:/Users/megan/OneDrive/Desktop'

#------------------------------------------
plotname <- "potomac"
segswhere <- "hydrocode LIKE '%wshed_P%'"
scenario_a <- c("vahydro-1.0","runid_11")
scenario_b <- c("cbp-6.0","hsp2_2022")

#------------------------------------------
# process geo data
polygons_a <- model_geoprocessor(scenario_a,segswhere)
polygons_b <- model_geoprocessor(scenario_b,segswhere)

# generate & save plot figure
#filename <- paste0("Model_Map_",scenario_a[1],scenario_a[2],"_",scenario_b[1],scenario_b[2],"_",plotname,".png")
#png(file=paste(export_path,filename,sep=""), width=1500, height=1500)
plot(polygons_a, axes = 1, main=paste0(plotname,"\n",segswhere), cex.main=2, cex.axis=2.5, border = "black")
plot(polygons_b, col = "blue", add = T)
legend("topleft", legend=c(paste0(scenario_a[1],", ",scenario_a[2], " (",length(polygons_a),")"), 
                           paste0(scenario_b[1],", ",scenario_b[2], " (",length(polygons_b),")")), 
       fill = c("white","blue"), cex=3.5)
#dev.off()