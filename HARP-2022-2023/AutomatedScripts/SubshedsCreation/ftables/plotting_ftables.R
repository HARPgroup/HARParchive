
# Plots Pre-Existing Ftables for Comparison

#arguments
argst <- commandArgs(trailingOnly = T)
path <- argst[1] #path to where ftable files are
riverseg1 <- argst[2]
riverseg2 <- argst[3]

#Testing: delete later
path_uci <- 'http://deq1.bse.vt.edu:81/p532/input/param/river/hsp2_2022/ftables/'

path <- '/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/ftables/'
riverseg1 <- "JL2_6850_6890" #Rockfish 
riverseg2 <- "OR1_7700_7980" #Roanoke 

#reading ftables from .uci format
ftable1 <- read.csv(paste(path_uci, riverseg1, '.ftable', sep=''), 
                    sep='', 
                    header=FALSE,
                    col.names = c('depth','area','vol', 'disch','flo-thru'),
                    nrows = 19, skip= 5)

ftable2 <- read.csv(paste(path, riverseg2, '.ftable', sep=''), 
                    sep='', 
                    header=FALSE,
                    col.names = c('depth','area','vol', 'disch','flo-thru'),
                    nrows = 19, skip= 5)

#take out flo-thru when done with old uci's ^^

par(mfrow = c(2,3)) #to arrange plots in 2x3 grid
#----Full Plots----
#--Area
plot(ftable1$depth, ftable1$area, 
     type='l', col = 'red', 
     ylim=c(0, max(c(max(ftable1$area),max(ftable2$area)))), #ylim = max area out of both ftables
     xlab = "Depth (ft)", ylab = "Water Surface Area (acres)")

lines(ftable2$depth, ftable2$area, type='l', col='blue')
title(main = 'Surface Area')
legend(x='bottomright',
       legend= c(riverseg1, riverseg2),
       col=c('red', 'blue'),bty='n',lty=1, cex=0.85)

#--Volume
plot(ftable1$depth, ftable1$vol, 
     type='l', col = 'red',
     ylim=c(0, max(c(max(ftable1$vol),max(ftable2$vol)))), #ylim = max vol out of both ftables 
     xlab = "Depth (ft)", ylab = "Volume (acre-ft)")

lines(ftable2$depth, ftable2$vol, type='l', col='blue')
title(main = 'Volume')
legend(x='topleft',
       legend= c(riverseg1, riverseg2),
       col=c('red', 'blue'),bty='n',lty=1, cex = 0.85)

#--Discharge
plot(ftable1$depth, ftable1$disch, 
     type='l', col = 'red',
     ylim=c(0, max(c(max(ftable1$disch),max(ftable2$disch)))), #ylim = max disch out of both ftables
     xlab = "Depth (ft)", ylab = "Discharge (cfs)")

lines(ftable2$depth, ftable2$disch, type='l', col='blue')
title(main = 'Discharge')
legend(x='topleft',
       legend= c(riverseg1, riverseg2), 
       col=c('red', 'blue'),bty='n',lty=1, cex=0.85)

#----Channel-Only Plots----
# Area
plot(ftable1$depth, ftable1$area, 
     type='l', col = 'red',
     xlim=c(0,max(c(ftable1$depth[10], ftable2$depth[10]))), #row 10 = bankfull data (from ftable_creation.R) 
     ylim=c(0,max(c(ftable1$area[10], ftable2$area[10]))), 
     xlab = "Depth (ft)", ylab = "Water Surface Area (acres)")

lines(ftable2$depth, ftable2$area, type='l', col='blue')
title(main = 'SA up to Bankfull')
legend(x='bottomright',
       legend= c(riverseg1, riverseg2), 
       col=c('red','blue'),bty='n',lty=1, cex=0.85)

# Volume
plot(ftable1$depth, ftable1$vol, 
     type='l', col = 'red',
     xlim=c(0,max(c(ftable1$depth[10], ftable2$depth[10]))),
     ylim=c(0,max(c(ftable1$vol[10], ftable2$vol[10]))), 
     xlab = "Depth (ft)", ylab = "Volume (acre-ft)")

lines(ftable2$depth, ftable2$vol, type='l', col='blue')
title(main = 'Volume up to Bankfull')
legend(x='topleft',
       legend= c(riverseg1, riverseg2), 
       col=c('red','blue'),bty='n',lty=1, cex=0.85)

# Discharge
plot(ftable1$depth, ftable1$disch, 
     type='l', col = 'red',
     xlim=c(0,max(c(ftable1$depth[10], ftable2$depth[10]))),
     ylim=c(0,max(c(ftable1$disch[10], ftable2$disch[10]))), 
     xlab = "Depth (ft)", ylab = "Discharge (cfs)")

lines(ftable2$depth, ftable2$disch, type='l', col='blue')
title(main = 'Discharge up to Bankfull')
legend(x='topleft',
       legend= c(riverseg1, riverseg2), 
       col=c('red','blue'),bty='n',lty=1, cex=0.85)
