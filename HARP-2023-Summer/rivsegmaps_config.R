## Customizations for riverseg drought metric maps

#For riverseg drought metric maps 
rivseg_pct_vect <- c(-20,-10,-2,2,10,20,500) #vector of values for rivseg drought maps
#^last value should be higher than any % difference value expected, since classification is done using <=
rivbreaks <- seq(1:length(rivseg_pct_vect))
rivmap_colors <- c("firebrick2","darkorange","#FFCC99",
                   "white","palegreen","limegreen","green4") #colors for fills based on % diff 
#^needs to be same length as rivseg_pct_vect
rivmap_labs <- c(" <= -20", #less than or equal to first value in pct vector
                 "-20 to -10", #labeling the ranges between each value from pct vector 
                 "-10 to -2",
                 "-2 to +2",
                 "+2 to +10",
                 "+10 to +20",
                 " > +20") #last label should be greater than 2nd-to-last value in pct vector 
#^needs to be same length as rivseg_pct_vect

rseg_highlight_limit <- -10 #sets upper llimit of % difference for highlighting values/rows in the riverseg table