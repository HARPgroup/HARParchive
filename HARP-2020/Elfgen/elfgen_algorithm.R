library(elfgen)
library(sqldf)

site <- "http://deq2.bse.vt.edu/d.dh"
save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');


basepath = '/var/www/R';
source(paste(basepath,'config.R',sep='/'))
source(paste(basepath,'auth.PRIVATE',sep='/'))


save_directory <-  "/var/www/html/data/proj3/out"


hydroid <- '59776' #'59760'   #'59776'
huc_level<- 'huc8'
flow_metric<- 'erom_q0001e_mean'
flow_reduction_pct<-10
site_comparison <- paste('http://deq1.bse.vt.edu/d.dh/dh-feature-contained-within-export', hydroid, "watershed", sep = '/')
containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")


nhdplus_id <- as.numeric(sqldf("SELECT hydroid 
                               FROM containing_watersheds 
                               WHERE ftype='nhdplus' "))


nhd_code<- sqldf(paste("SELECT hydrocode 
                      FROM containing_watersheds 
                      WHERE ftype = 'nhd_",huc_level,"'", sep=""))


inputs <- list(
  varkey = flow_metric,
  featureid = nhdplus_id,
  entity_type = "dh_feature"
)

#property dataframe returned
mean_prop <- getProperty(inputs, site)
mean_intake<-mean_prop$propvalue


# Example input parameters for retrieving data from VAHydro
watershed.code <- nhd_code$hydrocode
watershed.bundle <- 'watershed'
watershed.ftype <- paste("nhd_", huc_level, sep="")
x.metric <- flow_metric
y.metric <- 'aqbio_nt_total'
y.sampres <- 'species'
datasite <- 'http://deq2.bse.vt.edu/d.dh'

# elfdata_vahydro() function for retrieving data from VAHydro
watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,
                                x.metric,y.metric,y.sampres,datasite)

# clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
watershed.df <- clean_vahydro(watershed.df)

elf_quantile <- 0.80
breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, 
                     "blo" = 100, "bhi" = 1000)  

elf <- elfgen("watershed.df" = watershed.df,
              "quantile" = elf_quantile,
              "breakpt" = breakpt,
              "yaxis_thresh" = 53, 
              "xlabel" = flow_metric,
              "ylabel" = "Fish Species Richness")


m<-elf$stats$m
b<-elf$stats$b
int<-m*log(mean_intake)+b

if (mean_intake>breakpt){
   print("The mean flow at the intake is greater than the breakpoint.")
}


elf$plot +
   geom_segment(aes(x = mean_intake, y = -Inf, xend = mean_intake, yend = int), linetype=2, color="red", size=1)+
   geom_segment(aes(x = 0, y = int, xend = mean_intake, yend =  int), linetype=2, color="red",size=1)+
   geom_point(aes(x=mean_intake, y=int, fill="Mean Flow @ Intake"),  color="red", shape="triangle",size=3)+
   labs(fill="Intake Legend")


elf$stats

abs_richness_change<-richness_change(elf$stats, "pctchg" = flow_reduction_pct) #, "xval" = mean_intake)
pct_richness_change<-richness_change(elf$stats, "pctchg" = flow_reduction_pct, "xval" = mean_intake)




# Confidence interval
colnames(watershed.df)[1] <- "x_var"
colnames(watershed.df)[2] <- "y_var"
colnames(watershed.df)[3] <- "watershed"

# data_below_breakpt <- watershed.df[!(watershed.df$x_var > breakpt),]

# # UPPER SUBSET
# upper_quant_data <- rq(y_var ~ log(x_var), data = data_below_breakpt, tau = 0.8)
# 
# 
# newy <- c(log(data_below_breakpt$x_var)*coef(upper_quant_data)[2]+coef(upper_quant_data)[1])
# upper_quant <- subset(data_below_breakpt, data_below_breakpt$y_var > newy)

up<-data.frame(elf$stats$quantile)


upper<-upper_quant

upper.lm <-lm(y_var~(log(x_var)), data=upper)

predict<-data.frame(predict(upper.lm, 
                            newdata=data.frame(x_var=mean_intake), 
                            interval = 'confidence', 
                            level=0.95))

lwr<-as.numeric(predict$lwr)
upr<-as.numeric(predict$upr)
fit<-as.numeric(predict$fit)


species_richness<-m*log(mean_intake)+b
species_richness

percent_error<-((fit-species_richness)/species_richness)*100
percent_error





















fname <- paste(
save_directory,
 paste0(
    'fig.elfgen.',
    watershed.code, '.', x.metric,'.', y.metric, '.png'
 ),
 sep = '/'
)

furl <- paste(
 save_url,
 paste0(
    'fig.elfgen.',
    watershed.code, '.', x.metric,'.', y.metric, '.png'
 ),
 sep = '/'
)


print(fname)
ggsave(fname)

##### Naming for saving and posting to VAHydro
print(paste("Saved file: ", fname, "with URL", furl))



#vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.30daymax_unmet', 0.0, site, token)


#VAHYDRO PROPS (posting)
#Confidence Interval
#Legend 

