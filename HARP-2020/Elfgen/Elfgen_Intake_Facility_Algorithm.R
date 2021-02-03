########################################
###### Elfgen Intake Facility Algorithm
########################################


#### Libraries

library(elfgen)
library(sqldf)
library(ggplot2)
library(stringr)


#### Load in directories and repositories
site <- "http://deq2.bse.vt.edu/d.dh"
save_directory <- "/var/www/html/data/proj3/out"
save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');

basepath ='/var/www/R';
source(paste(basepath,'config.R',sep='/'))


#### Take in input arguments

# argst <- commandArgs(trailingOnly=T)
# hydroid <- as.integer(argst[1])
# huc_level <- as.integer(argst[2])
# flow_metric <- as.integer(argst[3])
# flow_reduction_pct <- as.integer(argst[4])

hydroid <- 59776
huc_level <- 'huc8'
flow_metric <- 'erom_q0001e_mean'
flow_reduction_pct <- 10

#### Take in watershed and mean intake data

site_comparison <- paste(site,'dh-feature-contained-within-export', hydroid, 'watershed', sep = '/')

containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")

nhd_code <- sqldf(paste("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_", huc_level,"'", sep = ""))

hydroid2 <- sqldf("SELECT hydroid 
                  FROM containing_watersheds 
                  WHERE ftype 
                  LIKE '%nhdplus%'")

#### Return property dataframe and mean intake

inputs <- list(
  varkey = flow_metric,
  featureid = as.numeric(hydroid2$hydroid),
  entity_type = "dh_feature"
)

dataframe <- getProperty(inputs, site)

mean_intake <- dataframe$propvalue

#### Input parameters for retrieving data from VAHydro

watershed.code <- as.character(nhd_code)
watershed.bundle <- 'watershed'
watershed.ftype <- paste("nhd_", huc_level, sep = "")
x.metric <- flow_metric
y.metric <- 'aqbio_nt_total'
y.sampres <- 'species'

# elfdata_vahydro() function for retrieving data from VAHydro
watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,site)
# clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
watershed.df <- clean_vahydro(watershed.df)

elf_quantile <- 0.80
breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, "blo" = 100, "bhi" = 1000)  

elf <- elfgen("watershed.df" = watershed.df,
              "quantile" = elf_quantile,
              "breakpt" = breakpt,
              "yaxis_thresh" = 53, 
              "xlabel" = flow_metric,
              "ylabel" = "Fish Species Richness")


#### Solving for confidence interval lines

# xdat <- c(elf$plot$data$x_var)
# ydat <- c(elf$plot$data$y_var)
# data <- as.data.frame(elf$plot$data)

uq <- elf$plot$plot_env$upper.quant

upper.lm <- lm(y_var ~ log(x_var), data = uq)

predict <- as.data.frame(predict(upper.lm, newdata = data.frame(x_var = mean_intake), interval = 'confidence'))

species_richness<-elf$stats$m*log(mean_intake)+elf$stats$b

# Comparing predict to actual values
#fit<-as.numeric(predict$fit)
#species_richness<-elf$stats$m*log(mean_intake)+elf$stats$b
#percent_error<-((fit-species_richness)/species_richness)*100


xmin <- min(uq$x_var)
xmax <- max(uq$x_var)

yval1 <- predict(upper.lm, newdata = data.frame(x_var = xmin), interval = 'confidence')
yval2 <- predict(upper.lm, newdata = data.frame(x_var = xmax), interval = 'confidence')

ymin1 <- yval1[2] # bottom left point, line 1
ymax1 <- yval2[3] # top right point, line 1

ymin2 <- yval1[3] # top left point, line 2
ymax2 <- yval2[2] # bottom right point, line 2

m <- elf$stats$m
b <- elf$stats$b
int <- m*log(mean_intake) + b      # solving for mean_intake y-value

m1 <- (ymax1-ymin1)/(log(xmax)-log(xmin)) # line 1
b1 <- ymax1-(m1*log(xmax))

m2 <- (ymax2-ymin2)/(log(xmax)-log(xmin)) # line 2
b2 <- ymax2 - (m2*log(xmax))


#### Plot

plt <- elf$plot +
  geom_segment(aes(x = mean_intake, y = -Inf, xend = mean_intake, yend = int), color = 'red', linetype = 'dashed') +
  geom_segment(aes(x = 0, xend = mean_intake, y = int, yend = int), color = 'red', linetype = 'dashed') +
  geom_point(aes(x = mean_intake, y = int, fill = 'Intake'), color = 'red', shape = 'triangle', size = 2) +
  geom_segment(aes(x = xmin, y = (m1 * log(xmin) + b1), xend = xmax, yend = (m1 * log(xmax)) + b1), color = 'blue', linetype = 'dashed') +
  geom_segment(aes(x = xmin, y = (m2 * log(xmin) + b2), xend = xmax, yend = (m2 * log(xmax)) + b2), color = 'blue', linetype = 'dashed') +
  labs(fill = 'Intake Legend')

plt
#### Using confidence interval lines to find percent/absolute richness bounds
#following two lines are for the base m and b values
base_pct_richness <- richness_change(elf$stats, 'pctchg' =flow_reduction_pct, 'xval'=mean_intake)
abs_base <- richness_change(elf$stats, 'pctchg' = flow_reduction_pc)

elf$bound1stats$m <- m1
elf$bound1stats$b <- b1

percent_richness_change_bound1 <- richness_change(elf$bound1stats, "pctchg" = flow_reduction_pct, "xval" = mean_intake)
abs_richness_change_bound1 <- richness_change(elf$bound1stats, "pctchg" = flow_reduction_pct)

elf$bound2stats$m <- m2
elf$bound2stats$b <- b2

percent_richness_change_bound2 <- richness_change(elf$bound2stats, "pctchg" = flow_reduction_pct, "xval" = mean_intake)
abs_richness_change_bound2 <- richness_change(elf$bound2stats, "pctchg" = flow_reduction_pct)

# checking diffs in pct richness
diff1 <- percent_richness_change_bound1 - base_pct_richness
diff2 <- percent_richness_change_bound2 - base_pct_richness

#checking diffs in abs richness
abs_d1 <- abs_base - abs_richness_change_bound1
abs_d2 <- abs_base - abs_richness_change_bound1

#### Saving

fname <- paste(
  save_directory,
  paste0(
    'fig.elfgen.',
    watershed.code, '.', x.metric, '.', y.metric, '.png'
  ),
  sep = '/'
)

furl <- paste(
  save_url,
  paste0(
    'fig.elfgen.',
    watershed.code, '.', x.metric, '.', y.metric, '.png'
  ),
  sep = '/'
)

print(fname)
ggsave(fname, width = 7, height = 5.5)

print(paste("Saved file: ", fname, "with URL", furl))


#### Posting to VAHydro