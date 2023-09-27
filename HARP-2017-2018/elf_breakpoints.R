bundle <- 'watershed';
ftype <- 'nhd_huc8';
metric <- 'aqbio_nt_cent';
selected <- 'all';
quantile <- 0.8;
dataset_tag = 'ymax50'; # full, full_ymax_q75, ymax50, 1990-2010, 1970-1980, ...
qftype <- 'fe_quantreg_pwit'; # fe_quantreg, fe_quantreg_pwit, fe_quantreg_ymax, fe_twopoint
pmax  <- 0.15;
xvar <- 'all';
uri <- paste("http://deq1.bse.vt.edu/d.dh/fe-export-regparms", bundle, ftype, selected, metric, quantile, pmax, qftype, xvar, dataset_tag, sep='/');
data = read.csv(uri, header = TRUE, sep = "\t");
data.da <- subset(data, x == 'nhdp_drainage_sqkm');
data.mean <- subset(data, x == 'erom_q0001e_mean');
data.jan <- subset(data, x == 'erom_q0001e_jan');
data.feb <- subset(data, x == 'erom_q0001e_feb');
data.mar <- subset(data, x == 'erom_q0001e_mar');
data.apr <- subset(data, x == 'erom_q0001e_apr');
data.may <- subset(data, x == 'erom_q0001e_may');
data.jun <- subset(data, x == 'erom_q0001e_june');
data.jul <- subset(data, x == 'erom_q0001e_july');
data.aug <- subset(data, x == 'erom_q0001e_aug');
data.sep <- subset(data, x == 'erom_q0001e_sept');
data.oct <- subset(data, x == 'erom_q0001e_oct');
data.nov <- subset(data, x == 'erom_q0001e_nov');
data.dec <- subset(data, x == 'erom_q0001e_dec');
n = c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
title <- paste("R^2 of upper ", 100.*(1.0 - quantile), "% for \n", metric, " = f(DA), f(Qmean), f(Qjan),..., f(Qdec) for ", ftype);

boxplot(
  data.da$Rsq, data.mean$Rsq, data.jan$Rsq, data.feb$Rsq, data.mar$Rsq, 
  data.apr$Rsq, data.may$Rsq, data.jun$Rsq, data.jul$Rsq, data.aug$Rsq, 
  data.sep$Rsq, data.oct$Rsq, data.nov$Rsq, data.dec$Rsq,
  names = n, 
  main = title
);
z <- cbind(
  rbind(as.matrix(summary(data.da$Rsq)), as.matrix(list(Count = length(data.da$adminid)))), 
  rbind(as.matrix(summary(data.mean$Rsq)), as.matrix(list(Count = length(data.mean$adminid)))), 
  rbind(as.matrix(summary(data.jan$Rsq)), as.matrix(list(Count = length(data.jan$adminid)))), 
  rbind(as.matrix(summary(data.feb$Rsq)), as.matrix(list(Count = length(data.feb$adminid)))), 
  rbind(as.matrix(summary(data.mar$Rsq)), as.matrix(list(Count = length(data.mar$adminid)))), 
  rbind(as.matrix(summary(data.apr$Rsq)), as.matrix(list(Count = length(data.apr$adminid)))), 
  rbind(as.matrix(summary(data.may$Rsq)), as.matrix(list(Count = length(data.may$adminid)))), 
  rbind(as.matrix(summary(data.jun$Rsq)), as.matrix(list(Count = length(data.jun$adminid)))), 
  rbind(as.matrix(summary(data.jul$Rsq)), as.matrix(list(Count = length(data.jul$adminid)))), 
  rbind(as.matrix(summary(data.aug$Rsq)), as.matrix(list(Count = length(data.aug$adminid)))), 
  rbind(as.matrix(summary(data.sep$Rsq)), as.matrix(list(Count = length(data.sep$adminid)))), 
  rbind(as.matrix(summary(data.oct$Rsq)), as.matrix(list(Count = length(data.oct$adminid)))), 
  rbind(as.matrix(summary(data.nov$Rsq)), as.matrix(list(Count = length(data.nov$adminid)))), 
  rbind(as.matrix(summary(data.dec$Rsq)), as.matrix(list(Count = length(data.dec$adminid))))
);

colnames(z) <- c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
print(z);

# box-whisker plot of breakpoint by y-metric (biometric)

title <- paste("Breakpoint summary for \n", metric, " = f(DA), f(Qmean), f(Qjan),..., f(Qdec) for ", ftype);
boxplot(
  data.da$bkpt, data.mean$bkpt, data.jan$bkpt, data.feb$bkpt, data.mar$bkpt, 
  data.apr$bkpt, data.may$bkpt, data.jun$bkpt, data.jul$bkpt, data.aug$bkpt, 
  data.sep$bkpt, data.oct$bkpt, data.nov$bkpt, data.dec$bkpt,
  names = n, 
  main = title
);
z <- cbind(
  as.matrix(summary(data.da$bkpt)), 
  as.matrix(summary(data.mean$bkpt)), 
  as.matrix(summary(data.jan$bkpt)), 
  as.matrix(summary(data.may$bkpt)), 
  as.matrix(summary(data.aug$bkpt)), 
  as.matrix(summary(data.dec$bkpt))
)
colnames(z) <- c('DA', 'Mean', 'January', 'April', 'August', 'December')
print(z);
