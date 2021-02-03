stormSeparate<-function(timeIn,inflow,plt=F,path=paste0(getwd(),"/")){
  require(EcoHydRology)
  require(zoo)
  sto<-BaseflowSeparation(inflow,passes=3)#Baseflow filter from EcoHydRology with three passes
  sto$timestamp<-timeIn#Add timestamp to the baseflow separation for convenience
  
  a<-as.zoo(sto$qft)#Convert baseflow to zoo for data extraction
  maxes<-rollapply(a,3,function(x) which.max(x)==2)#Find mins/maxes of three consecutive points
  mins<-rollapply(a,3,function(x) which.min(x)==2)#such that the extreme is in the middle
  mins<-data.frame(timestamp=sto$timestamp[index(mins)[coredata(mins)]],mins=sto$qft[index(mins)[coredata(mins)]])
  maxes<-data.frame(timestamp=sto$timestamp[index(maxes)[coredata(maxes)]],maxes=sto$qft[index(maxes)[coredata(maxes)]])
  #Need to find mins/maxes below baseline flow. But first baseline flow must be defined
  #Below function hreg (horizontal regression) will try to fit mulitple horizontal lines
  #through subset of data involving every point below that line until best fit is found.
  #This becomes baseline flow, brk
  hreg<-function(x,limit=1){
    lim<-as.numeric(quantile(x,limit))
    x<-x[x<=lim]
    lns<-as.numeric(quantile(x,seq(0,1,0.00001)))
    lns<-lns[lns!=0]
    mse<-numeric(length(lns))
    for (i in 1:length(lns)){
      line<-lns[i]
      mse[i]<-mean((x-line)^2)
    }
    return(lns[which.min(mse)])
  }
  #Find the break associated with this run and buffer by 10%
  brk<-hreg(sto$qft,limit=1)
  brk<-brk*1.1
  #Next step is to isolate storms. This can be accomplished by taking a minimum and the next point
  #to fall below baseline flow, brk. Each storm is checked to ensure a maximum above some reference
  #level, here 1.5*brk. This eliminates small storms with little or no peak. Only minimums below the
  #brk value are considered as these are storms that occur at baseflow and fully rise/recede.
  x<-mins$timestamp[mins$mins<brk]#minimums at baseflow
  stormsep<-data.frame(timestamp=as.POSIXct(sto$timestamp),Baseflow=sto$bt,QuickFlow=sto$qft)#A data frame to build with storms in each column
  for (i in 1:(length(x)-1)){
    storm<-c(x[i],x[i+1])#Initial guess at storm endpoints
    maxtime<-sto$timestamp[sto$qft==max(sto$qft[sto$timestamp>x[i]&sto$timestamp<x[i+1]])]
    endAlt<-(sto$timestamp[sto$qft<brk&sto$timestamp>x[i]&sto$timestamp<x[i+1]&sto$timestamp>maxtime])[1]
    if (!is.na(endAlt)){#If there is a point at baseflow before next minimum, use it instead
      storm<-c(x[i],endAlt)#This prevent overelongated tails
    }
    stormdat<-sto[sto$timestamp>=storm[1]&sto$timestamp<=storm[2],]#data frame of storm data
    store<-data.frame(timestamp=sto$timestamp,qft=NA)#data frame of whole time series
    store$qft[store$timestamp>=storm[1]&store$timestamp<=storm[2]]<-stormdat$qft#Fills in only flow data during storm, leaving rest as NA
    assign(paste0("Storm",length(colnames(stormsep))-2),store$qft)
    if(max(store$qft,na.rm=T)>(1.5*brk)){#If maximum exceeds limit, bind it to stormsep data frame
      stormsep<-cbind(stormsep,get(paste0("Storm",length(colnames(stormsep))-2)))
      names(stormsep)[length(colnames(stormsep))]<-paste0("Storm",length(colnames(stormsep))-3)
    }
  }
  #rm(storm,endAlt,i,store,x,a,stormdat)#Clean up environment
  
  #Now plot each storm and fit exponential curves to rising and falling limbs
  #Store coefficients and statistics for each curve into a data frame, looking at 
  #shape of curve and the adjusted R square values. Store each storm as a PNG graph
  #in the designated area.
  #Need to prevent errors from zero flow. Added 0.0001 to all flow values, this is the tolerance
  #of the MPM method used
  ext<-".png"
  transients<-data.frame(rising=numeric(length(colnames(stormsep))-3),RsqR=NA,falling=NA,RsqF=NA,durAll=NA,durF=NA,durR=NA)
  for (i in 4:length(colnames(stormsep))){
    storm<-stormsep[!is.na(stormsep[,i]),c(1,i)]#Find the storm of interest
    maxtime<-storm[storm[,2]==max(storm[,2]),1]#Look for where the max is
    rising<-storm[storm[,1]<=maxtime,]#Separate rising and falling limbs
    falling<-storm[storm[,1]>=maxtime,]
    modelR<-lm(log(rising[,2]+0.0001)~seq(1,length(rising[,1])))#Create an exponential regression for the rising limb
    transients$rising[i-3]<-summary(modelR)$coefficients[2]#Store exponential coefficient and 
    transients$RsqR[i-3]<-summary(modelR)$adj.r.squared#adjusted r squared values
    modelF<-lm(log(falling[,2]+0.0001)~seq(1,length(falling[,1])))#Create an exponential regression for the falling limb
    transients$falling[i-3]<-summary(modelF)$coefficients[2]
    transients$RsqF[i-3]<-summary(modelF)$adj.r.squared
    transients$durAll[i-3]<-length(storm$timestamp)#Finds duration of the storm, rising and falling limbs combined
    transients$durF[i-3]<-length(rising$timestamp)#Finds duration of the rising limb
    transients$durR[i-3]<-length(falling$timestamp)#Finds duration of the falling limb
    #Plot the storm and the fitted rising and falling limbs and store them in designated path
    if(plt==T){
      png(paste0(path,"storm",i,ext),width=1820,height=760)
      par(mar=c(5,6,2,4))
      plot(storm[,1],storm[,2],type='l',xlab='Date',ylab='Flow (cfs)',lwd=2,cex.axis=2,cex.lab=2)
      lines(storm[storm[,1]<=maxtime,1],exp(fitted(modelR)),col='blue',lwd=2)
      lines(storm[storm[,1]>=maxtime,1],exp(fitted(modelF)),col='red',lwd=2)
      abline(h=brk,lty=3,lwd=2)
      dev.off()
    }
  }
  #rm(maxtime,storm,rising,falling,modelR,modelF,i,ext,fxn_locations)
  out<-list(Storms=stormsep,Stats=transients)
  return(out)
}