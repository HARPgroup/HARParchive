---
  title: "Creating ArcGIS Images Using R"
author: "HARP"
date: "August 1, 2018"
output:
  html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
This tutorial is meant to step you through accessing data that is stored in a file somewhere on your computer, and from online sources. It also serves as a brief introduction to modifying your data and using the plot command.  

## Accessing local data and using the plot command
Let's start by finding the folder that you have stored this tutorial in. Odds are it is in your downloads folder, and the filepath looks something like this: "C:\\\\Users\\\\Username\\\\Downloads\\\\Access_and_Plot".  

Within that folder are two csv files, Model and Gage. We are going to open these files in RStudio using the `read.csv()` command. Go ahead and open a new script in R.    
<br>

#### Copy Entire File Path
First we'll open the gage csv manually. This means that we are going to paste in the entire path name where the csv is stored. Copy the following line into your script, replacing the filepath with yours. Remember to include either double backslashes (\\\\) or single forward slashes (/) within the path. 
```{r, echo=FALSE}
USGS_gage <- read.csv('C:\\Users\\Kelsey\\Downloads\\Access_and_Plot\\Gage.csv')
```

```{r, echo=TRUE, eval=FALSE}
USGS_gage <- read.csv('C:\\Users\\Person\\Downloads\\Access_and_Plot\\Gage.csv')
```
Now we're going to load the model data that is stored in the same location, but this time we are going to automate the process a little. Here, we'll use the `paste0` command to make our lives easier, rather than having to paste the whole file path every single time. This is particularly helpful when loading lots of data from one file, or when creating multiple outputs that must be stored in one location. 
<br>
  
  #### Create File Path Variable for pasting 
  Let's start off by looking at the paste0 command. Copy the path that you just used above, and we'll store it in a variable called `file_path`. We'll also play with some of the modifications you can make to a string with `paste0`: 
```{r, collapse=TRUE}
file_path <- 'C:\\Users\\Person\\Downloads\\Access_and_Plot'

paste0(file_path)
paste0(file_path, 'This is cool')
paste0(file_path, ' ', 2+2, ' ', 'paste0 is your friend')
paste0(file_path, '\\What am I doing')
```
<br>
So now that we understand how to use paste0, lets read the model data csv:
```{r, echo=FALSE, eval=TRUE}
file_path<- 'C:\\Users\\Kelsey\\Downloads\\Access_and_Plot'
```

```{r, echo=TRUE}
model_flow <- read.csv(paste0(file_path, '\\Model.csv'))
```
<br>
We can look at our outputs now. To keep it simple, we'll just look at the first three rows from each dataframe at the moment. And that's it: you've pulled in a csv file! 
  ```{r, collapse=TRUE}
USGS_gage[1:3,]; #Display Gage flow
model_flow[1:3,]; #Display Model Flow
```
<br>
  <br>
  
  ## Pulling Data from an online source
  Now that we've learned how to pull in locally stored data, we can move on to pulling it from a database online, such as NWIS. To do so, you'll first need to install the required packages. For this tutorial, we'll use the `dataRetrieval` package to load some hydrologic data. The first time you use any package on your computer, you'll need to use the `install.packages("")` command to download the package in R (any other time you use the package you can skip installation). You can do this in your console, without having to clutter the beginning of your script with this line.

Next, you'll need to call the library at the beginning of your script using `library()`. The first few lines of a code are usually a list of libraries that are used somewhere within the script. Let's go ahead and do these steps. *Note that libraries in R are case sensitive, and that you must put quotes around the package when you install it. 

`install.packages("dataRetrieval")`
<br>
  `library(dataRetrieval)`

<br>
  
  If all goes well, the package will be successfully unpacked and loaded for use. That being said, sometimes you may get an error stating that another package is required for installation. If that happens, follow the same steps to install the required package, and R will *probably* not hate you. Now let's get into playing with dataRetrieval. 


To get a better understanding of this package, let's go to its vignette by typing `??dataRetrieval` in the console. You'll see the Help window display search results for this package. Click on the first option that says `dataRetrieval::dataRetrieval`. You can also Google "dataRetrieval package in R" to pull up a pdf version of this page, which is sometimes easier to read. 


Let's pull daily flow data for a USGS streamgage near Blacksburg. USGS gage 03171000 is located in Radford on the New River, so let's pull that gage's data. By looking at the dataRetrieval vignette, can you see what command would help us pull daily data for this gage? If you said `readNWISdv`, you'd be correct. You can also see what inputs are required to load our data. 

```{r, echo=TRUE, warning=FALSE}
library(dataRetrieval)
gage_id <- '03171000'
startDate <- '2017-01-01'
endDate <- '2018-01-01'
pCode <- '00060' #00060 is discharge in cfs 
statCd <- '00003' #00003 is daily mean data
USGS_flow <- readNWISdv(gage_id, pCode, startDate, endDate, statCd)

```
So let's see what command did for us - we're going to open up USGS_flow, which you can do a few different ways. Either type `View(USGS_flow)` in your console, or open the variable from your environment. You'll see the agency code, site number, date, flow code (00060_00003 means daily mean discharge data - it references the pCode and statCd you entered. You can search the USGS website for other parameter and statistic codes), and the type of data that it is (Approved, Approved estimate, Provisional, etc.). 

This looks junky, so we'll also go ahead and rename our columns to something more descriptive: 
```{r}
colnames(USGS_flow) <- c('Agency', 'Site No', 'Date', 'Flow', 'Code')
```


Now you've done it! You've pulled in flow data from NWIS, which can be used to generate hydrographs and analyze other flow statistics. Let's move on to some more data modifications that we need to make.  

<br>
  <br>
  
  ## Data Modifications and Structure checking
  It is always important to check the structure the data that you are working with. For example, in the previously loaded model and gage data, we have date columns in both our gage and model data frames that are not in the proper date format. Lets analyze this to find out what they are: 
  
  ```{r, collapse=TRUE}
str(USGS_gage$Date)
str(model_flow$date)
```
We see that both of these date columns are in fact listed as being factors. Before we plot our data, we need to make sure that we use `as.Date()` to properly format it. When we do an `str()` on these columns again, we see the format has been changed. 
```{r, collapse=TRUE}
USGS_gage$Date <- as.Date(USGS_gage$Date)
model_flow$date <- as.Date(model_flow$date)
str(USGS_gage$Date)
str(model_flow$date)
```
<br>
  <br>
  
  ## Creating Plots
  Let's get to some basic plotting. Though the `plot()` command is helpful for quick analyses, its best to get comfortable with ggplot2 as soon as you understand the basics of plotting here. In general, plotting should be done as follows:
<br>

`plot(x, y, type = '', col='', lwd='', cex='', xlim='', ylim='', log = '', xlab = '', ylab = '', axes = TRUE)` 
<br>

Note, you don't always need to specify everything mentioned here. Search plot in the help window, or look in the graphics package for more information.

Let's start by plotting just the USGS gage data:
```{r, echo=TRUE, fig.height=5, fig.width=8}
plot(USGS_gage$Date, USGS_gage$Flow, type='l', col='blue', lwd='.8', xlab='Date', ylab='Flow (cfs)' )
```

We see a basic hydrograph for a 21 year flow regime. We can add much more to this, including a legend, and the ability to plot multiple lines at once. 
```{r, echo=TRUE, fig.height=5, fig.width=8}
plot(USGS_gage$Date, USGS_gage$Flow, type='l', col='blue', lwd=.9, xlab='Date', ylab='Flow (cfs)' , ylim = c(0,10000) )
lines(model_flow$date, model_flow$mod.flow, type='l', col='red', lwd=.9)
legend('topright', legend=c("USGS Flow", "Model Flow"),
col=c("blue", "red"), lty=1, cex=0.8)
```

Here is a list of a few of the things you can do to modify this plot (Go ahead and try some of these on your own) 
<br>

Plot Command | What it does | Ways to change it|
------------- | ------------- | -------------
lwd=.9 | changes line weight | change number | 
cex=1 | changes point size or font size | plot: change number, legend: change scale |
legend('') | changes legend position | specify a position or coordinates | 
lty=1 | changes line type in legend | change number for different types |
type='l' | specifies line type |p, l, b, c, o, h, n |
col='blue' | changes line color | type colors() in console for full list| ylim = c(0,10000) | changes ymin and max | change number, same for x axis| 
<br>
<br>


