suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))

# Load in Final Data from Github
# 2018-2023
data_18 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/2018_2023_final_data", colClasses = c(gage = "character"))

# 2013-2017
data_13 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/2013_2017_final_data", colClasses = c(gage = "character"))

# 2008-2012
data_08 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/2008_2012_final_data", colClasses = c(gage = "character"))

# 2003-2007
data_03 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/2003_2007_final_data", colClasses = c(gage = "character"))

# 1998-2002
data_98 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/1998_2002_final_data", colClasses = c(gage = "character"))

# 1993-1997
data_93 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/1993_1997_final_data", colClasses = c(gage = "character"))

# 1988-1992
data_88 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/1988_1992_final_data", colClasses = c(gage = "character"))

# 1983-1987
data_83 <- read.csv("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/5year_CV_plots_and_data/1983_1987_final_data", colClasses = c(gage = "character"))


combined_data <- sqldf(
  "select * from data_83 
  union 
  select * from data_88
  union
  select * from data_93
  union 
  select * from data_98
  union
  select * from data_03
  union
  select * from data_08
  union
  select * from data_13
  union 
  select * from data_18
  "
)


find.missing.gage <- function(data) {

  # Set dataset
  data_frame <- data
  
  # Create new columns for if the data is na in each source
  NA_col_data <- sqldf("
  select gage, start_year, end_year,
  case when prism_precip is null then 'no' else 'yes' end as prism_data,
  case when daymet_precip is null then 'no' else 'yes' end as daymet_data,
  case when nldas_precip is null then 'no' else 'yes' end as nldas_data
  from data_frame
  ")
  
  missing_data_col <- sqldf("
  select *,
  case when prism_data is 'no' or 
  daymet_data is 'no' or
  nldas_data is 'no' then 'yes'
  else 'no' end
  as missing_data
  from NA_col_data
  ")

  
  missing_gage_area <- sqldf(
    " select a.gage, a.start_year, a.end_year,
    a.prism_data, a.daymet_data, 
    a.nldas_data, a.missing_data,
    b.area
  from missing_data_col as a
  left outer join data_frame as b
  on (
    a.gage = b.gage and
    a.start_year = b.start_year
  )
  "
  )

  return(missing_gage_area)
  
}

missing_data <- find.missing.gage(combined_data)

missing_gages_count <- sqldf(
  "select gage, count(*)
  from missing_data
  where 
  missing_data = 'yes'
  group by gage
  "
)
