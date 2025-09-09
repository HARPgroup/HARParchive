library(dplyr)
library(lubridate)

##EDIT THESE (this is currently just an example)##
usgs_csv  <- "C:/HARPgeneral/strasburg_usgs_flow.csv"
model_csv <- "C:/HARPgeneral/6.13meeting/Strasburg_PS3_5100_5080_results.csv"
out_csv   <- "C:/HARPgeneral/6.13meeting/model_trim_like_usgs.csv"
use_intersection_if_empty <- TRUE
agency_tag <- "MODEL"   # or "VAHYDRO"
flow_cd    <- "M"

##USGS: keep site_no as string; get date window##
usgs <- read.csv(usgs_csv, stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = c(site_no = "character"))
if ("" %in% names(usgs)) usgs <- usgs[ , names(usgs) != ""]
stopifnot("Date" %in% names(usgs), "site_no" %in% names(usgs))
usgs$Date <- as.Date(usgs$Date)
usgs_site <- unique(usgs$site_no)[1]
usgs_min  <- min(usgs$Date, na.rm = TRUE)
usgs_max  <- max(usgs$Date, na.rm = TRUE)
cat("USGS site:", usgs_site, "| window:", format(usgs_min), "→", format(usgs_max), "\n")

##MODEL: read + build Date from (year,month,day) or (year,jday)##
mdl <- read.csv(model_csv, stringsAsFactors = FALSE, check.names = FALSE)
nm <- names(mdl); bad <- which(is.na(nm) | nm == ""); if (length(bad)) names(mdl)[bad] <- paste0("col", bad)

#pick flow column
flow_candidates <- intersect(c("Qout","Qdout","Flow","flow","value","val","flow_cfs","discharge_cfs","runoff_cms","Qsim"), names(mdl))
if (!length(flow_candidates)) stop("Could not find a flow-like column in model CSV.")
fcol <- flow_candidates[1]

#build Date
if (all(c("year","month","day") %in% names(mdl))) {
  Date <- as.Date(sprintf("%04d-%02d-%02d", as.integer(mdl$year), as.integer(mdl$month), as.integer(mdl$day)))
} else if (all(c("year","jday") %in% names(mdl))) {
  #jday = day-of-year (1..365/366)
  Date <- as.Date(as.integer(mdl$year) * 1000 + 1, format = "%Y%j") + (as.integer(mdl$jday) - 1)
} else {
  #last resort (backup just in case): try typical date cols
  date_candidates <- intersect(c("Date","date","datetime","timestamp","Timestamp","time","Time"), names(mdl))
  if (!length(date_candidates)) stop("No (year,month,day) or (year,jday) and no standard date column found.")
  Date <- suppressWarnings(as.Date(substr(as.character(mdl[[date_candidates[1]]]), 1, 10)))
}

Flow <- suppressWarnings(as.numeric(gsub("[^0-9eE+\\-\\.]", "", as.character(mdl[[fcol]]))))

model_clean <- data.frame(Date = Date, Flow = Flow) %>%
  filter(!is.na(Date) & is.finite(Flow)) %>%
  arrange(Date)

if (!nrow(model_clean)) stop("Model parsed to 0 usable rows after constructing Date and Flow.")

mdl_min <- min(model_clean$Date); mdl_max <- max(model_clean$Date)
cat("MODEL window:", format(mdl_min), "→", format(mdl_max), "| n =", nrow(model_clean), "\n")

##trim to USGS window##
trim_exact <- model_clean %>% filter(Date >= usgs_min, Date <= usgs_max)

trim_final <- trim_exact
if (!nrow(trim_final) && use_intersection_if_empty) {
  inter_min <- max(usgs_min, mdl_min)
  inter_max <- min(usgs_max, mdl_max)
  if (inter_min <= inter_max) {
    cat("No rows in exact USGS window; using INTERSECTION:", format(inter_min), "→", format(inter_max), "\n")
    trim_final <- model_clean %>% filter(Date >= inter_min, Date <= inter_max)
  }
}
if (!nrow(trim_final)) stop("No overlap between USGS window and model data.")

cat("Rows after trim:", nrow(trim_final), "\n")

##Match USGS schema and write##
out_df <- trim_final %>%
  transmute(
    agency_cd = agency_tag,
    site_no   = usgs_site,   # keep same id for downstream code that expects one
    Date      = Date,
    Flow      = Flow,        # convert units here if model isn't in cfs
    Flow_cd   = flow_cd
  )

write.csv(out_df, out_csv, row.names = FALSE)
cat("Wrote:", out_csv, "\n")
