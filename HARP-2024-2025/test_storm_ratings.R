pfile = "http://deq1.bse.vt.edu:81/met/stormVol_prism/out/usgs_ws_01632000-rating-ts.csv"
nfile = "http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/out/usgs_ws_01632000-rating-ts.csv"
dfile = "http://deq1.bse.vt.edu:81/met/stormVol_daymet/out/usgs_ws_01632000-rating-ts.csv"

pratings = read.table(pfile, header = TRUE, sep = ",")
nratings = read.table(nfile, header = TRUE, sep = ",")
dratings = read.table(dfile, header = TRUE, sep = ",")

pratings$yr <- year(as.Date(pratings$start_date))
nratings$yr <- year(as.Date(nratings$start_date))
dratings$yr <- year(as.Date(dratings$start_date))

pann <- sqldf("select yr, avg(rating) as rating, CASE WHEN avg(rating)< 0 THEN 0 ELSE avg(rating) END as nzrating from pratings group by yr order by yr")
nann <- sqldf("select yr, avg(rating) as rating, CASE WHEN avg(rating)< 0 THEN 0 ELSE avg(rating) END as nzrating from nratings group by yr order by yr")
dann <- sqldf("select yr, avg(rating) as rating, CASE WHEN avg(rating)< 0 THEN 0 ELSE avg(rating) END as nzrating from dratings group by yr order by yr")

plot(pann$rating ~ pann$yr)
points(dann$rating ~ dann$yr, pch=15)
points(nann$rating ~ nann$yr, pch=19)

bestfit <- sqldf(
  "select nrat.yr, 
   CASE 
     WHEN drat.rating < nrat.rating AND drat.rating < prat.rating THEN drat.rating
     WHEN prat.rating < nrat.rating AND prat.rating < drat.rating THEN prat.rating
     ELSE nrat.rating
   END as brat,
   CASE 
     WHEN drat.rating < nrat.rating AND drat.rating < prat.rating THEN 'daymet'
     WHEN prat.rating < nrat.rating AND prat.rating < drat.rating THEN 'prism'
     ELSE 'nldas2'
   END as dset
  from nann as nrat 
  left outer join pann as prat 
  on (prat.yr = nrat.yr)
  left outer join dann as drat 
  on (drat.yr = nrat.yr)
  order by nrat.yr
  "
)
