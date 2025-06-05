library(dataRetrieval)
#shenandoah (01634000)
#rappahannock (01668000)
#potomac (01646500)
#vail (09066325)

#IMPORT SHENANDOAH DATA
shenandoah_data <- readNWISdata(sites = "01634000", 
             parameterCd = "00060",
             startDate = "1990-01-01", 
             endDate = "2020-12-31")

#SHENANDOAH SQL STUFF
library(sqldf)
shenandoah_sql <- sqldf(
  "Select dateTime, x_00060_00003 as Discharge_cfs
  from shenandoah_data
  "
)

#SHENANDOAH PLOT
library(ggplot2)
shenandoah_plot <- ggplot(data = shenandoah_sql,
             aes(dateTime, Discharge_cfs)) +
  geom_line()
shenandoah_plot
#revised
shenandoah_plot <- shenandoah_plot +
  theme_minimal(base_size = 14) +
  geom_line(color = "steelblue", size = 1) +
  xlab("Date") +
  ylab("Discharge (cfs)") +
  ggtitle("N F Shenandoah River Near Strasburg, VA")
shenandoah_plot

#IMPORT RAPPAHANNOCK DATA
rappahannock_data <- readNWISdata(sites = "01668000",
                                  parameterCd = "00060",
                                  startDate = "1990-01-01",
                                  endDate = "2020-12-31")

#RAPPAHANNOCK SQL STUFF
rappahannock_sql <- sqldf(
  "Select dateTime, x_00060_00003 as Discharge_cfs
  from rappahannock_data
  "
)

#RAPPAHANNOCK PLOT
rappahannock_plot <- ggplot(data = rappahannock_sql,
             aes(dateTime, Discharge_cfs)) +
  geom_line()
rappahannock_plot
#revised
rappahannock_plot <- rappahannock_plot +
  theme_minimal(base_size = 14) +
  geom_line(color = "steelblue", size = 1) +
  xlab("Date") +
  ylab("Discharge (cfs)") +
  ggtitle("Rappahannock River Near Fredericksburg, VA")
rappahannock_plot

#IMPORT POTOMAC DATA
potomac_data <- readNWISdata(sites = "01646500", 
                                parameterCd = "00060",
                                startDate = "1990-01-01", 
                                endDate = "2020-12-31")

#POTOMAC SQL STUFF
potomac_sql <- sqldf(
  "Select dateTime, x_00060_00003 as Discharge_cfs
  from potomac_data
  "
)

#POTOMAC PLOT
potomac_plot <- ggplot(data = potomac_sql,
                            aes(dateTime, Discharge_cfs)) +
  geom_line()
potomac_plot
#revised
potomac_plot <- potomac_plot +
  theme_minimal(base_size = 14) +
  geom_line(color = "steelblue", size = 1) +
  xlab("Date") +
  ylab("Discharge (cfs)") +
  ggtitle("Potomac River Near Wash, DC Little Falls Pump Station")
potomac_plot

#IMPORT VAIL DATA
vail_data <- readNWISdata(sites = "09066325",
                          parameterCd = "00060",
                          startDate = "2010-01-01",
                          endDate = "2010-12-31")

#VAIL SQL STUFF
vail_sql <- sqldf(
  "Select dateTime, x_00060_00003 as Discharge_cfs
  from vail_data
  "
)

#VAIL PLOT
vail_plot <- ggplot(data = vail_sql,
                       aes(dateTime, Discharge_cfs)) +
  geom_line()
vail_plot
#revised
vail_plot <- vail_plot +
  theme_minimal(base_size = 14) +
  geom_line(color = "steelblue", size = 1) +
  xlab("Date") +
  ylab("Discharge (cfs)") +
  ggtitle("Gore Creek Discharge at Vail, CO (2010)")
vail_plot