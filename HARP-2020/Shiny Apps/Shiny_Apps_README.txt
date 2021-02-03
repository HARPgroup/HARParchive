R Shiny App Descriptions

Heatmap_app.R - This app generates an unmet demand heatmap based on user-inputted values 
for facility pid, elid, runid, start date, and end date.

Natty App.R - This app generates USGS gage plots, unmet demand vahydro plots, and elfgen plots based on various user inputs such as,
USGS site id and vahydro facility pid, eild, and runid.

Kyle_shiny.R - This shiny app creates an interactive dashboard where the user can input which graph they want to see 
(from the two in watersupplyelement: either the unmet demand heatmap, or Lal's critical period flow graph). 
Then, takes inputs for the desired runid, pid, and elid of the vahydro facility. The output is the plot at the desired facility 
identified by the element id and PID The default facility is the Shenandoah Town Water Treatment Plant

USGS_flow_graph_shiny.R - The app shows a hydrograph of the streamflow for a given USGS gage over a given time period.
 The user inputs variables such as the desired USGS site ID, the start date, and the end date, to get their desired hydrograph.

threshold_flow_heatmap_shiny.R - The app shows a heatmap of the number of days per month in which the USGS gage in question has a
 flow that is less than the threshold flow. THe user input variables are the USGS gage id, start date, end date, and threshold flow.
