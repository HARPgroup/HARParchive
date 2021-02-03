DesignStormsV2.0.R
This script was written to interact with the web services of the USGS StreamStat program. By feeding an input csv 
file (see Input_example.csv for formatting), this script takes latitude and longitude of a given location, feeds it
to StreamStat, and develops the drainage area and suspected design flows (from various USGS regressions). In this manner,
the script is extremely helpful in impoundment design. By inputting the location of an impoundment outlet like in the example file,
you can get back the drainage area, design flow, and preliminary outlet dimensions (assuming a rectangular orifice). 

NOTE: If you encounter an HTTP error (500), try running the script again. If this fails, navigate manually to the json_file URL in a web
	browser to ensure data is there. If it is, try running the script again. For some reason, StreamStat web services do not like being
	accessed by automated routines (it seems to be sluggish in responding to Query IDs it creates). 
	You may have to manually set i for each loop iteration and run the loop manually. Alternativley, you can use the code labeled "Alternative
	Method" to manually calculate flows. This will prevent the need for manual looping but will limit you to the calculations I've put in there
	(https://pubs.usgs.gov/sir/2014/5090/pdf/sir2014-5090.pdf)