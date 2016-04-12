# intplot  
## INTERACTIVE PLOT OF LON - LAT AND A VARIABLE DATA
 
 *Created by Sven Gastauer 2016*  
 *contact: sven.gastauer@gmx.net*  
   
 Dependencies: shiny, ggplot2, ggmap, Cairo, colorRamps, RColorBrewer,DT  
  
###Function input:

- dataset = name of the dataset to be used e.g. speed_data   
- Lon = [character] name of the Longitude variable, default =  "Longitude", class= character, e.g. "Lon"  
- Lat = [character] name of the Latitude variable, default = "Latitude", class = character, e.g."Lat"  
- variable = [character] name of the variable to define the color scale on the maps (must be a numeric variable), class= numeric, e.g. "Speed"  
- mapoffset = [numeric] degrees which will be added to the min/max longitude/latitude to make the map more readable, default=1, class=numeric  
- mapsource =  [character] source of the background map as defined by ggmap, default = "stamen", options are: Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"), or CloudMade maps ("cloudmade")  
- maptype = [character] type of the map as defined by ggmap, default = "toner", options are: "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps), "terrain", "watercolor", and "toner" (stamen maps), or a positive integer for cloudmade maps (see ?get_cloudmademap)  
- colour = [character] colour of the background map as defined by ggmap, default = "bw", can be either colour ("color") or black-and-white ("bw")  

#### Examples    
maximal example:  
`source('intplot.R')  
intplot(mydata, "Lons","Lats","coolvariable", 0.5, "google", "terrain","color")`
minimal example:  
`intplot(mydata,variable="variable") #assuming mydata contains columns Longitude, Latitude, variable `

Result example:  
![alt text](https://github.com/SvenGastauer/intplot/blob/master/intplot.png "Results example")

Please notify the author if results of this function are used in any sort of publication  
For contact, suggestions or support contact the author  
