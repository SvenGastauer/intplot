###################################################################################
# INTERACTIVE PLOT OF LON - LAT AND A VARIABLE DATA
# 
# created by Sven Gastauer 2016
# contact: sven.gastauer@gmx.net
# 
# Dependencies: shiny, ggplot2, ggmap, Cairo, colorRamps, RColorBrewer,DT
# 
# Function input:
# dataset = name of the dataset to be used e.g. speed_data 
# Lon = [character] name of the Longitude variable, default =  "Longitude", class= character, e.g. "Lon"
# Lat = [character] name of the Latitude variable, default = "Latitude", class = character, e.g."Lat"
# variable = [character] name of the variable to define the color scale on the maps (must be a numeric variable), class= numeric, e.g. "Speed"
# mapoffset = [numeric] degrees which will be added to the min/max longitude/latitude to make the map more readable, default=1, class=numeric
# mapsource =  [character] source of the background map as defined by ggmap, default = "stamen", options are: Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen"), or CloudMade maps ("cloudmade")
# maptype = [character] type of the map as defined by ggmap, default = "toner", options are: "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps), "terrain", "watercolor", and "toner" (stamen maps), or a positive integer for cloudmade maps (see ?get_cloudmademap)
# colour = [character] colour of the background map as defined by ggmap, default = "bw", can be either colour ("color") or black-and-white ("bw")
#
# maximal example: intplot(mydata, "Lons","Lats","coolvariable", 0.5, "google", "terrain","color")
# minimal example: intplot(mydata,"variable") #assuming mydata contains columns Longitude, Latitude, variable
#
# Please notify the author if results of this function are used in any sort of publication
# For contact, suggestions or support contact the author
###################################################################################

intplot <- function(dataset,Lon="Longitude",Lat="Latitude",variable,
                    mapoffset=1,mapsource="stamen",
                    maptype="toner",colour="bw"){

  message(paste(" intplot is using ", deparse(substitute(dataset)),
                "\n Longitude variable is called",Lon,
                "\n Latitude variable is called",Lat,
                "\n mapinng offset is set to", mapoffset,
                "\n background map uses", mapsource,
                "\n the map type is", maptype,
                "\n the map is in ", colour))
  
  #Get lon lat limits
  lons <- range(na.omit(dataset[[Lon]]))
  lons[1]<-floor(lons[1])-mapoffset
  lons[2]<-ceiling(lons[2])+mapoffset
  lats <- range(na.omit(dataset[[Lat]]))
  lats[1]<-floor(lats[1])-mapoffset
  lats[2]<-ceiling(lats[2])+mapoffset
#download map data
  message("Getting background map...")
aus <- get_map(location = c( lon = mean(dataset[[Lon]]), lat = mean(dataset[[Lat]])),
               source=mapsource, color=colour, zoom=6, maptype=maptype)
message("Background map loaded...")
#generate gmap plot
g <- ggmap(aus) +
  geom_point(data=dataset,aes_string(x=Lon,y=Lat, color=variable,group=1),size=2)+ 
  scale_color_gradientn(colours=matlab.like(10))

#Select box x value
selval <- list()
selval[names(dataset)]=names(dataset)
numval<-list()
numval[names(dataset[,sapply(dataset, is.numeric)])]=names(dataset[,sapply(dataset, is.numeric)])


#######################################################################################################
##    SHINY PART
########################################################################################################

#start page design
ui <- fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           h4("Vessel variable"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot2", height = 500,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    plotOutput("plot3", height = 500,
                               dblclick = "plot3_dblclick",
                               click = "plot_click",
                               hover = hoverOpts(id = "plot_hover"),
                               
                               brush = brushOpts(
                                 id = "plot3_brush",
                                 resetOnNew = TRUE
                               )
                    )
             )
           )
    )
    
  ),
  #   fluidRow(
  #     column(width = 3,
  #            verbatimTextOutput("click_info")
  #     ),
  #     column(width = 3,
  #            verbatimTextOutput("dblclick_info")
  #     ),
  #     column(width = 3,
  #            verbatimTextOutput("hover_info")
  #     ),
  #     column(width = 3,
  #            verbatimTextOutput("brush_info")
  #     )
  #   ),
  #   
  fluidRow(
    column(width = 9,
           h4("Selected Points:"),
           dataTableOutput("plot_brushed_points"))),
  fluidRow(column(width = 3,
                  selectInput("select_x", label = h3("Select X variable"), 
                              choices = selval),
                  selectInput("select_y", label = h3("Select y variable"), 
                              choices = selval),
                  selectInput("select_col", label = h3("Select colour variable"), 
                              choices = c("1",selval)),
                  selectInput("select_size", label = h3("Select size variable"), 
                              choices = c("1",numval))),
           column(width = 9,
                  h4("More Plots:"),
                  plotOutput("moreplots1",height = 500)
                  
                  #     ),
                  #     column(width = 3,
                  #            verbatimTextOutput("plot_brushinfo")
           )
  )
) 


server <- function(input, output) {
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  xvar <- "Longitude"
  yvar <- "Latitude"
  
  output$plot2 <- renderPlot({
    ggmap(aus) +
      geom_point(data=dataset,aes_string(x=Lon,y=Lat, color=variable,group=1),size=2)+ 
      scale_color_gradientn(colours=matlab.like(10))+
      coord_cartesian(xlim = lons, ylim = lats)
  })
  
  output$plot3 <- renderPlot({
    ggmap(aus) +
      geom_point(data=dataset,aes_string(x=Lon,y=Lat, color=variable,group=1),size=2)+ 
      scale_color_gradientn(colours=matlab.like(10))+
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  })
  
  output$moreplots1 <- renderPlot({
    ggplot(data=brushedPoints(dataset, input$plot3_brush, "Longitude", "Latitude"),
           aes_string(x=input$select_x,y=input$select_y,col=input$select_col,size=input$select_size)) +
      geom_point(data=brushedPoints(dataset, input$plot3_brush, "Longitude", "Latitude"))+
      scale_color_gradientn(colours=matlab.like(10))+
      theme_bw()
  })
  
  
  #   output$click_info <- renderPrint({
  #     cat("Clicked Point:\n")
  #     str(input$plot_click$x)
  #   })
  #   output$hover_info <- renderPrint({
  #     cat("Mouseover Point:\n")
  #     str(input$plot_hover)
  #   })
  #   
  #   output$plot_brushinfo <- renderPrint({
  #     res <- brushedPoints(dat, input$plot3_brush, "Longitude", "Latitude")
  #     cat("input$plot3_brush:\n")
  #     str(res)
  #   })
  
  output$plot_brushed_points <- renderDataTable({
    brushedPoints(dataset, input$plot3_brush, "Longitude", "Latitude")
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  
  
}

return(suppressWarnings(shinyApp(ui, server)))
}
