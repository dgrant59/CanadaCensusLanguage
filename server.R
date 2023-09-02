library(tidyverse)
library(readxl)
library(magrittr)
library(sf)
library(shiny)
library(shinyjs)
library(bslib)
library(data.table)
library(ggspatial)
library(magrittr)
library(plotly)
library(crosstalk)
library(shinythemes)
library(shinycssloaders)
# library(listviewer)
# library(jsonlite)
library(viridis)
library(colorspace)

popdata_PROV <- read.csv("popdata_PROV.csv",encoding='UTF-8')

lang_order_PROV <- popdata_PROV %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)


popdata_CD <- read.csv("popdata_CD.csv",encoding='UTF-8')

lang_order_CD <- popdata_CD %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_Toronto <- read.csv("popdata_Toronto.csv",encoding='UTF-8')

lang_order_Toronto <- popdata_Toronto %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_KWRegion <- read.csv("popdata_KWRegion.csv",encoding='UTF-8')

lang_order_KWRegion <- popdata_KWRegion %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_London <- read.csv("popdata_London.csv",encoding='UTF-8')

lang_order_London <- popdata_London %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)
###### MAP DATA
#--------------------------------------------------------------------------
canada_PROV <- read_sf(dsn = "./ShapeFiles/PROV Shapefiles/PROVSimplifiedT.shp", 
                       stringsAsFactors = T)              
# 
# #convert projection to standardized projection, WSG84
# canada_PROV <- st_transform(canada_PROV, 4326)
# st_write(canada_PROV,dsn = "./ShapeFiles/PROV Shapefiles/PROVSimplifiedT.shp")
# # 
# # #Join map data to case data for easy plotting
canada_PROV <- right_join(canada_PROV,popdata_PROV, by=c("DGUID"="DGUID"),keep=F) 
# 
#--------------------------------------------------------------------------
canada_CD <- read_sf(dsn = "./ShapeFiles/CD Shapefiles/CDSimplifiedT.shp", 
                     stringsAsFactors = T)              
# 
# #convert projection to standardized projection, WSG84
# canada_CD <- st_transform(canada_CD, 4326)
# st_write(canada_CD,dsn = "./ShapeFiles/CD Shapefiles/CDSimplifiedT.shp")
# 
# #Join map data to case data for easy plotting
 canada_CD <- right_join(canada_CD,popdata_CD, by=c("DGUID"="DGUID"),keep=F) 
# 
#--------------------------------------------------------------------------

canada_Toronto <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/TorontoSimplifiedT.shp", 
                          stringsAsFactors = T)              
# 
# #convert projection to standardized projection, WSG84
# canada_Toronto <- st_transform(canada_Toronto, 4326)
# # 
# st_write(canada_Toronto,dsn = "./ShapeFiles/DA Shapefiles/TorontoSimplifiedT.shp")
# #Join map data to case data for easy plotting
canada_Toronto <- right_join(canada_Toronto,popdata_Toronto, by=c("DGUID"="DGUID"),keep=F) 
# 

canada_KWRegion <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/KWRegionSimplifiedT.shp", 
                           stringsAsFactors = T)              
# 
# #convert projection to standardized projection, WSG84
#canada_KWRegion <- st_transform(canada_KWRegion, 4326)
# st_write(canada_KWRegion,dsn = "./ShapeFiles/DA Shapefiles/KWRegionSimplifiedT.shp")
# 
# #Join map data to case data for easy plotting
canada_KWRegion <- right_join(canada_KWRegion,popdata_KWRegion, by=c("DGUID"="DGUID"),keep=F) 
#

canada_London <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/LondonSimplifiedT.shp", 
                         stringsAsFactors = T)              

# #convert projection to standardized projection, WSG84
#canada_London <- st_transform(canada_London, 4326)
#st_write(canada_London,dsn = "./ShapeFiles/DA Shapefiles/LondonSimplifiedT.shp")
# 
# #Join map data to case data for easy plotting
canada_London <- right_join(canada_London,popdata_London, by=c("DGUID"="DGUID"),keep=F) 


set.seed(20220905)
languages_used <- distinct(rbind(lang_order_PROV,
                                 lang_order_CD,
                                 lang_order_Toronto,
                                 lang_order_London,
                                 lang_order_KWRegion)
                           ,Language)
languages_used %<>% arrange(Language)

### Have to swap around some colours because they don't work well being so similar
#on maps
map_colours_rainbow<- desaturate(rainbow(nrow(languages_used)),0.2)
names(map_colours_rainbow) <- languages_used$Language
map_colours_rainbow[names(map_colours_rainbow)=="Tie (Hover to See Languages)"] <- desaturate("grey30",0.2)
map_colours_rainbow[names(map_colours_rainbow)=="NO DATA"] <- "white"
map_colours_rainbow[names(map_colours_rainbow)=="No non-English/French Speakers"] <- desaturate("grey80",0.2)

map_colours_rainbow[names(map_colours_rainbow)=="Tagalog (Pilipino, Filipino)"] <- "#CAB2D6"
map_colours_rainbow[names(map_colours_rainbow)=="Spanish"] <- desaturate("khaki2",0.2)
map_colours_rainbow[names(map_colours_rainbow)=="Polish"] <- "dodgerblue2"
map_colours_rainbow[names(map_colours_rainbow)=="Portuguese"] <- desaturate("khaki1",0.2)

map_colours_viridis <- viridis(nrow(languages_used))
names(map_colours_viridis) <- languages_used$Language
map_colours_viridis[names(map_colours_viridis)=="Tie (Hover to See Languages)"] <- desaturate("grey30",0.2)
map_colours_viridis[names(map_colours_viridis)=="NO DATA"] <- "white"
map_colours_viridis[names(map_colours_viridis)=="No non-English/French Speakers"] <- desaturate("grey80",0.2)

colourswap1 <- map_colours_viridis[names(map_colours_viridis)=="American Sign Language"]
colourswap2 <- map_colours_viridis[names(map_colours_viridis)=="Cree, n.o.s."]
colourswap3 <- map_colours_viridis[names(map_colours_viridis)=="Slavey, n.o.s."]


map_colours_viridis[names(map_colours_viridis)=="American Sign Language"] <- map_colours_viridis[names(map_colours_viridis)=="Tagalog (Pilipino, Filipino)"]
map_colours_viridis[names(map_colours_viridis)=="Cree, n.o.s."] <- map_colours_viridis[names(map_colours_viridis)=="Spanish"]
map_colours_viridis[names(map_colours_viridis)=="Slavey, n.o.s."] <- map_colours_viridis[names(map_colours_viridis)=="Polish"]

map_colours_viridis[names(map_colours_viridis)=="Tagalog (Pilipino, Filipino)"] <- colourswap1
map_colours_viridis[names(map_colours_viridis)=="Spanish"] <- colourswap2
map_colours_viridis[names(map_colours_viridis)=="Polish"] <- colourswap3

#-------------------------------------------------------------------------------
map_theme <- theme(legend.title.align = 0.5,
                   axis.line = element_blank(),  #bunch of options to remove "graph" visuals
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_blank())

bar_theme <- theme(legend.title.align = 0.5,
                   axis.line = element_blank(),  #bunch of options to remove "graph" visuals
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.ticks.length = unit(0, "pt"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   plot.title = element_text(size=12),
                   legend.position = 'none',
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   legend.title = element_blank())
function(input,output,session){
  
  #Tells graphs what palette to use based on radio button selection
  rainbowviridis <- function(){
    if(input$colourradio==2){
      map_colours_viridis
    }
    else{
      map_colours_rainbow
    }
  } 
  
  # output$map_PLACE <- renderPlotly({
  #   ggplotly(
  #     ggplot(canada_PLACE) + 
  #
  #This is the backgroun map that appears behind a region when you deselect it
  #White fill, black border, borderlines 0.03
  #       geom_sf(data=canada_PLACE, fill="white",color="black",lwd=0.03)+
  #
  #This is the actual map with information on it. Fill is the discrete language
  #variable, and borderlines are 0.1 
  #
  #       geom_sf(aes(fill=Language),
  #               lwd=0.1)+ 
  #
  #Customizing the fill of the above map layer, we use the rainbowviridis() function
  #which will output the correct palette based on the radio buttons. We select only
  #unique(canada_PLACE$Language) because map_colours_X is a dictionary that contains
  #colours for every region that is mapped across all 5 maps. If we do not filter to
  #unique(canada_PLACE$Language), the legend would contain a bunch of unused (on this map)
  #languages
  #
  #       scale_fill_manual(values=rainbowviridis()[unique(canada_PLACE$Language)])+
  #
  #This adds a geom point somewhere guaranteed to be within each polygon. It too
  #will be filled with the colour assigned to Language, and it will also be
  #scaled based on the % of the region that language's speakers make up.
  #The "text" attribute defines what appears when you hover over each point in the
  #info box (\n makes a new line). Stroke defines the border around each point
  #do not show legend because this would add onto the existing legend and give each
  #language a line and a point entry in the legend.
  #
  #       stat_sf_coordinates(
  #         aes(fill=Language,
  #             size=C10_RATE_TOTAL,
  #             text=paste("Dissemination Area (DA):",DAUID,
  #                        "\nLanguage(s):",Languages,
  #                        "\n% of CD Population:",C10_RATE_TOTAL,
  #                        "\nData Quality Flag:", DATA_QUALITY_FLAG)),
  #         stroke=0.2,
  #         show.legend = F)+
  #
  #
  #deals with the aspect ratio of the plot, twice as tall as wide
  #       coord_fixed(2)+
  #
  #
  #add map theme as defined above, make sure the "text" attribute shows up on hover
  #and do not allow clicking on the map to do anything.
  
  #       map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  # })
  # 
  #This is for the language bar
  #
  # output$langbar_Toronto <- renderPlotly({
  #   ggplotly(
  #
  #Since this plot is rotated, y will be the languages and x will be the count of 
  #each language
  #
  #     ggplot(canada_PLACE,aes(y=Language,fill=Language))+
  #       scale_fill_manual(values=rainbowviridis()[unique(canada_PLACE$Language)])+
  #       geom_bar(aes(text=Language))+
  #       geom_text(
  #         aes(label = Language), 
  #         stat = "count", 
  #         hjust="center", 
  #
  #nudge_x is a way to move the labels to the right of the bar so they do not intersect it
  #and look ugly. Since each language has a different number of characters, each
  #text label will need to move a different amount. These numbers are all basically 
  #trial and error.
  #
  #         nudge_x =strwidth(sort(unique(canada_PLACE$Language)),font=3,units="in")*85, 
  #         colour = "black",
  #         size=3)+
  #       scale_y_discrete(limits = lang_order_Toronto$Language)+
  #       labs(x = NULL, y = NULL)+
  #
  #add extra x axis room for label to appear beyond bar
  #
  #       scale_x_continuous(limits=c(0,max(table(canada_PLACE$Language))+500),
  #                          expand = c(0,0)) +
  #
  #show count and language on hover
  #
  #       bar_theme,tooltip = c("text","x")) %>% 
  #
  #
  #
  #     layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
  #
  # Text labels are also classified as a "trace" and would have an on hover effect
  #this is obviously unwanted, so I have disabled the hover effects above each
  #language label
  #
  #     style(hoverinfo = "none", traces = c(55:107))
  # })
  
  output$map_PROV <- renderPlotly({
    ggplotly(
      ggplot(canada_PROV) +
        geom_sf(data=canada_PROV, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1
        ) + 
        scale_fill_manual(values=rainbowviridis()[unique(canada_PROV$Language)])+
        stat_sf_coordinates(aes(
          fill=Language,
          size=C10_RATE_TOTAL,
          text=paste("Province/Territory:",PRNAME,
                     "\nLanguage(s):",Language, 
                     "\n% of Prov/Terr Population:",C10_RATE_TOTAL,
                     "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text")
    ) %>% layout(clickmode="none")%>% config(scrollZoom = TRUE)
  })
  
  output$langbar_PROV <- renderPlotly({
    ggplotly(
      ggplot(canada_PROV,aes(y=Language, fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_PROV$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_PROV$Language)),font=5,units="in")/1.5, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_PROV$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_PROV$Language))+5),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")
    ) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(9:16))
  })
  
  
  output$map_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD) +
        geom_sf(data=canada_CD, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1
        )+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_CD$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Census Division (CD):",CDNAME,
                         "\nLanguage(s):",Languages, 
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")%>% config(scrollZoom = TRUE)
  })
  
  output$langbar_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_CD$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), 
                  stat = "count",
                  hjust="center", 
                  nudge_x =strwidth(sort(unique(canada_CD$Language)),font=3,units="in")*12, 
                  colour = "black",
                  size=3)+
        scale_y_discrete(limits = lang_order_CD$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_CD$Language))+20),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")
    ) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(36:70))
  })
  
  output$map_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto) +
        geom_sf(data=canada_Toronto, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_Toronto$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")%>% config(scrollZoom = TRUE)
  })
  
  output$langbar_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_Toronto$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_Toronto$Language)),font=3,units="in")*85, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_Toronto$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_Toronto$Language))+500),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(55:107))
  })
  
  output$map_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion)+
        geom_sf(data=canada_KWRegion, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_KWRegion$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")%>% config(scrollZoom = TRUE)
  })
  
  output$langbar_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_KWRegion$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_KWRegion$Language)),font=3,units="in")*30, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_KWRegion$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_KWRegion$Language))+180),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>%
      style(hoverinfo = "none", traces = c(40:77))
  })
  output$map_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London) +
        geom_sf(data=canada_London, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_London$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")%>% config(scrollZoom = TRUE)
  })
  
  output$langbar_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_London$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x = strwidth(sort(unique(canada_London$Language)),font=3,units="in")*20, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_London$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_London$Language))+150),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(31:60))
  })
  outputOptions(output, "map_PROV", suspendWhenHidden = FALSE)
  outputOptions(output, "langbar_PROV", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_CD", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_CD", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_Toronto", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_Toronto", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_KWRegion", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_KWRegion", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_London", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_London", suspendWhenHidden = FALSE)
}
