#Visualizations

library(plotly)
library(gapminder)
#animation bubble plot
#Victim count across several regions in the world in a span of 50 years
dataset <- subset(globalterrorismdb_0617dist, select=c(1,2,3,8,9,10,11,29,30,99))
dataset <- na.omit(dataset)

library(plyr)

count_attack <- aggregate(attacktype1 ~ iyear + country_txt + region_txt, dataset, sum)

bubble <- aggregate(nkill ~ region_txt + country_txt + iyear, dataset ,sum)

a <- gapminder %>%
  plot_ly(
    x = ~bubble$iyear, 
    y = ~bubble$nkill, 
    size = ~count_attack$attacktype1, 
    color = ~bubble$region_txt, 
    frame = ~bubble$iyear, 
    text = ~bubble$region_txt, 
    marker=list( size=40 , opacity=0.5),
    colors=c("green","blue","red","orange","yellow","brown", "black", "purple"),
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list( type = "log", title="Years" ),
    yaxis = list(range=c(0,6000), title="Number of persons killed"),
    legend = list(font = list(family = "sans-serif",
      size = 16,
      color = "#000"))
  ) %>%
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

a
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link_bubble = api_create(p, filename="animations/mulitple-trace")
chart_link_bubble

#---------------------------------------------------------------------------------------------------------
#GEOREFERENTIAL analysis
#scatter map plots

scatter_data <- subset(globalterrorismdb_0617dist, select=c(9,14,15,30))
success_data <- subset(globalterrorismdb_0617dist, select=c(9,14,15,27))

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoic3dhdGluaCIsImEiOiJjajl3cGVlZ3g3cGkzMnFwYTRuZXAxZHN0In0.JJh8U5usbS3brD4PSy0-3g')

#scatter plot of terrorist activity differentiated by the TYPE OF ATTACK
p <- scatter_data %>%
  plot_mapbox(lat = ~latitude, lon = ~longitude,
              split = ~attacktype1_txt, size=2,
              mode = 'scattermapbox', hoverinfo='name') %>%
  layout(title = 'Terrorist attacks by class ',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         marker=list( size=20 , opacity=0.5),
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))

p
#scatter plot of terrorist activity differentiated by the SUCCESS OF ATTACK
r <- success_data %>%
  plot_mapbox(lat = ~latitude, lon = ~longitude,
              split = ~success, size=2,
              mode = 'scattermapbox', hoverinfo='name') %>%
  layout(title = 'Sctter plot of Terrorist attack by nature of success',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
r
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link_scatter_type_of_attack = plotly_POST(p, filename="mapbox/scatter_attacktype")
chart_link_scatter_type_of_attack
chart_link_success = plotly_POST(r, filename="mapbox/success_of_Attack")
chart_link_success

#------------------------------------------------------------------------------------------------

#A Multiple chart to compare Number of lives lost(line chart) 
# and property damage (As bar chart), with time as x axis and y axis 
# accordingly for the two graphs.

year2016 <- subset(gtd_13to16_0617dist, iyear==2016,select=c(1,2,3,29,30,99))
library(plyr)

count_attack <- count(year2016, "attacktype1_txt")

deadly <- aggregate(nkill ~ attacktype1_txt, year2016 ,FUN = length)
deadly$freq <- count_attack$freq

s <- plot_ly(deadly) %>%
  add_trace(x = ~attacktype1_txt, y = ~freq, type = 'bar', name = 'Frequency of Attack',
            marker = list(color = '#C9EFF9'),
            hoverinfo = "text") %>%
  #text = ~paste(, ' mph')) %>%
  add_trace(x = ~attacktype1_txt, y = ~nkill, type = 'scatter', mode = 'lines', name = 'No of Persons killed', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text") %>%
  #text = ~paste(Temp, '°F')) %>%
  layout(title = 'Terrorist attack damage caused in 2016',
         xaxis = list(title = ""),
         yaxis = list(side = 'left', title = 'Frequency of Attack', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Number of persons killed', showgrid = FALSE, zeroline = FALSE))

s
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link_multichart = plotly_POST(p, filename="multiple/bar_line")
chart_link_multichart

#---------------------------------------------------------------------------------------------------------
#Heatmap time series animation for all countries along 50 years
dataset <- globalterrorismdb_0617dist
#require(plotly)
library(gapminder)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 1.0)

# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator')
)

df <- aggregate(eventid ~ country_txt + iyear, dataset, FUN=length) 
names(df)[3]<-"No_of_incidents"

c <- gapminder %>%
  plot_ly(
    z = ~df$No_of_incidents,
    colors = 'Reds', 
    frame = ~df$iyear, 
    text = ~df$country_txt, 
    # hoverinfo = "text",
    type = 'choropleth',
    locationmode = 'country names',
    locations = ~df$country_txt,
    marker = list(line = l)
  ) %>%
  colorbar(title = 'No_of Incidents',limits=c(0,2000)) %>%
  layout(
    title = 'Heatmap of Number of terrorist incidents',
    geo = g
  )%>%
  layout(
    xaxis = list( type = "log", title="Years" ),
    yaxis = list(title="Number of terrorist attacks"),
    legend = list(font = list(family = "sans-serif",
                              size = 20,
                              color = "#000", title="No_of_Incidents"))
  ) %>%
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

c
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link_heatmap = plotly_POST(c, filename="heatmap/world")
chart_link_heatmap