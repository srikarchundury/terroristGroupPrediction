dataset <- globalterrorismdb_0617dist
require(plotly)
library(gapminder)

df <- aggregate(eventid ~ country_txt, dataset, FUN=length) 
 
# light grey boundaries
l <- list(color = toRGB("grey"), width = 1.0)

# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator')
)


q <- plot_geo(df) %>%
  add_trace(
    z = ~df$eventid, colors = 'Purples', locationmode = 'country names',
    text = ~df$country_txt, locations = ~df$country_txt, marker = list(line = l)
  ) %>%
  colorbar(title = 'No_of Incidents',limits=c(0,3000)) %>%
  layout(
    title = 'Heatmap of Number of terrorist incidents<br>Source:<a href="https://www.start.umd.edu/gtd/">Global Terrorism Databse</a>',
    geo = g
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

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link_heatmap = plotly_POST(q, filename="heatmap/world")

chart_link_heatmap