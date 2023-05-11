library(plotly)

scatter_data <- subset(globalterrorismdb_0617dist, select=c(9,14,15,30))
success_data <- subset(globalterrorismdb_0617dist, select=c(9,14,15,27))

  
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoic3dhdGluaCIsImEiOiJjajl3cGVlZ3g3cGkzMnFwYTRuZXAxZHN0In0.JJh8U5usbS3brD4PSy0-3g')
p <- scatter_data %>%
  plot_mapbox(lat = ~latitude, lon = ~longitude,
              split = ~attacktype1_txt, size=2,
              mode = 'scattermapbox', hoverinfo='name') %>%
  layout(title = 'Terrorist attacks by TYPE OF ATTACK ',
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
r <- success_data %>%
  plot_mapbox(lat = ~latitude, lon = ~longitude,
              split = ~success, size=2,
              mode = 'scattermapbox', hoverinfo='name') %>%
  layout(title = 'Scatter plot by Success of attack',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="mapbox/multiple")
chart_link_scatter