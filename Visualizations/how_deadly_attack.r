library(plotly)

year2016 <- subset(gtd_13to16_0617dist, iyear==2016,select=c(1,2,3,29,30,99))
library(plyr)

count_attack <- count(year2016, "attacktype1_txt")

deadly <- aggregate(nkill ~ attacktype1_txt, year2016 ,FUN = length)
deadly$freq <- count_attack$freq

#airquality_sept$Date <- as.Date(paste(airquality_sept$Month, airquality_sept$Day, 1973, sep = "."), format = "%m.%d.%Y")

s <- plot_ly(deadly) %>%
  add_trace(x = ~attacktype1_txt, y = ~freq, type = 'bar', name = 'Frequency of Attack',
            marker = list(color = '#C9EFF9'),
            hoverinfo = "text") %>%
            #text = ~paste(, ' mph')) %>%
  add_trace(x = ~attacktype1_txt, y = ~nkill, type = 'scatter', mode = 'lines', name = 'No of Persons killed', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text") %>%
            #text = ~paste(Temp, '°F')) %>%
  layout(title = 'terrorist attack damage caused in 2016',
         xaxis = list(title = ""),
         yaxis = list(side = 'left', title = 'Frequency of Attack', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Number of persons killed', showgrid = FALSE, zeroline = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="multiple/bar_line")
chart_link