#install.packages("plotly")
library(plotly)




Sys.setenv("plotly_username"="al_uk")
Sys.setenv("plotly_api_key"="")

library(plotly)
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
api_create(p, filename = "r-docs-midwest-boxplots")