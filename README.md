# This repository contains some of the work completed by me and anther student during a visualization course in my master's program.

The code is written in R with mostly using packages like ggplot2, plotly, dplyr, tidyr and shiny.
The work is done in Rmarkdown and html's have been generated, but they could be too large to view here on github. 


*The labs contain various types of graphs and problems, increasing in complexity!*

lab 1: Density plots in ggplot and plotly and a shiny app! <br><br>
lab 2: Scatterplots created with ggplot and non-metric MDS on baseball data <br><br>
lab 3: Maps created with Mapbox and plotly(plot_geo) <br><br>
lab 4: Heatmaps and  parallel coordinatet created with plotly and trellis plot with ggplot <br><br>
lab 5: Several motions graphs created with visNetwork and plotly <br><br>
lab 6: Word clouds created with wordcloud package and different interactive plots with filter, brushing etc created with plotly. <br>

#### If you have any questions regarding the work, please contact me

# A simple shiny app

```{r}

ui <- fluidPage( # Slider input 
  sliderInput(inputId="ws", label="Choose bandwidth size", value=0.1, min=0.1, max=1),
  
  # Checkbox input for all variables except X3
  checkboxGroupInput('Variable', 'Variables to choose for graphs',c('X1 - Length of Stay' = 1,
                                                     'X2 - Age' = 2,
                                                     'X4 - Routine Culturing Ratio' = 4,
                                                     'X5 - Routine Chest X-ray Ratio' = 5,
                                                     'X6 - Number of Beds' = 6,
                                                     'X9 - Average Daily Census' = 9,
                                                     'X10 - Number of Nurses' = 10,
                                                     'X11 - Available Facilities & Services' = 11)),
  
  plotOutput("densPlot") # output
)

server <- function(input, output,session) {

  output$densPlot <- renderPlot({
    
    p_list <- list()
    for (i in c(2,3,5,6,7,8,9,10,11,12)){
      var_name <- colnames(data[,2:12]) # Assigning the names
      indice <- data.frame(data[quantile_func(data[,i]),i]) # using the function to collect the outliers
      colnames(indice) <- var_name[i-1]
      indice$Y <- rep(0,nrow(indice))
      # creating the plot and assigningn it to p1,p2 etc...
      p_list[[i-1]] <- ggplot(data,aes_string(x= var_name[i-1] ))  + geom_density(bw=input$ws,fill=8,alpha=0.7) + 
        geom_point(data=indice,aes(y=Y),shape=5)+ ylab('Density') + xlab(text[i]) + theme_bw()
    }
    if (length(input$Variable) >=1){ # Controlling that at least 1 box is marked for a graph to be printed
    grid.arrange(grobs = p_list[as.numeric(input$Variable)])}
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
```
<br>
# A map of Sweden with Linköping pointed out as a red dot

## Starting with merging a csv and JSONfile

```R
# changed the encoding to get the åäö in the names
data_scb <- read.csv("000006SW_20230912-130931.csv",fileEncoding = "ISO-8859-1")

data_json <- fromJSON(file="gadm41_SWE_1.json")

# splitting the age variable 
data_scb <- spread(data_scb, key=age, value=X2016)

colnames(data_scb)[2:4] <- c('Young', 'Adult', 'Senior')
data_scb$region <- str_replace(data_scb$region,' county','')

data_scb$region <- gsub("[[:digit:] ]", "",data_scb$region )

data_scb$region[data_scb$region=='Örebro'] <- 'Orebro' # örebro is Orebro in json file..
# there is also numbers and spaces for each region so that will have to be taken care of for a possible merge

g=list(fitbounds="locations", visible=FALSE)

p<-plot_geo(data_scb)%>%add_trace(type="choropleth",geojson=data_json, locations=~region,
                                  z=~Young, featureidkey="properties.NAME_1",name="")%>%
  add_trace(lat = ~58.41109, lon = ~15.62565, type='markers',marker=list(color="red"), name='Linköping') %>% # changed the color
  layout(geo=g, showlegend = FALSE) # and name on the marker to red and Linköping!
p

```




<br><br>

<div align="center">
  <img src="https://media1.giphy.com/media/v1.Y2lkPTc5MGI3NjExYjMxMXlsM2JqYjU2anUyNHVvMGhpMjFuM2s3aHdibWtwZ3o3anRiMiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/bmQBu3aSF0DxadphkG/giphy.gif" width="600" height="600"/>
</div>

