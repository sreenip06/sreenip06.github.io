library(shiny)
library(ggplot2)
library(usmap)     # for US map

beers <- read.csv ("./data/Beers.csv", header = TRUE, strip.white=TRUE, na.strings=c("", " ", "NA"))
# Remove Duplicates
beers <- distinct(beers, Name, ABV, IBU, Brewery_id, Style, Ounces, .keep_all=TRUE)
# Read breweries data
breweries <- read.csv ("./data/Breweries.csv", header= TRUE, strip.white = TRUE, na.strings=c("", " ", "NA"))
# Get state names, format them and add DC
stateDF <- data.frame(cbind(state.abb, state.name))
stateDF$state.abb <- as.character(stateDF$state.abb)
stateDF$state.name <- as.character(stateDF$state.name)
stateDF <- rbind(stateDF, c("DC", "District of Columbia"))
# Add state names to the DF
breweries <- merge(x=breweries, y=stateDF, by.x="State", by.y="state.abb")
# Merge beer and brewery data sets
beerBreweryDF <- merge(x=beers,y=breweries,by.x="Brewery_id",by.y="Brew_ID")
# Tidy columns Name
names(beerBreweryDF) <- c("Brewery_Id","Beer_Name","Beer_Id","ABV","IBU","Beer_Style","Beer_Oz","state","Brewery_Name","Brewery_City","State_Name")
# Cleanup a bit; keep beers and breweries as we will
# need them to display on raw data screen
rm(stateDF)

# Define server logic to read selected file ----
server <- function(input, output) {

  # output for beers raw data  
  output$beerContents <- renderTable({
    if(input$disp1 == "head") {
     return(head(beers))
    }
    else {
     return(beers)
    }
  })
  
  # output for breweries raw data  
  output$breweryContents <- renderTable({
    if(input$disp2 == "head") {
      return(head(breweries))
    }
    else {
      return(breweries)
    }
  })

  # for displaying link to the complete EDA; looks like we need
  # different output containers and cannot reuse them across panels
  url <- a("Craft Beer EDA", href="https://sreenip06.github.io/US-Carft-Beers-and-Breweries---EDA.html")
  output$url1 <- renderUI({tagList("Check out our complete EDA at:", url)})
  output$url2 <- renderUI({tagList("Check out our complete EDA at:", url)})
  output$url3 <- renderUI({tagList("Check out our complete EDA at:", url)})
  
  # plot output
  output$plot <- renderPlot({
    # filter data set in case states are selected; if "All"
    # is also selected with any other states, logic will
    # default to "All"
    dataSubset <- beerBreweryDF
    if (!(length(which(input$stateSelect=="All"))) > 0) {
      dataSubset <- filter(beerBreweryDF, State_Name %in% (input$stateSelect))
    }
    # assign data set to ggplot; if custom plot is selected,
    # build plot using usmap
    if (input$plotSelect == "4") {
      # aggregate data to get mean ABV
      babv <- aggregate(ABV ~ state+State_Name, data=dataSubset[!is.na(dataSubset$ABV),],  FUN=mean, keep.names=T)
      p <- plot_usmap(data=babv, values = "ABV", color = "white", labels=FALSE)
      p <- p +
        scale_fill_continuous(low = "darkgoldenrod2", high = "darkorange4", guide="colorbar") +
        scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c())
    } else
      p <- ggplot(data=dataSubset)
    # add default theme
    p <- p + theme_bw()
    # now add plot type based on selection
    padd <- switch(input$plotSelect,
                   "1" = switch(input$measure,
                                "ABV" = geom_histogram(aes(x=ABV), fill="goldenrod1", color="black", na.rm=TRUE, bins=input$numBins),
                                "IBU" = geom_histogram(aes(x=IBU), fill="goldenrod1", color="black", na.rm=TRUE, bins=input$numBins)),
                   "2" = switch(input$measure,
                                "ABV" = geom_boxplot(aes(y=ABV), fill="goldenrod1", na.rm=TRUE),
                                "IBU" = geom_boxplot(aes(y=IBU), fill="goldenrod1", na.rm=TRUE)),
                   "3" = geom_point(aes(x=ABV, y=IBU), color="goldenrod1", na.rm=TRUE)
                   )
    # set title and labels
    plabs <- switch(input$plotSelect,
                    "1" = switch(input$measure,
                                 "ABV" = labs(title="Histogram of ABV", x="ABV", y="Count"),
                                 "IBU" = labs(title="Histogram of IBU", x="IBU", y="Count")),
                    "2" = switch(input$measure,
                                 "ABV" = labs(title="Boxplot of ABV", x="", y="ABV"),
                                 "IBU" = labs(title="Boxplot of IBU", x="", y="IBU")),
                    "3" = labs(title="Scatterplot of ABV vs. IBU", x="ABV", y="IBU"),
                    "4" = labs(fill = "Mean ABV", title = "Mean ABV by State", x="", y="")
    )
    p <- p + padd + plabs
    # remove x axis text and ticks for boxplot
    if (input$plotSelect == "2") {
      p <- p +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    }
    # add regression line to scatterplot if selected
    if (input$regLine && input$plotSelect == "3") {
      p <- p + geom_smooth(aes(x=ABV, y=IBU), method=lm, na.rm=TRUE)
    }
    # set axes limits for scatterplot so that toggling of 
    # regression line is smooth
    if (input$plotSelect == "3"){
      p <- p + xlim(0, 0.15) + ylim(0, 150)
    }
    # add some common formatting
    p <- p + theme(plot.title=element_text(hjust=0.5, size=18))
    # finally, render the plot
    return(p)
  })
}
