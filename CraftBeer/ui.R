library(shiny)
library(dplyr) # for distinct

#define list of states for selection
states <- rbind(state.name, "District of Columbia")

# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Craft Beer EDA"),
  navbarPage("Craft Beer Breweries",
    # first tab for exploration
    tabPanel("Exploration",
      sidebarPanel(
        # plot select
        selectInput( "plotSelect", label = "Select Plot", 
                     choices = list("Histogram" = 1, "Boxplot" = 2, "Scatterplot" = 3, "Mean Beer Strength Plot" = 4), 
                     selected = 1 ),
        # measure selection; only applicable for histogram and boxplot
        conditionalPanel(
          condition = "input.plotSelect == '1' || input.plotSelect == '2'",
          tags$hr(),
          # Input: Select separator ----
          radioButtons( "measure", "Measure",
                        choices = c( ABV = "ABV",
                                     IBU = "IBU" ),
                        selected = "ABV", inline=TRUE
                      )
        ),
        # regression line selection; only applicable for scatterplot
        conditionalPanel(
          condition = "input.plotSelect == '3'",
          tags$hr(),
          # Input: Checkbox if file has header ----
          checkboxInput("regLine", "Linear Regression Line", TRUE)
        ),
        # slider for bins; only applicable for histogram
        conditionalPanel(
          condition = "input.plotSelect == '1'",
          tags$hr(),
          sliderInput( "numBins", label = "Number of Bins", 
                       min=10, max=80, value=30 )
        ),
        # state selection; applicable for all plots
        tags$hr(),
        selectInput( "stateSelect", label = "Select State(s)", 
                     choices = c("All", sort(unique(states))), 
                     selected = "All", multiple = TRUE, selectize = FALSE, size = 10 )
      ),
      mainPanel(
        plotOutput("plot"),
        uiOutput("url1")
      )
    ),
    # second tab for raw data
    tabPanel("Raw Data",
      # create a tab set panel for two different tabs;
      # one for Beers and one for Breweries
      tabsetPanel(
        tabPanel("Beers Data",
          # Sidebar panel for inputs ----
          sidebarPanel(
            # Input: Select number of rows to display ----
            radioButtons( "disp1", "Display",
                          choices = c( Head = "head",
                                       All = "all" ),
                          selected = "head", inline=TRUE
                        )
          ),
          # Main panel for displaying outputs ----
          mainPanel(
            tableOutput("beerContents"),
            uiOutput("url2")
          )
        ),
        tabPanel("Breweries Data",
          # Sidebar panel for inputs ----
          sidebarPanel(
            # Input: Select number of rows to display ----
            radioButtons( "disp2", "Display",
                          choices = c( Head = "head",
                                       All = "all" ),
                          selected = "head", inline=TRUE
                        )
          ),
          # Main panel for displaying outputs ----
          mainPanel(
            tableOutput("breweryContents"),
            uiOutput("url3")
          )
        )
      )
    )
  )
)