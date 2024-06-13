library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(leaflet)

cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/cover_species_garden_500.csv") )


ui <- dashboardPage(
  dashboardHeader(title = "Cover Botanical Garden"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      checkboxGroupInput(inputId = "Garden", label = "Select Garden", choices = c("Neuchâtel" = "ne", "Fribourg" = "fr", "Lausanne" = "la")),
      selectInput(inputId = "family", label = "Family to Test", choices = unique(cover_species_garden_full$family), selected = ""),
      actionButton(inputId = "action", label = "Go!", icon = icon("play"), style = "color: #fff; background-color: #27ae60;"),
      menuItem("Garden Tree", tabName = "garden_tree", icon = icon("tree"), selected = TRUE),
      menuItem("Family Tree", tabName = "family_tree", icon = icon("leaf")),
      menuItem("Whitakker Garden plot", tabName = "whit_garden_plot", icon = icon("chart-area")),
      menuItem("Whitakker Family plot", tabName = "whit_family_plot", icon = icon("chart-bar")),
      menuItem("Species Selection", tabName = "species_selection", icon = icon("dna")),
      menuItem("Species distribution", tabName = "species_distribution", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "garden_tree",
              helpText("This section displays the Garden Tree plot."),
        fluidRow(
          box(title = "Garden Tree", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput(outputId = "treePlot", height = "600px"),
            div(class = "btn-group",
              downloadButton(outputId = "downloadFullPlot", label = "Download Garden Tree Plot", class = "btn btn-primary")
            )
          )
        )
      ),
tabItem(tabName = "family_tree",
  helpText("This section presents the Family Tree plot, highlighting the genus names in blue if they are included in the botanical garden, thereby enhancing the coverage optimization for the family. You can adjust the window size and the step for change the calcul parameters."),
  fluidRow(
    box(title = "Family Tree", status = "primary", solidHeader = TRUE, width = 12,
      sliderInput(inputId = "genus_select", label = "Number of genus to select", min = 1, max = 30, value = 5),
      tags$div(style = "color: #ff0000; font-size: 14px; font-weight: bold; margin-top: 10px;",
        "It may happen that the number of genera selected cannot be perfectly matched to your request, in which case the model will choose the number of genera closest to your pre-selection, maximizing genus coverage.
        furthermore, if you've chosen a high number of genus and the tree has no blue branches, this means that you've reached the maximum number of selectable genus and there are none to select as a priority"
      ),
      textOutput("textgenus"),
      tableOutput("onlygenus"),
      plotOutput(outputId = "FamilyPlot", height = "800px"),  
      div(style = "margin-top: 20px;",
        tableOutput("mytable"),
        tags$style(HTML("
          #mytable table {
            width: 100%;
          }
          #mytable td {
            width: 33%;
          }
        "))
      ),
            div(class = "btn-group",
              downloadButton(outputId = "downloadFamilyPlot", label = "Download Family Tree Plot", class = "btn btn-primary"),
              tags$br(), tags$br(),
              downloadButton(outputId = "downloadTable", label = "Download Priority Table", class = "btn btn-primary")
            )
          )
        )
      ),
      tabItem(tabName = "whit_garden_plot",
              helpText("This section displays the Whitakker plot for the gardens selected."),
        fluidRow(
          box(title = "Whitakker Garden Plot", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput(outputId = "whitplot", height = "1000px"),
            div(class = "btn-group",
              downloadButton(outputId = "dlwhitplot", label = "Download Whitakker Plot", class = "btn btn-primary")
            )
          )
        )
      ),
      tabItem(tabName = "whit_family_plot",
              helpText("This section displays the Whitakker plot for the family selected."),
        fluidRow(
          box(title = "Whitakker Family Plot", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput(outputId = "whitplotFamily", height = "1000px"),
            div(class = "btn-group",
              downloadButton(outputId = "dlwhitplotFamily", label = "Download Whitakker Family Plot", class = "btn btn-primary")
            )
          )
        ),
        fluidRow(
          box(title = "Whitakker Family Kernel Density Plot", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput(outputId = "whitplotFamilyKernel", height = "1000px"),
            div(class = "btn-group",
              downloadButton(outputId = "dlwhitplotFamilyKernel", label = "Download Whitakker Family Plot with Kernel Density", class = "btn btn-primary")
            )
          )
        ),
        fluidRow(
          box(title = "Whitakker Family Plot Selection", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput(outputId = "whitplotSelect", height = "1000px")
          )
        )
      ),
      tabItem(tabName = "species_selection",
              helpText("This section displays a table indicating the locations of each family, genus, or species across various botanical gardens."),
        fluidRow(
          box(title = "Species Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("selected_family", "Family", choices = NULL),
            selectInput("selected_genus", "Genus", choices = c("", NULL)),
            selectInput("selected_species", "Species", choices = c("", NULL)),
            tags$br(), tags$br(),
            div(class = "btn-group",
              downloadButton(outputId = "downloadTablespecies", label = "Download table of species", class = "btn btn-primary")
            ),
            tableOutput("selectedData")
          )
        )
      ),
      tabItem(tabName = "species_distribution",
              helpText("This section displays the species distribution in the world."),
        fluidRow(
          box(title = "Species Distribution", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("GPS_genus", "Select a genus:", choices = NULL),
            selectInput("GPS_species", "Select a species:", choices = NULL, multiple = TRUE),
            actionButton("goButton", "Go"),
            textOutput("errortext"),
            leafletOutput("map", width = "100%", height = "500px"),
            tags$br(), tags$br(),
            plotOutput(outputId = "mapsSimple", height = "1000px"),
            div(class = "btn-group", downloadButton(outputId ="downloaddistrib", label = "Download Distribution map", class = "btn btn-primary"))
          )
        )
      )
    )
  )
)
