#install.packages(c("shiny", "shinydashboard", "plotly", "leaflet", "curl"))


library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(leaflet)

cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/cover_species_garden_500.csv"))

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(style = "font-size: 18px", "Cover Botanical Garden")
  ),
  dashboardSidebar(
    tags$style(HTML("
      .filters-section {
        background-color: #008d4c;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .sidebar-menu {
        margin-top: 15px;
      }
    ")),
    sidebarMenu(
      div(class = "filters-section",
          menuItem("Filters", tabName = "filters", icon = icon("filter"), selected = TRUE),
          checkboxGroupInput(inputId = "Garden", label = "Select Garden", choices = c("Neuchâtel" = "ne", "Fribourg" = "fr", "Lausanne" = "la", "Geneva" = "ge")),
          selectInput(inputId = "family", label = "Family to Test", choices = sort(unique(cover_species_garden_full$family)), selected = ""),
          actionButton(inputId = "action", label = "Go!", icon = icon("play"), style = "color: #fff; background-color: #222c32;")
      ),
      menuItem("Phylogenetic", icon = icon("sitemap"),
               menuSubItem("Garden Tree", tabName = "garden_tree", icon = icon("tree")),
               menuSubItem("Family Tree", tabName = "family_tree", icon = icon("leaf"))
      ),
      menuItem("Biome", icon = icon("globe"),
               menuSubItem("Whitakker Garden plot", tabName = "whit_garden_plot", icon = icon("chart-area")),
               menuSubItem("Whitakker Family plot", tabName = "whit_family_plot", icon = icon("chart-bar"))
      ),
      menuItem("Quick Search", icon = icon("search"),
               menuSubItem("Species Selection", tabName = "species_selection", icon = icon("dna")),
               menuSubItem("Species World Distribution", tabName = "species_distribution", icon = icon("map"))
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      /* Logo and top bar */
      .skin-blue .main-header .logo {
        background-color: #008d4c; /* Green */
      }
      .skin-blue .main-header .navbar {
        background-color: #00a75a; /* Light Green */
      }
      /* Sidebar */
      .skin-blue .main-sidebar {
        background-color: #222c32; /* Dark Green */
      }
      /* Active tab */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #008d4c; /* Darker Green */
      }
      /* Other links */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #222c32; /* Lighter Green */
        color: #ffffff;
      }
      /* Hovered links */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #008d4c; /* Green */
      }
      /* Toggle button hovered */
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #008d4c; /* Green */
      }
      /* Change border color of box */
      .tab-pane.active .box {
        border-color: #00a75a; /* Light Green */
      }
      /* Change box header color */
      .tab-pane.active .box .box-header {
        background-color: #00a75a; /* Light Green */
        color: #ffffff; /* White */
      }
      /* Change download button color */
      .tab-pane.active .btn-primary {
        background-color: #00a75a; /* Light Green */
        border-color: #00a75a; /* Light Green */
      }
    '))),
    tabItems(
      tabItem(tabName = "filters",
        fluidRow(
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 30px; text-align: center;",
            "Welcome to the Botanical Garden Coverage Application."
          ),
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
            "To get started, choose one or more gardens."
          ),
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
            "Select a plant family of interest."
          ),
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
            "Launch the script with the 'Go' button."
          ),
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
           "Click on the various dropdown menus to view your results. Several pages display multiple graphs, so don't hesitate to scroll."
          ),
          tags$div(
            style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
            "If you find a bug or have any comments on the ergonomics or improvements for the app, and if you work in a botanical garden and 
            want to see your data included in the application, you can contact me at ",
            tags$a(
              href = "mailto:mazzarine.laboureau@unine.ch", 
              "mazzarine.laboureau@unine.ch.",
              style = "color: #3c8dbc; font-weight: bold;"
            ),
          ),
          tags$div(style = "color: #000000; font-size: 30px; font-weight: bold; margin-top: 50px; text-align: center;",
            "All the data and the script are available on my ",
            a(href = "https://github.com/MazzarineL/SBG_app", "GitHub page.")
          )
        )
      ),
      tabItem(tabName = "garden_tree",
        helpText(tags$strong("This section displays the Garden Tree plot.")),
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
        helpText(tags$strong("This section presents the Family Tree plot, highlighting the genus names in blue if they are included in the botanical garden, thereby enhancing the coverage optimization for the family. You can adjust the window size and the step for change the calcul parameters.")),
        fluidRow(
          box(title = "Family Tree", status = "primary", solidHeader = TRUE, width = 12,
            sliderInput(inputId = "genus_select", label = "Number of genus to select", min = 1, max = 30, value = 5),
            tags$div(style = "color: #000000; font-size: 14px; font-weight: bold; margin-top: 10px;",
              "It may happen that the number of genera selected cannot be perfectly matched to your request, in which case the model will choose the number of genera closest to your pre-selection, maximizing genus coverage. Furthermore, if you've chosen a high number of genus and the tree has no blue branches, this means that you've reached the maximum number of selectable genus and there are none to select as a priority."
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
        helpText(tags$strong("This section displays the Whitakker plot for the gardens selected.")),
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
        helpText(tags$strong("This section displays a table indicating the locations of each family, genus, or species across various botanical gardens.")),
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
        helpText(tags$strong("This section displays the species distribution in the world.")),
        fluidRow(
          box(title = "Species Distribution", status = "primary", solidHeader = TRUE, width = 12,
            tags$div(style = "color: #000000; font-size: 14px; font-weight: bold; margin-top: 10px;",
              "Some species do not have location data available on GBIF and iNaturalist; in such cases, they will not appear on the map."
            ),
            tags$br(), tags$br(),
            selectInput("GPS_family", "Select a family", choices = NULL),
            selectInput("GPS_genus", "Select a genus:", choices = NULL),
            selectInput("GPS_species", "Select a species:", choices = NULL, multiple = TRUE),
            actionButton("goButton", "Go"),
            leafletOutput("map", width = "100%", height = "500px"),
            tags$br(), tags$br(),
            plotOutput(outputId = "mapsSimple", height = "1000px"),
            div(class = "btn-group",
              downloadButton(outputId = "downloaddistrib", label = "Download Distribution map", class = "btn btn-primary")
            )
          )
        )
      )
    )
  )
)







