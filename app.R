# Load packages using library function as shinyapps.io does not allow installation
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(tmap)
library(spdep)
library(rgeos)

# -----Load data files
load("data/map_sp.rda")


# -----All Global Parameters here

# List of Borough, Ward
varBor <- str_sort(unique(map_sp@data$bor_nm))
varWard <- unique(map_sp@data$ward_nm)

# Choices for drop-downs
# Measures
varMeasure <- c(
  "Weight"="weight",
  "Volume"="volume",
  "Fat"="fat",
  "Saturate"="saturate",
  "Salt"="salt",
  "Sugar"="sugar",
  "Protein"="protein",
  "Carb"="carb",
  "Fibre"="fibre",
  "Alcohol"="alcohol",
  "Energy_Fat"="energy_fat",
  "Energy_Saturate"="energy_saturate",
  "Energy_Sugar"="energy_sugar",
  "Energy_Protein"="energy_protein",
  "Energy_Carb"="energy_carb",
  "Energy_Fibre"="energy_fibre",
  "Energy_Alcohol"="energy_alcohol",
  "Energy_Total"="energy_tot",
  "Energy_Density"="energy_density",
  "H_Nutrients_Weight"="h_nutrients_weight",
  "H_Nutrients_Weight_Norm"="h_nutrients_weight_norm",
  "H_Nutrients_Calories"="h_nutrients_calories",
  "H_Nutrients_Calories_Norm"="h_nutrients_calories_norm",
  "H_Items"="h_items",
  "H_Items_Norm"="h_items_norm",
  "H_Items_Weight"="h_items_weight",
  "H_Items_Weight_Norm"="h_items_weight_norm",
  "Representativeness"="representativeness_norm",
  "Population"="population",
  "Male"="male",
  "Female"="female",
  "Age_0-17"="age_0_17",
  "Age_18-64"="age_18_64",
  "Age_65+"="age_65",
  "Avg_Age"="avg_age",
  "Area_sqkm"="area_sq_km",
  "People_per_sqkm"="people_per_sq_km",
  "Overweight_4-5"="prevalence_overweight_reception",
  "Overweight_6-10"="prevalence_overweight_y6",
  "Obese_4-5"="prevalence_obese_reception",
  "Obese_6-10"="prevalence_obese_y6",
  "Diabetic"="estimated_diabetes_prevalence"
)


# -----Define UI for random distribution app ----
ui <- fluidPage(theme=shinytheme("cerulean"),
    
# -----Navigation Bar
    navbarPage("Tesco", fluid=TRUE, windowTitle="Tesco Grocery 1.0 Visual Analytics", selected="lisa",
               

# -----Data Panel
               tabPanel("Data", value="data", fluid=TRUE, icon=icon("database"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel("DM sidebarPanel", width=3, fluid=TRUE
                             ),
                             mainPanel("DM mainPanel", width=9
                             )
                         )
                ),


# -----EDA Panel
                tabPanel("EDA", value="eda", fluid=TRUE, icon=icon("search"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel("EDA sidebarPanel", width=3, fluid=TRUE
                             ),
                             mainPanel("EDA mainPanel", width=9
                             )
                         )
                ),


# -----Lisa Panel
                tabPanel("LISA", value="lisa", fluid=TRUE, icon=icon("globe-americas"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel(width=3, fluid=TRUE,
                                          selectInput(inputId="inBor",
                                                      label="Borough",
                                                      choices=varBor,
                                                      selected="Newham",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="inMeasure",
                                                      label="Measure",
                                                      choices=varMeasure,
                                                      selected="energy_carb",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                             ),
                             mainPanel(width=9,
                                       fluidRow(
                                         column(6,
                                                leafletOutput("lisa"),
                                                selectInput(inputId="inLisaMethod",
                                                            label="Analysis Method",
                                                            choices=c("Contiguity Queen"="q",
                                                                      "Contiguity Rook"="r",
                                                                      "K Nearest Neighbours"="knn",
                                                                      "IDW Queen"="idw-q",
                                                                      "IDW Rook"="idw-r"
                                                            ),
                                                            selected="q",
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
                                                selectInput(inputId="inLisaSignificance",
                                                            label="Confidence Level",
                                                            choices=c("90%"=0.1,
                                                                      "95%"=0.05,
                                                                      "99%"=0.01,
                                                                      "99.9%"=0.001
                                                            ),
                                                            selected=0.1,
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
                                                conditionalPanel(condition="input.inLisaMethod=='knn'",
                                                                 sliderInput(inputId="k",
                                                                             label="Select K",
                                                                             min=1,
                                                                             max=10,
                                                                             value=3,
                                                                             width="100%"
                                                                 )
                                                )
                                         ),
                                         column(6,
                                                leafletOutput("reference"),
                                                selectInput(inputId="inReference",
                                                            label="Reference Value",
                                                            choices=c("Raw Values"="r",
                                                                      "Local Moran's I"="i",
                                                                      "P-Value"="p"
                                                            ),
                                                            selected="r",
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
                                                selectInput(inputId="inBinning",
                                                            label="Binning Method",
                                                            choices=c("Std Deviation"="sd",
                                                                      "Equal"="equal",
                                                                      "Pretty"="pretty",
                                                                      "Quantile"="quantile",
                                                                      "K-means"="kmeans",
                                                                      "Hierarchical Cluster"="hclust",
                                                                      "Binary Cluster"="bclust",
                                                                      "Fisher"="fisher",
                                                                      "Jenkins"="jenks",
                                                                      "Log10"="log10_pretty"
                                                            ),
                                                            selected="quantile",
                                                            multiple=FALSE,
                                                            width="100%"
                                                )
                                         )
                                       )
                             )
                         )
                ),


# -----Clustering Panel
                tabPanel("Clustering", value="clustering", fluid=TRUE, icon=icon("globe-asia"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel("Cluster sidebarPanel", width=3, fluid=TRUE
                             ),
                             mainPanel("Cluster mainPanel", width=9
                             )
                         )
                ),


# -----GWR Panel
                tabPanel("GWR", value="gwr", fluid=TRUE, icon=icon("laptop-code"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel("GWR sidebarPanel", width=3, fluid=TRUE
                             ),
                             mainPanel("GWR mainPanel", width=9
                             )
                         )
                )
    )
)




# -----Server Function
server <- function(input, output) {


# -----All Global functions, variables here
  rv <- reactiveValues()


# -----Data functions


# -----EDA functions

  
# -----Lisa functions
  output$lisa <- renderLeaflet({
    
    subset <- map_sp[map_sp$bor_nm==input$inBor,] 
    indicator <- pull(subset@data, input$inMeasure)
    
    if (input$inLisaMethod=="q") {
      wm <- poly2nb(subset, queen=TRUE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="r") {
      wm <- poly2nb(subset, queen=FALSE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="knn") {
      wm <- knn2nb(knearneigh(coordinates(subset), k=input$k), row.names=row.names(subset$ward_nm))
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="idw-q") {
      wm <- poly2nb(subset, queen=TRUE)
      dist <- nbdists(wm, coordinates(subset), longlat=TRUE)
      idw <- lapply(dist, function(x) 1/(x/1000))
      rswm <- nb2listw(wm, glist=idw, style="B", zero.policy=TRUE)
    }
    else {
      wm <- poly2nb(subset, queen=FALSE)
      dist <- nbdists(wm, coordinates(subset), longlat=TRUE)
      idw <- lapply(dist, function(x) 1/(x/1000))
      rswm <- nb2listw(wm, glist=idw, style="B", zero.policy=TRUE)
    }
    
    rv$lmoran <- localmoran(indicator, rswm)
    
    quadrant <- vector(mode = "numeric", length = nrow(rv$lmoran))
    DV <- indicator - mean(indicator)
    C_mI <- rv$lmoran[,1] - mean(rv$lmoran[,1])
    
    quadrant[DV >0 & C_mI>0] <- 4 
    quadrant[DV <0 & C_mI<0] <- 1 
    quadrant[DV <0 & C_mI>0] <- 2 
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[rv$lmoran[,5] >input$inLisaSignificance] <- 0
    
    subset$quadrant <- quadrant
    
    legend <- c("insignificant","low-low", "low-high", "high-low", "high-high")
    colors <- c("white","blue","cyan","pink","red")
    
    lisaPlot <- tm_shape(subset) +
      tm_fill("quadrant",
              title="LISA Cluster",
              style="cat",
              palette="Blues",
              labels=legend,
              id="ward_nm",
              alpha=0.7
      ) +
      tm_borders(alpha=0.7
      ) +
      tm_view(view.legend.position=c("right","bottom"),
              control.position=c("left","bottom"),
              colorNA="Black"
      ) +
      tmap_options(basemaps=c("Esri.WorldGrayCanvas","Esri.WorldTopoMap","Esri.WorldImagery","OpenStreetMap"),
                   basemaps.alpha=c(0.9,0.7,0.7,0.7)
      )
    tmap_leaflet(lisaPlot, in.shiny=TRUE)
    
  })

  output$reference <- renderLeaflet({

    subset <- map_sp[map_sp$bor_nm==input$inBor,]
    refDf <- cbind(subset, rv$lmoran)

    if (input$inReference=="r"){
      tmFill <- input$inMeasure
      tmTitle <- "Raw Values"
    }
    else if (input$inReference=="i"){
      tmFill <- "Ii"
      tmTitle <- "I-Values"
    }
    else {
      tmFill <- "Pr.z...0."
      tmTitle <- "P-Values"
    }
    
      tmRaw <- tm_shape(refDf) +
        tm_fill(tmFill,
                title=tmTitle,
                style=input$inBinning,
                palette="Blues",
                id="ward_nm",
                alpha=0.7
        ) +
        tm_borders(alpha=0.7
        ) +
        tm_view(view.legend.position=c("right","bottom"),
                control.position=c("left","bottom"),
                colorNA="Black"
        ) +
        tmap_options(basemaps=c("Esri.WorldGrayCanvas","Esri.WorldTopoMap","Esri.WorldImagery","OpenStreetMap"),
                     basemaps.alpha=c(0.9,0.7,0.7,0.7)
        )
      tmap_leaflet(tmRaw, in.shiny=TRUE)

  })


observe({
  coords1 <- input$lisa_bounds
  if (!is.null(coords1)) {
    leafletProxy("reference") %>% 
      fitBounds(coords1$west,
                coords1$south,
                coords1$east,
                coords1$north)
  }
}, priority=1)

observe({
  coords2 <- input$reference_bounds
  if (!is.null(coords2)) {
    leafletProxy("lisa") %>% 
      fitBounds(coords2$west,
                coords2$south,
                coords2$east,
                coords2$north)
  }
}, priority=3)


# -----GWR functions


}


# -----Create Shiny app ----
shinyApp(ui, server)