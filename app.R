# Load packages using library function as shinyapps.io does not allow installation
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(tmap)
library(spdep)
library(rgeos)
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(GWmodel)
#library(reactlog)

#jufri
library(ClustGeo)
library(dendextend)
library(GGally)
library(ggdendro)
library(corrplot)

# tell shiny to log all reactivity
#options(shiny.reactlog = TRUE)


# -----Load data files
load("data/maprgn_sp.rda")
load("data/maplad_sp.rda")
load("data/mapward_sp.rda")
load("data/mapmsoa_sp.rda")
load("data/maplsoa_sp.rda")
load("data/ladbbox.rda")
load("data/maprgn_sf.rda")
load("data/maplad_sf.rda")
load("data/mapward_sf.rda")
load("data/mapmsoa_sf.rda")
load("data/maplsoa_sf.rda")


# -----All Global Parameters here

# Level of Detail
varLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA",
  "LSOA"="LSOA"
)
  
# List of LAD
#varLad <- str_sort(unique(mapward_sp@data$lad_nm))
varLad <- c(
  "All",
  "City of London",
  "Barking and Dagenham",
  "Barnet",
  "Bexley",
  "Brent",
  "Bromley",
  "Camden",
  "Croydon",
  "Ealing",
  "Enfield",
  "Greenwich",
  "Hackney",
  "Hammersmith and Fulham",
  "Haringey",
  "Harrow",
  "Havering",
  "Hillingdon",
  "Hounslow",
  "Islington",
  "Kensington and Chelsea",
  "Kingston upon Thames",
  "Lambeth",
  "Lewisham",
  "Merton",
  "Newham",
  "Redbridge",
  "Richmond upon Thames",
  "Southwark",
  "Sutton",
  "Tower Hamlets",
  "Waltham Forest",
  "Wandsworth",
  "Westminster"
)

# Choices for drop-downs
# Measures1
varMeasure1 <- c(
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
  "People_per_sqkm"="people_per_sq_km"
)

# Measures2
varMeasure2 <- c(
  "Overweight_4-5"="prevalence_overweight_reception",
  "Overweight_6-10"="prevalence_overweight_y6",
  "Obese_4-5"="prevalence_obese_reception",
  "Obese_6-10"="prevalence_obese_y6"
)

# GWR Level of Detail
varGwrLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA"
)

# GWR List of LAD
varGwrLad <- c(
  "All",
  "City of London",
  "Barking and Dagenham",
  "Barnet",
  "Bexley",
  "Brent",
  "Bromley",
  "Camden",
  "Croydon",
  "Ealing",
  "Enfield",
  "Greenwich",
  "Hackney",
  "Hammersmith and Fulham",
  "Haringey",
  "Harrow",
  "Havering",
  "Hillingdon",
  "Hounslow",
  "Islington",
  "Kensington and Chelsea",
  "Kingston upon Thames",
  "Lambeth",
  "Lewisham",
  "Merton",
  "Newham",
  "Redbridge",
  "Richmond upon Thames",
  "Southwark",
  "Sutton",
  "Tower Hamlets",
  "Waltham Forest",
  "Wandsworth",
  "Westminster"
)

# GWR Approach
varGwrApproach <- c(
  "CV"="CV",
  "AIC"="AIC"
)

# GWR Kernel
varGwrKernel <- c(
  "Gaussian"="gaussian",
  "Exponential"="exponential",
  "Bisquare"="bisquare",
  "Tricube"="tricube",
  "Boxcar"="boxcar"
)

# GWR Bandwidth
varGwrBandwidth <- c(
  "Fixed"=FALSE,
  "Adaptive"=TRUE
)

# GWR AutoBandwidth
varGwrAutoBandwidth <- c(
  "Manual"=FALSE,
  "Auto"=TRUE
)

# GWR Distance
varGwrDistance <- c(
  "Euclidean"=2,
  "Manhattan"=1
)


# -----Define UI for app
ui <- fluidPage(theme=shinytheme("superhero"),
    
# -----Navigation Bar
    navbarPage("Tesco", fluid=TRUE, windowTitle="Tesco Grocery 1.0 Visual Analytics", selected="gwr",
               

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


# -----ESDA Panel
                tabPanel("ESDA", value="esda", fluid=TRUE, icon=icon("globe-americas"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel(width=3, fluid=TRUE,
                                          column(5,
                                          selectInput(inputId="inLod",
                                                      label="ESDA Level",
                                                      choices=varLod,
                                                      selected="LAD",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )),
                                          column(7,
                                          selectInput(inputId="inLad",
                                                      label="LAD Zoom",
                                                      choices=varLad,
                                                      selected="All",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )),
                                          selectInput(inputId="inMeasure",
                                                      label="Select Variable",
                                                      choices=varMeasure1,
                                                      selected="energy_carb",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )
                             ),
                             mainPanel(width=9,
                                       fluidRow(
                                         column(6,
                                                leafletOutput("lisa"),
                                                column(6,
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
                                                )),
                                                column(6,
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
                                                )),
                                                conditionalPanel(condition="input.inLisaMethod=='knn'",
                                                                 sliderInput(inputId="k",
                                                                             label="Select K",
                                                                             min=2,
                                                                             max=30,
                                                                             value=5,
                                                                             width="100%"
                                                                 )
                                                )
                                         ),
                                         column(6,
                                                leafletOutput("reference"),
                                                column(6,
                                                selectInput(inputId="inReference",
                                                            label="Reference Value",
                                                            choices=c("Raw Values"="r",
                                                                      "Local Moran's I"="i",
                                                                      "P-Value"="p"
                                                            ),
                                                            selected="p",
                                                            multiple=FALSE,
                                                            width="100%"
                                                )),
                                                column(6,
                                                conditionalPanel(condition="input.inReference!='p'",
                                                                 selectInput(inputId="inBinning",
                                                                             label="Binning Method",
                                                                             choices=c("Std Deviation"="sd",
                                                                                       "Equal"="equal",
                                                                                       "Pretty"="pretty",
                                                                                       "Quantile"="quantile",
                                                                                       "K-means Cluster"="kmeans",
                                                                                       "Hierarchical Cluster"="hclust",
                                                                                       "Bagged Cluster"="bclust",
                                                                                       "Fisher"="fisher",
                                                                                       "Jenks"="jenks",
                                                                                       "Log10 Pretty"="log10_pretty"
                                                                             ),
                                                                             selected="quantile",
                                                                             multiple=FALSE,
                                                                             width="100%"
                                                                 ))),
                                                conditionalPanel(condition="input.inReference!='p'",
                                                                 sliderInput(inputId="inN",
                                                                             label="Select number of classes",
                                                                             min=2,
                                                                             max=10,
                                                                             value=5,
                                                                             width="100%"
                                                                 ))
                                                )
                                         )
                                       )
                             )
                ),


# -----Clustering Panel
tabPanel("Clustering", value="clustering", fluid=TRUE, icon=icon("globe-asia"),
         sidebarLayout(position="left", fluid=TRUE,
                       sidebarPanel("Cluster sidebarPanel", width=3, fluid=TRUE,
                                    selectInput(inputId="inLodc",
                                                label="Level of Detail",
                                                choices=varLod,
                                                selected="LAD",
                                                multiple=FALSE,
                                                width="100%"
                                    ),
                                    conditionalPanel(condition="input.inLodc!='LAD'",
                                                     selectInput(inputId="inLadc",
                                                                 label="Local Authority District",
                                                                 choices=varLad,
                                                                 selected="All",
                                                                 multiple=FALSE,
                                                                 width="100%"
                                                     )
                                    ),
                                    selectInput(inputId="inMeasureCluster",
                                                label="Measure",
                                                choices=varMeasure1,
                                                selected=c("Fat"="fat",
                                                           "Saturate"="saturate",
                                                           "Salt"="salt",
                                                           "Sugar"="sugar",
                                                           "Protein"="protein",
                                                           "Carb"="carb",
                                                           "Fibre"="fibre",
                                                           "Alcohol"="alcohol"),
                                                multiple=TRUE,
                                                width="100%"
                                    ),
                                    checkboxInput(inputId="inShowCorrPlot", label="Show Correlation Plot", value=FALSE),
                                    conditionalPanel(condition="input.inShowCorrPlot",
                                                     plotOutput("corrplot")
                                    ),
                                    sliderInput(inputId="inClusterSize", label="Cluster Size", min=1, max=10,
                                                value=5, step=1, round=TRUE
                                    ),
                                    plotOutput("findkplot", height = 300),
                       ),
                       mainPanel(width=9,
                                 fluidRow(
                                   column(6,
                                          radioButtons(inputId = "clustMethod",
                                                       label = "Select Clustering Method",
                                                       choiceNames = c("Hierarchical Clustering","Geo Spatial Clustering","Skater Clustering"),
                                                       choiceValues = c("HC","GS","SK"),
                                                       selected = "HC"),
                                          leafletOutput("cluster_left"),
                                          conditionalPanel(condition="input.clustMethod=='GS'",
                                                           sliderInput(inputId = "inAlpha", label = "Mixing Factor", min = 0, 
                                                                       max = 1, value = 0.4,width="100%", step=0.1),
                                                           plotOutput("alphaplot", height = 300)
                                          ),
                                          conditionalPanel(condition="input.clustMethod=='HC'",
                                                           selectInput(inputId="inAggloMethod",
                                                                       label="Agglomeration Method",
                                                                       choices=c("ward.D"="ward.D",
                                                                                 "ward.D2"="ward.D2",
                                                                                 "single"="single",
                                                                                 "complete"="complete",
                                                                                 "average"="average",
                                                                                 "mcquitty"="mcquitty",
                                                                                 "median"="median",
                                                                                 "centroid"="centroid"
                                                                       ),
                                                                       selected="ward.D"
                                                           ),
                                                           tableOutput("aggloplot")
                                          )            
                                   ),
                                   column(6,
                                          plotOutput("cluster_right"),
                                          conditionalPanel(condition="input.inLodc!='LSOA'",
                                                           plotOutput("dendoplot")
                                          ),
                                          conditionalPanel(condition="input.inLodc=='LSOA'",
                                                           "Dendogram not recommended for LSOA. More than 4000 records."
                                          ),
                                          radioButtons(inputId = "inDendoGraph",
                                                       label = "Graph type",
                                                       inline = TRUE, 
                                                       choiceNames = c("Tree", "Fan"),
                                                       choiceValues = c("tree","fan"),
                                                       selected = "tree"),
                                   )
                                 )
                       )
         )
),


# -----GWR Panel
                tabPanel("GWR", value="gwr", fluid=TRUE, icon=icon("laptop-code"),
                         sidebarLayout(position="right", fluid=TRUE,
                             sidebarPanel(width=3, fluid=TRUE,
                                          column(5,
                                          selectInput(inputId="GwrLod",
                                                      label="GWR Level",
                                                      choices=varGwrLod,
                                                      selected="LAD",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )),
                                          column(7,
                                          selectInput(inputId="GwrLad",
                                                      label="LAD Zoom",
                                                      choices=varGwrLad,
                                                      selected="All",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )),
                                          selectInput(inputId="GwrY",
                                                      label="Dependent Variable",
                                                      choices=varMeasure2,
                                                      selected="prevalence_obese_y6",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrX",
                                                      label="Explanatory Variables",
                                                      choices=varMeasure1,
                                                      selected="energy_carb",
                                                      multiple=TRUE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrKernel",
                                                      label="Kernel Method",
                                                      choices=varGwrKernel,
                                                      selected="gaussian",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          column(5,
                                                 radioButtons(inputId="GwrApproach",
                                                              label="Approach",
                                                              choices=varGwrApproach,
                                                              selected="CV",
                                                              inline=FALSE,
                                                              width="100%"
                                                 ),
                                          checkboxInput(inputId="GwrBandwidth",
                                                        label="Adaptive",
                                                        value=TRUE,
                                                        width="100%"
                                          )
                                          ),
                                          column(7,
                                                 radioButtons(inputId="GwrDistance",
                                                              label="Distance",
                                                              choices=varGwrDistance,
                                                              selected=2,
                                                              inline=FALSE,
                                                              width="100%"
                                                 ),
                                          checkboxInput(inputId="GwrAutoBandwidth",
                                                        label="Auto Bandwidth",
                                                        value=TRUE,
                                                        width="100%"
                                          )),
                                          fluidRow(
                                          conditionalPanel(condition="input.GwrAutoBandwidth==0",
                                                           sliderInput(inputId="ManualBandwidth",
                                                                       label="Specify Bandwidth",
                                                                       min=5,
                                                                       max=1000,
                                                                       value=20,
                                                                       width="100%"
                                                           )
                                          )
                             )),
                             mainPanel(width=9, fluid=TRUE,
                                       fluidRow(
                                         column(6,
                                                leafletOutput("gwr1"),
                                                column(6,
                                                selectInput(inputId="Gwr1Reference",
                                                            label="Reference Value",
                                                            choices=c("Local R2"="Local_R2"
                                                            ),
                                                            selected=NULL,
                                                            multiple=FALSE,
                                                            width="100%"
                                                )),
                                                column(6,
                                                selectInput(inputId="Gwr1Binning",
                                                            label="Binning Method",
                                                            choices=c("Std Deviation"="sd",
                                                                      "Equal"="equal",
                                                                      "Pretty"="pretty",
                                                                      "Quantile"="quantile",
                                                                      "K-means Cluster"="kmeans",
                                                                      "Hierarchical Cluster"="hclust",
                                                                      "Bagged Cluster"="bclust",
                                                                      "Fisher"="fisher",
                                                                      "Jenks"="jenks",
                                                                      "Log10 Pretty"="log10_pretty"
                                                            ),
                                                            selected="quantile",
                                                            multiple=FALSE,
                                                            width="100%"
                                                )),
                                                sliderInput(inputId="Gwr1N",
                                                            label="Select number of classes",
                                                            min=2,
                                                            max=30,
                                                            value=5,
                                                            width="100%"
                                                )
                                         ),
                                         column(6,
                                                leafletOutput("gwr2")
                                                # selectInput(inputId="Gwr2Reference",
                                                #             label="Reference Value",
                                                #             choices=c("P-Value"="p",
                                                #                       "Local R2"="r",
                                                #                       "Residuals"="i"
                                                #             ),
                                                #             selected="p",
                                                #             multiple=FALSE,
                                                #             width="100%"
                                                # ),
                                                # selectInput(inputId="Gwr2Binning",
                                                #             label="Binning Method",
                                                #             choices=c("Std Deviation"="sd",
                                                #                       "Equal"="equal",
                                                #                       "Pretty"="pretty",
                                                #                       "Quantile"="quantile",
                                                #                       "K-means Cluster"="kmeans",
                                                #                       "Hierarchical Cluster"="hclust",
                                                #                       "Bagged Cluster"="bclust",
                                                #                       "Fisher"="fisher",
                                                #                       "Jenks"="jenks",
                                                #                       "Log10 Pretty"="log10_pretty"
                                                #             ),
                                                #             selected="quantile",
                                                #             multiple=FALSE,
                                                #             width="100%"
                                                # ),
                                                # sliderInput(inputId="Gwr2N",
                                                #             label="Select number of classes",
                                                #             min=2,
                                                #             max=10,
                                                #             value=5,
                                                #             width="100%"
                                                # )
                                         )
                                       ),
                                       tableOutput(outputId="GwrTable"),
                                       # DT::dataTableOutput(outputId="GwrTable"),
                                       verbatimTextOutput(outputId="GwrSummary")
                             )
                         )
                )
    )
)





# -----Server Function
server <- function(input, output, session) {


# -----All Global functions, variables here
  rv <- reactiveValues()


# -----Data functions


# -----EDA functions

  
# -----ESDA functions
  legend <- c("insignificant","low-low", "low-high", "high-low", "high-high")
  #colors <- c("white","blue","sky-blue","darkpink","red")
  colorsRd <- c("#ffffff","#fcae91","#fb6a4a","#de2d26","#a50f15")
  colorsBu <- c("#ffffff","#bdd7e7","#6baed6","#3182bd","#08519c")
  colorsNBu <- c("#08519c","#3182bd","#6baed6","#bdd7e7","#ffffff")
  colorsLi <- c("#ffffff","#08519c","#6baed6","#fb6a4a","#a50f15")
  
  output$lisa <- renderLeaflet({
    
    if (input$inLod=="LAD") {
    subset <- maplad_sp[,"area_nm"]
    indicator <- pull(maplad_sp@data, input$inMeasure)
    # subsetView <- maprgn_sp
    }
    else if (input$inLod=="Ward") {
      subset <- mapward_sp[,"area_nm"]
      indicator <- pull(mapward_sp@data, input$inMeasure)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    else if (input$inLod=="MSOA") {
      subset <- mapmsoa_sp[,"area_nm"]
      indicator <- pull(mapmsoa_sp@data, input$inMeasure)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    else {
      subset <- maplsoa_sp[,"area_nm"]
      indicator <- pull(maplsoa_sp@data, input$inMeasure)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    
    if (input$inLad=="All"){
      rv$subsetView <- maprgn_sp[,"area_nm"]
    }
    else {
      rv$subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
    }

    if (input$inLisaMethod=="q") {
      wm <- poly2nb(subset, queen=TRUE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="r") {
      wm <- poly2nb(subset, queen=FALSE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="knn") {
      wm <- knn2nb(knearneigh(coordinates(subset), k=input$k), row.names=row.names(subset$area_nm))
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="idw-q") {
      wm <- poly2nb(subset, queen=TRUE)
      dist <- nbdists(wm, coordinates(subset), longlat=FALSE)
      idw <- lapply(dist, function(x) 1/(x/1000))
      rswm <- nb2listw(wm, glist=idw, style="B", zero.policy=TRUE)
    }
    else {
      wm <- poly2nb(subset, queen=FALSE)
      dist <- nbdists(wm, coordinates(subset), longlat=FALSE)
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
    
    lisaPlot <- tm_shape(subset) +
      tm_fill("quadrant",
              title="LISA Cluster",
              style="cat",
              palette=colorsLi,
              midpoint=0,
              labels=legend,
              id="area_nm",
              alpha=0.8,
              legend.format=list(digits=2)
      ) +
      tm_borders(alpha=0.8
      ) +
      tm_view(view.legend.position=c("right","top"),
              control.position=c("left","bottom"),
              colorNA="Black"
      ) +
      tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                   basemaps.alpha=c(0.8,0.5,0.7)
      ) +
      tm_shape(rv$subsetView) +
      tm_borders(col="black",
                 lwd=3)
    tmap_leaflet(lisaPlot, in.shiny=TRUE)
    
  })

  output$reference <- renderLeaflet({
    
    if (input$inLod=="LAD") {
      subset <- maplad_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      # subsetView <- maprgn_sp
    }
    else if (input$inLod=="Ward") {
      subset <- mapward_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    else if (input$inLod=="MSOA") {
      subset <- mapmsoa_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    else {
      subset <- maplsoa_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      # if (input$inLad=="All") {
      #   subsetView <- maprgn_sp
      # }
      # else {
      #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      # }
    }
    
    # if (input$inLad=="All"){
    #   subsetView <- maprgn_sp[,"area_nm"]
    # }
    # else {
    #   subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
    # }

    if (input$inReference=="r"){
      tmFill <- input$inMeasure
      tmTitle <- "Raw Values"
      tmStyle <- input$inBinning
      tmPalette <- "RdBu"
    }
    else if (input$inReference=="i"){
      tmFill <- "Ii"
      tmTitle <- "I-Values"
      tmStyle <- input$inBinning
      tmPalette <- "RdBu"
    }
    else {
      tmFill <- "Pr.z...0."
      tmTitle <- "P-Values"
      tmStyle <- "fixed"
      tmPalette <- colorsNBu
    }
    
      refPlot <- tm_shape(refDf) +
        tm_fill(tmFill,
                title=tmTitle,
                style=tmStyle,
                n=input$inN,
                breaks=c(0,0.001,0.01,0.05,0.1,1),
                palette=tmPalette,
                midpoint=0,
                id="area_nm",
                alpha=0.8,
                legend.format=list(digits=3)
        ) +
        tm_borders(alpha=0.8
        ) +
        tm_view(view.legend.position=c("right","top"),
                control.position=c("left","bottom"),
                colorNA="Black"
        ) +
        tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                     basemaps.alpha=c(0.8,0.5,0.7)
        ) +
        tm_shape(rv$subsetView) +
        tm_borders(col="black",
          lwd=3)
      tmap_leaflet(refPlot, in.shiny=TRUE)

  })


observe({
  input$inLod
  input$inMeasure
  input$inLisaMethod
  input$inLisaSignificance
  input$k
  input$inReference
  input$inBinning
  input$inN
  coords3 <- ladbbox[ladbbox$area_nm==input$inLad,c("xmin","ymin","xmax","ymax")]
  if (!is.null(coords3)) {
    leafletProxy("reference") %>%
      fitBounds(coords3$xmin,
                  coords3$ymin,
                  coords3$xmax,
                  coords3$ymax)
    leafletProxy("lisa") %>%
      fitBounds(coords3$xmin,
                  coords3$ymin,
                  coords3$xmax,
                  coords3$ymax)
  }
}, priority=2)

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

# observe({
#   coords2 <- input$reference_bounds
#   if (!is.null(coords2)) {
#     leafletProxy("lisa") %>%
#       fitBounds(coords2$west,
#                 coords2$south,
#                 coords2$east,
#                 coords2$north)
#   }
# }, priority=1)


# -----Clustering functions
clusterset <- "global"

clustersetSelect <- reactive({
  if (input$inLodc=="LAD") {
    maplad_sp
  }
  else if (input$inLodc=="Ward") {
    mapward_sp
  }
  else if (input$inLodc=="MSOA") {
    mapmsoa_sp 
  }
  else {
    maplsoa_sp 
  }
})

output$cluster_left <- renderLeaflet({
  
  clusterset <<- clustersetSelect()
  
  if (input$clustMethod=='SK'){
    ## Skater ##  
    vars <- input$inMeasureCluster
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
    
    clusterset.nb <- poly2nb(clusterset)
    
    lcosts <- nbcosts(clusterset.nb,sdat)
    
    clusterset.w <- nb2listw(clusterset.nb,lcosts,style="B")
    
    mpsz.mst <- mstree(clusterset.w)
    
    clusterGrp <- skater(mpsz.mst[,1:2],sdat,input$inClusterSize-1)
    
    groups <- as.factor(clusterGrp$groups)
    #clusterset$cluster <- as.matrix(groups)
    
    ## Skater ##  
  } else {  
    ## H Clustering ##
    vars <- input$inMeasureCluster
    #  str(input$inMeasure2)
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
    
    D0 <- dist(sdat, method = 'euclidean')
    
    if (input$clustMethod=='HC'){
      hclust_scaled <- hclust(D0, method = input$inAggloMethod) 
    } else {
      # Build neighbourhood list based on regions with adjacent boundries
      list.nb <- spdep::poly2nb(clusterset)
      # Create adjacency matrix
      A <- spdep::nb2mat(list.nb,style="W", zero.policy = TRUE)
      diag(A) <- 1
      colnames(A) <- rownames(A)
      D1 <- as.dist(1-A)
      hclust_scaled <- hclustgeo(D0,D1,alpha=input$inAlpha)
    }
    
    groups <- as.factor(cutree(hclust_scaled, k=input$inClusterSize))
    #clusterset$cluster <- as.matrix(groups)
    
    ## H Clustering ##
  }
  
  clusterset$cluster <<- as.matrix(groups)
  
  if (input$inLodc!="LAD") {
    if (input$inLadc!="All") {
      clusterset <- clusterset[clusterset$lad_nm==input$inLadc,] 
    }
  } 
  
  clusterPlot <- tm_shape(clusterset) +
    tm_polygons("cluster", title="Clusters", id="area_nm") +
    tm_format("World") 
  # +
  # tm_borders(alpha=0.8
  # ) +
  # tm_view(view.legend.position=c("right","bottom"),
  #         control.position=c("left","bottom"),
  #         colorNA="Black"
  # ) +
  # tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
  #              basemaps.alpha=c(0.8,0.5,0.7)
  # )
  
  tmap_leaflet(clusterPlot, in.shiny=TRUE)
  
})

output$findkplot <- renderPlot({
  
  clusterset <- clustersetSelect()
  
  vars <- input$inMeasureCluster
  #  str(input$inMeasure2)
  sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
  
  D0 <- dist(sdat, method = 'euclidean')
  tesco_clust <- hclust(D0, method = "ward.D")
  num_k <- find_k(tesco_clust)
  plot(num_k)
  
})

output$corrplot <- renderPlot({
  
  #clusterset <- clusterset()
  
  vars <- input$inMeasureCluster
  #  str(input$inMeasure2)
  sdat_corr <- data.frame(scale(as.data.frame(clusterset)[,vars]))
  
  clust.cor = cor(sdat_corr)
  
  corrplot.mixed(clust.cor, 
                 lower = "ellipse", 
                 upper = "number",
                 #p.mat = ward.sig$p,
                 sig.level = .05,
                 tl.pos = "lt",
                 bg = "white",
                 diag = "l",
                 order="AOE",
                 tl.col = "black")
})

output$cluster_right <- renderPlot({
  
  #clusterset <- clustersetSelect()
  #l_clusterset <- clusterset
  vars <-  input$inMeasureCluster
  k <- input$inClusterSize
  m <- input$clustMethod
  inAlpha <- input$inAlpha
  #  str(input$inMeasure2)
  sdat2 <- data.frame(scale(as.data.frame(clusterset)[,vars]))
  
  sdat2["cluster"] <- as.factor(clusterset$cluster)
  #str(sdat2$cluster)
  
  # bp <- ggplot(sdat2, aes(x=dose, y=len, group=cluster)) + 
  #   geom_boxplot(aes(fill=dose))
  # bp
  ggparcoord(data = sdat2,
             columns = c(1:(ncol(sdat2)-1)),
             groupColumn = "cluster",
             scale = "uniminmax",
             boxplot = TRUE,
             title = "Parallel Coord. Tesco Measures") +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())+
    facet_wrap(~ cluster)
  
  # boxplot(sdat2[,1:(ncol(sdat2)-1)],
  #         main = "Multiple boxplots for comparision",
  #         #at = c(1,2,4,5),
  #         #names = c("ozone", "normal", "temp", "normal"),
  #         las = 2,
  #         #col = c("orange","red"),
  #         border = "brown",
  #         #horizontal = TRUE,
  #         notch = TRUE
  # )+
  #   facet_wrap(~ cluster)
  
  
})

output$alphaplot <- renderPlot({ 
  
  clusterset <- clustersetSelect()
  
  ## H Clustering ##
  vars <- input$inMeasureCluster
  sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
  
  D0 <- dist(sdat, method = 'euclidean')
  
  
  # Build neighbourhood list based on regions with adjacent boundries
  list.nb <- spdep::poly2nb(clusterset)
  # Create adjacency matrix
  A <- spdep::nb2mat(list.nb,style="W", zero.policy = TRUE)
  diag(A) <- 1
  colnames(A) <- rownames(A)
  D1 <- as.dist(1-A)
  
  # Choosing Alpha
  range.alpha <- seq(0,1,0.1)
  choicealpha(D0,D1,range.alpha,input$inClusterSize,graph=TRUE)
})

output$aggloplot <- renderTable({ 
  
  clusterset <- clustersetSelect()
  
  vars <- input$inMeasureCluster
  sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
  
  tesco_matrix <- data.matrix(sdat)
  tesco_d <- dist((tesco_matrix), method = "euclidean")
  
  dend_expend(tesco_d)[[3]]
  
})

output$dendoplot <- renderPlot({ 
  
  dendro_data_k <- function(hc, k) {
    
    hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
    seg       <-  hcdata$segments
    labclust  <-  cutree(hc, k)[hc$order]
    segclust  <-  rep(0L, nrow(seg))
    heights   <-  sort(hc$height, decreasing = TRUE)
    height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
    
    for (i in 1:k) {
      xi      <-  hcdata$labels$x[labclust == i]
      idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
      idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
      idx3    <-  seg$yend < height
      idx     <-  idx1 & idx2 & idx3
      segclust[idx] <- i
    }
    
    idx                    <-  which(segclust == 0L)
    segclust[idx]          <-  segclust[idx + 1L]
    hcdata$segments$clust  <-  segclust
    hcdata$segments$line   <-  as.integer(segclust < 1L)
    hcdata$labels$clust    <-  labclust
    
    hcdata
  }
  
  set_labels_params <- function(nbLabels,
                                direction = c("tb", "bt", "lr", "rl"),
                                fan       = FALSE) {
    if (fan) {
      angle       <-  360 / nbLabels * 1:nbLabels + 90
      idx         <-  angle >= 90 & angle <= 270
      angle[idx]  <-  angle[idx] + 180
      hjust       <-  rep(0, nbLabels)
      hjust[idx]  <-  1
    } else {
      angle       <-  rep(0, nbLabels)
      hjust       <-  0
      if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
      if (direction %in% c("tb", "rl")) { hjust <- 1 }
    }
    list(angle = angle, hjust = hjust, vjust = 0.5)
  }
  plot_ggdendro <- function(hcdata,
                            direction   = c("lr", "rl", "tb", "bt"),
                            fan         = FALSE,
                            scale.color = NULL,
                            branch.size = 1,
                            label.size  = 3,
                            nudge.label = 0.01,
                            expand.y    = 0.1) {
    
    direction <- match.arg(direction) # if fan = FALSE
    ybreaks   <- pretty(segment(hcdata)$y, n = 5)
    ymax      <- max(segment(hcdata)$y)
    
    ## branches
    p <- ggplot() +
      geom_segment(data         =  segment(hcdata),
                   aes(x        =  x,
                       y        =  y,
                       xend     =  xend,
                       yend     =  yend,
                       linetype =  factor(line),
                       colour   =  factor(clust)),
                   lineend      =  "round",
                   show.legend  =  FALSE,
                   size         =  branch.size)
    
    ## orientation
    if (fan) {
      p <- p +
        coord_polar(direction = -1) +
        scale_x_continuous(breaks = NULL,
                           limits = c(0, nrow(label(hcdata)))) +
        scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_x_continuous(breaks = NULL)
      if (direction %in% c("rl", "lr")) {
        p <- p + coord_flip()
      }
      if (direction %in% c("bt", "lr")) {
        p <- p + scale_y_reverse(breaks = ybreaks)
      } else {
        p <- p + scale_y_continuous(breaks = ybreaks)
        nudge.label <- -(nudge.label)
      }
    }
    
    # labels
    labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
    hcdata$labels$angle <- labelParams$angle
    
    p <- p +
      geom_text(data        =  label(hcdata),
                aes(x       =  x,
                    y       =  y,
                    label   =  label,
                    colour  =  factor(clust),
                    angle   =  angle),
                vjust       =  labelParams$vjust,
                hjust       =  labelParams$hjust,
                nudge_y     =  ymax * nudge.label,
                size        =  label.size,
                show.legend =  FALSE)
    
    # colors and limits
    if (!is.null(scale.color)) {
      p <- p + scale_color_manual(values = scale.color)
    }
    
    ylim <- -round(ymax * expand.y, 1)
    p    <- p + expand_limits(y = ylim)
    
    p
  }
  
  if(input$inLodc!='LSOA'){
    dummyLod <- input$inLodc
    vars <- input$inMeasureCluster
    #  str(input$inMeasure2)
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
    row.names(sdat) <- clusterset$area_id
    #str(clusterset)
    
    D <- dist(sdat, method = 'euclidean')
    
    hc  <- hclust(D)
    hc$labels <- as.factor(clusterset$area_nm)
    
    hcdata <- dendro_data_k(hc, input$inClusterSize)
    
    if(input$inDendoGraph=='tree'){
      p <- plot_ggdendro(hcdata,
                         direction   = "lr",
                         expand.y    = 0.2)
      p
    } else {
      p <- plot_ggdendro(hcdata,
                         fan         = TRUE,
                         #scale.color = cols,
                         label.size  = 2,
                         nudge.label = 0.02,
                         expand.y    = 0.4)
      
      mytheme <- theme(panel.background = element_rect(fill = "white"))
      
      p + theme_void() + mytheme
    }
  }
})



# -----GWR functions
observe({
rv$variableSelect <- input$GwrX
updateSelectInput(session, inputId="Gwr1Reference",
                  label="Reference Value",
                  choices=c("Local R2"="Local_R2",rv$variableSelect)
)
})


output$gwr1 <- renderLeaflet({
  

  if (input$GwrLod=="LAD") {
    GwrDataSp <- maplad_sp
    GwrDataSf <- maplad_sf
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  else if (input$GwrLod=="Ward") {
    GwrDataSp <- mapward_sp
    GwrDataSf <- mapward_sf
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  else {
    GwrDataSp <- mapmsoa_sp
    GwrDataSf <- mapmsoa_sf
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  
  if (input$GwrLad=="All"){
    rv$subsetGwrView <- maprgn_sf[,"area_nm"]
  }
  else {
    rv$subsetGwrView <- maplad_sf[maplad_sf$area_nm==input$GwrLad,"area_nm"]
  }
  

  GwrFormula <- as.formula(paste(input$GwrY,paste(input$GwrX, collapse="+"), sep="~"))
  if (input$GwrAutoBandwidth==1) {
    GwrBw <- bw.gwr(GwrFormula, data=GwrDataSp, approach=input$GwrApproach, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=FALSE)
  }
  else {
    GwrBw <- input$ManualBandwidth
  }
  
  rv$Gwr <- gwr.basic(GwrFormula, data=GwrDataSp, bw=GwrBw, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=FALSE, cv=TRUE)
  var.n<-length(rv$Gwr$lm$coefficients)
  dp.n<-length(rv$Gwr$lm$residuals)
  #cat(file=stderr(), "variableSelect:", variableSelect, "\n")
  rv$GwrDiagnostic <- as.data.frame(rv$Gwr$GW.diagnostic) %>%
    mutate(lm_RSS=sum(rv$Gwr$lm$residuals^2)) %>%
    mutate(lm_AIC=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*(var.n + 1)) %>%
    mutate(lm_AICc=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*dp.n*(var.n+1)/(dp.n-var.n-2)) %>%
    mutate(lm_R2=summary(rv$Gwr$lm)$r.squared) %>%
    mutate(lm_R2.adj=summary(rv$Gwr$lm)$adj.r.squared) %>%
    mutate(bw=rv$Gwr$GW.arguments$bw) %>%
    mutate(dp.n=dp.n)
  GwrSDF <- as.data.frame(rv$Gwr$SDF)
  for (dim_ in rv$variableSelect) {
    #cat(file=stderr(), "variableSelect:", variableSelect, "\n")
    GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=length(GwrSDF)-1,lower.tail=FALSE)*2
  }
  rv$GwrResult <- GwrDataSf %>%
    select(area_id,area_nm,lad_id,lad_nm,geometry) %>%
    cbind(., as.matrix(GwrSDF))
  

  gwr1Plot <- tm_shape(rv$GwrResult) +
    tm_fill(input$Gwr1Reference,
            title=Gwr1Title,
            style=input$Gwr1Binning,
            n=input$Gwr1N,
            breaks=c(0,0.001,0.01,0.05,0.1,1),
            palette="RdBu",
            midpoint=0,
            id="area_nm",
            alpha=0.8,
            legend.format=list(digits=3)
    ) +
    tm_borders(alpha=0.8
    ) +
    tm_view(view.legend.position=c("right","top"),
            control.position=c("left","bottom"),
            colorNA="Black"
    ) +
    tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                 basemaps.alpha=c(0.8,0.5,0.7)
    ) +
    tm_shape(rv$subsetGwrView) +
    tm_borders(col="black",
               lwd=3)
  tmap_leaflet(gwr1Plot, in.shiny=TRUE)
  

})

output$gwr2 <- renderLeaflet({
  

  if (input$Gwr1Reference=="Local_R2") {
    GwrPV <- input$Gwr1Reference
  }
  else {
    GwrPV <- paste0(input$Gwr1Reference, "_PV")
  }
  

  gwr2Plot <- tm_shape(rv$GwrResult) +
    tm_fill(GwrPV,
            title="P-value",
            style="fixed",
            n=5,
            breaks=c(0,0.001,0.01,0.05,0.1,1),
            palette=colorsNBu,
            midpoint=0,
            id="area_nm",
            alpha=0.8,
            legend.format=list(digits=3)
    ) +
    tm_borders(alpha=0.8
    ) +
    tm_view(view.legend.position=c("right","top"),
            control.position=c("left","bottom"),
            colorNA="Black"
    ) +
    tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                 basemaps.alpha=c(0.8,0.5,0.7)
    ) +
    tm_shape(rv$subsetGwrView) +
    tm_borders(col="black",
               lwd=3)
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)
  

})


output$GwrTable <- renderTable(rv$GwrDiagnostic)
# output$GwrTable <- DT::renderDataTable(rv$GwrDiagnostic)

output$GwrSummary <- renderPrint({
  rv$Gwr
})


observe({
  input$GwrLod
  input$GwrY
  input$GwrX
  input$GwrModel
  input$GwrDistance
  input$GwrBandwidth
  input$GwrKernel
  input$GwrApproach
  input$GwrAutoBandwidth
  input$GwrManualBandwidth
  input$Gwr1Reference
  input$Gwr1Binning
  input$Gwr1N
  coordsGwr <- ladbbox[ladbbox$area_nm==input$GwrLad,c("xmin","ymin","xmax","ymax")]
  if (!is.null(coordsGwr)) {
    leafletProxy("gwr1") %>%
      fitBounds(coordsGwr$xmin,
                coordsGwr$ymin,
                coordsGwr$xmax,
                coordsGwr$ymax)
    leafletProxy("gwr2") %>%
      fitBounds(coordsGwr$xmin,
                coordsGwr$ymin,
                coordsGwr$xmax,
                coordsGwr$ymax)
  }
}, priority=2)

observe({
  coordsGwr2 <- input$gwr1_bounds
  if (!is.null(coordsGwr2)) {
    leafletProxy("gwr2") %>%
      fitBounds(coordsGwr2$west,
                coordsGwr2$south,
                coordsGwr2$east,
                coordsGwr2$north)
  }
}, priority=1)


}

# -----Create Shiny app ----
shinyApp(ui, server)
