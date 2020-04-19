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
library(reactlog)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)


# -----Load data files
load("data/maprgn_sp.rda")
load("data/maplad_sp.rda")
load("data/mapward_sp.rda")
load("data/mapmsoa_sp.rda")
load("data/maplsoa_sp.rda")
load("data/ladbbox.rda")
load("data/mapmsoa_sf.rda")


# -----All Global Parameters here

# Level of Detail
varLod <- c(
  "Local Authority District"="LAD",
  "Ward"="Ward",
  "Middle Super Output Area"="MSOA",
  "Lower Super Output Area"="LSOA"
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
  "Local Authority District"="LAD",
  "Ward"="Ward",
  "Middle Super Output Area"="MSOA"
)

# GWR Model
varGwrModel <- c(
  "Basic GWR"="gwr",
  "Generalised GWR"="ggwr"
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
                                          selectInput(inputId="inLod",
                                                      label="Level of Detail",
                                                      choices=varLod,
                                                      selected="LAD",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="inMeasure",
                                                      label="Select Variable",
                                                      choices=varMeasure1,
                                                      selected="energy_carb",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          conditionalPanel(condition="input.inLod!='LAD'",
                                          selectInput(inputId="inLad",
                                                      label="Local Authority District",
                                                      choices=varLad,
                                                      selected="All",
                                                      multiple=FALSE,
                                                      width="100%"
                                          )
                                          )
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
                                                                             min=2,
                                                                             max=30,
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
                                                            selected="p",
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
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
                                                                 ),
                                                                 sliderInput(inputId="inN",
                                                                             label="Select number of classes",
                                                                             min=2,
                                                                             max=10,
                                                                             value=5,
                                                                             width="100%"
                                                                 )
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
                             sidebarPanel(width=3, fluid=TRUE,
                                          selectInput(inputId="GwrLod",
                                                      label="Regression Level",
                                                      choices=varGwrLod,
                                                      selected="MSOA",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
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
                                          selectInput(inputId="GwrModel",
                                                      label="Regression Model",
                                                      choices=varGwrModel,
                                                      selected=NULL,
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrApproach",
                                                      label="Regression Approach",
                                                      choices=varGwrApproach,
                                                      selected="CV",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrKernel",
                                                      label="Select Kernel Method",
                                                      choices=varGwrKernel,
                                                      selected="gaussian",
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrBandwidth",
                                                      label="Bandwidth Method",
                                                      choices=varGwrBandwidth,
                                                      selected=FALSE,
                                                      multiple=FALSE,
                                                      width="100%"
                                          ),
                                          selectInput(inputId="GwrDistance",
                                                      label="Distance Method",
                                                      choices=varGwrDistance,
                                                      selected=2,
                                                      multiple=FALSE,
                                                      width="100%"
                                          )
                             ),
                             mainPanel(width=9, fluid=TRUE,
                                       fluidRow(
                                         column(6,
                                                leafletOutput("gwr1"),
                                                selectInput(inputId="Gwr1Reference",
                                                            label="Reference Value",
                                                            choices=c("Local R2"="r"
                                                            ),
                                                            selected="r",
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
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
                                                ),
                                                sliderInput(inputId="Gwr1N",
                                                            label="Select number of classes",
                                                            min=2,
                                                            max=10,
                                                            value=5,
                                                            width="100%"
                                                ),
                                                verbatimTextOutput("text1")
                                         ),
                                         column(6,
                                                leafletOutput("gwr2"),
                                                selectInput(inputId="Gwr2Reference",
                                                            label="Reference Value",
                                                            choices=c("P-Value"="p",
                                                                      "Local R2"="r",
                                                                      "Residuals"="i"
                                                                      
                                                            ),
                                                            selected="p",
                                                            multiple=FALSE,
                                                            width="100%"
                                                ),
                                                selectInput(inputId="Gwr2Binning",
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
                                                ),
                                                sliderInput(inputId="Gwr2N",
                                                            label="Select number of classes",
                                                            min=2,
                                                            max=10,
                                                            value=5,
                                                            width="100%"
                                                )
                                         )
                                       )
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
    subsetView <- maprgn_sp
    }
    else if (input$inLod=="Ward") {
      subset <- mapward_sp[,"area_nm"]
      indicator <- pull(mapward_sp@data, input$inMeasure)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
    }
    else if (input$inLod=="MSOA") {
      subset <- mapmsoa_sp[,"area_nm"]
      indicator <- pull(mapmsoa_sp@data, input$inMeasure)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
    }
    else {
      subset <- maplsoa_sp[,"area_nm"]
      indicator <- pull(maplsoa_sp@data, input$inMeasure)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
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
      tm_shape(subsetView) +
      tm_borders(col="black",
                 lwd=3)
    tmap_leaflet(lisaPlot, in.shiny=TRUE)
    
  })

  output$reference <- renderLeaflet({
    
    if (input$inLod=="LAD") {
      subset <- maplad_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      subsetView <- maprgn_sp
    }
    else if (input$inLod=="Ward") {
      subset <- mapward_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
    }
    else if (input$inLod=="MSOA") {
      subset <- mapmsoa_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
    }
    else {
      subset <- maplsoa_sp[,c("area_nm",input$inMeasure)]
      refDf <- cbind(subset, rv$lmoran)
      if (input$inLad=="All") {
        subsetView <- maprgn_sp
      }
      else {
        subsetView <- maplad_sp[maplad_sp$area_nm==input$inLad,"area_nm"]
      }
    }

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
        tm_shape(subsetView) +
        tm_borders(col="black",
          lwd=3)
      tmap_leaflet(refPlot, in.shiny=TRUE)

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
}, priority=3)

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

observe({
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
}, priority=1)


# -----GWR functions
output$gwr1 <- renderLeaflet({
  
  if (input$GwrLod=="LAD") {
    
  }
  else if (input$GwrLod=="Ward") {
    
  }
  else {
    GwrDataSp <- mapmsoa_sp
    GwrDataSf <- mapmsoa_sf
    Gwr1Title <- "Fixed Title"
    
  }
  
  
  GwrFormula <- as.formula(paste(input$GwrY,paste(input$GwrX, collapse="+"), sep="~"))
  GwrBw <- bw.gwr(GwrFormula, data=GwrDataSp, approach=input$GwrApproach, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=TRUE)
  Gwr <- gwr.basic(GwrFormula, data=GwrDataSp, bw=GwrBw, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=TRUE, cv=TRUE)
  # GwrSDF <- as.data.frame(Gwr$SDF)
  # GwrResult <- cbind(GwrDataSf, as.matrix(GwrSDF))
  var.n<-length(Gwr$lm$coefficients)
  dp.n<-length(Gwr$lm$residuals)
  variableSelect <- input$GwrX
  updateSelectInput(session, inputId="Gwr1Reference",
                    label="Reference Value",
                    choices=c("Local R2"="r",
                              setNames(variableSelect, input$GwrX)
                    )
  )
  #cat(file=stderr(), "variableSelect:", variableSelect, "\n")
  GwrDiagnostic <- as.data.frame(Gwr$GW.diagnostic) %>%
    mutate(lm_RSS=sum(Gwr$lm$residuals^2)) %>%
    mutate(lm_AIC=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*(var.n + 1)) %>%
    mutate(lm_AICc=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*dp.n*(var.n+1)/(dp.n-var.n-2)) %>%
    mutate(lm_R2=summary(Gwr$lm)$r.squared) %>%
    mutate(lm_R2.adj=summary(Gwr$lm)$adj.r.squared) %>%
    mutate(bw=Gwr$GW.arguments$bw) %>%
    mutate(dp.n=dp.n)
  GwrSDF <- as.data.frame(Gwr$SDF)
  for (dim_ in variableSelect) {
    #cat(file=stderr(), "variableSelect:", variableSelect, "\n")
    GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=length(GwrSDF)-1,lower.tail=FALSE)*2
  }
  GwrResult <- mapmsoa_sf %>%
    select(area_id,area_nm,lad_id,lad_nm,geometry) %>%
    cbind(GwrDataSf, as.matrix(GwrSDF))
  

  
  gwr1Plot <- tm_shape(GwrResult) +
    tm_fill("Local_R2",
            title=Gwr1Title,
            style=input$Gwr1Binning,
            n=input$Gwr1N,
            breaks=c(0,0.001,0.01,0.05,0.1,1),
            palette=colorsBu,
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
    ) 
    # tm_shape(subsetView) +
    # tm_borders(col="black",
    #            lwd=3)
  tmap_leaflet(gwr1Plot, in.shiny=TRUE)
  
  # output$text1 <- renderPrint({
  #   print(GwrFormula)
  # })
  
})

output$gwr2 <- renderLeaflet({
  
  gwr2Plot <- tm_shape(mapmsoa_sp) +
    tm_fill("representativeness_norm",
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
    ) 
    # tm_shape(subsetView) +
    # tm_borders(col="black",
    #            lwd=3)
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)
  
})


}

# -----Create Shiny app ----
shinyApp(ui, server)
