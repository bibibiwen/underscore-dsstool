####LOADING PACKAGES####
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(rgdal)
library(rmarkdown)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(knitr)
library(patchwork)
library(ggrepel)
library(mgcv)
library(DT)

#### modules####
GAM_ui <-function(id){
  NUTS_env_shiny <- read_sf("gis_data/polygons_startingconditions_CRUTS34_EMEP2018_endingscenarios_WGS84.shp")
  ns<-NS(id)
  fluidPage(
    fluidRow(column(12,#Inputs
                    #block with inputs in starting conditions
                    column(width = 6,
                           #Input the map
                           fluidRow(
                             sidebarPanel(tags$h3("1. Select a region"),
                                          uiOutput(ns("Select_region"))),
                             column(width = 8,leafletOutput(ns("map")))
                           ),
                           tags$br(),
                           
                           fluidRow(#block with the inputs for the scenarios 
                             sidebarPanel(width =4, h3("2. Select a scenario"),
                                          selectInput(inputId = ns("Ndep_scenario"),label=HTML(paste0("N deposition scenario",tags$br())), choices = c("Business as usual","Clean Air Outlook EU")),
                                          selectInput(inputId = ns("SSP_scenario"), 
                                                      label = HTML(paste0("MAT in a Shared Socioeconomic Pathway (SSP) scenario",
                                                                          tags$br())),choices = c("SSP 1 - Sustainability",
                                                                                                  "SSP 2 - Middle of the road",
                                                                                                  "SSP 3 - Regional rivalry",
                                                                                                  "SSP 5 - Fossil-fueled development")),
                                          strong("Canopy closure"), tags$br(),
                                          "Forest managers can influence the light availability at the forest floor. A closed canopy can buffer effects of environmental change.",
                             ),
                             
                             sidebarPanel(width = 4, uiOutput(ns("title_start")), 
                                          sliderInput(inputId = ns("Ndep_start"), label = HTML(paste0("Nitrogen (N) deposition",tags$br() , "(kg N ha",tags$sup("-1 "),"y",tags$sup(-1),")")),value = 20, min = NUTS_env_shiny%>%as_tibble()%>%slice_min(Ndep)%>%pull(Ndep), max = NUTS_env_shiny%>%as_tibble()%>%slice_max(Ndep)%>%pull(Ndep), step = 1),
                                          sliderInput(inputId = ns("MAT_start"), label = HTML(paste0("Mean Annual Temperature (MAT)",  tags$br(), "(°C)")),value = 10, min = NUTS_env_shiny%>%as_tibble()%>%slice_min(MAT)%>%pull(MAT), max = NUTS_env_shiny%>%as_tibble()%>%slice_max(MAT)%>%pull(MAT), step = 0.1), #richness models badly when MAT > 12 (outside "trained" range, so build in some warning system)
                                          selectInput(inputId = ns("canopy_start"), label = "Canopy closure", choices = c("Closed","Intermediate (selective harvest)","Open"), selected = "Closed")),
                             
                             #block with inputs for future conditions
                             sidebarPanel(width = 4,uiOutput(ns("title_end")), #dynamic title that shows the year of when the simulation ends and asks for those target conditions
                                          sliderInput(inputId = ns("Ndep_end"), label =  HTML(paste0("N deposition in scenario",tags$br() ,"(kg N ha",tags$sup("-1 "),"y",tags$sup(-1),")")),value = 20, min = NUTS_env_shiny%>%as_tibble()%>%slice_min(Ndep)%>%pull(Ndep), max = NUTS_env_shiny%>%as_tibble()%>%slice_max(Ndep)%>%pull(Ndep),step = 1),
                                          sliderInput(inputId = ns("MAT_end"), label = HTML(paste0("Difference in MAT",  tags$br(), "(°C)")),value = 0, min = -0.5, max =4, step = 0.1), #make this value dynamic: depends on what input slider says
                                          selectInput(inputId = ns("canopy_end"), label = "Canopy closure", choices = c("Closed","Intermediate (selective harvest)","Open"), selected = "Open"))
                           )),
                    #Outputs
                    column(width = 6, h3("3. Model output"), h4("Predictions with 95% confidence interval"),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = ns("richness")),plotOutput(outputId = ns("cover")))),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = ns("woody_ratio")),
                                         plotOutput(outputId = ns("percentage_specialists"))))
                           
                    ),
                    #block that gives remarks on simulation inputs
                    column(12,
                           h2("Your settings from a policy perspective"),
                           strong("Nitrogen deposition"),
                           uiOutput(ns("Ndep_remark")),
                           h2(" "),
                           strong("Global warming"),
                           uiOutput(ns("MAT_remark")),
                           h2(""), #empty space for outlining
                           h2(""),
                           strong("Canopy Closure"),
                           tags$br(),
                           "Forest management, canopy closure and light availability are tightly linked. Closed canopies are dark and favour forest specialists, while open canopies are bright and favour forest generalists. Forest managers can reduce the impact of tree harvest on canopy closure via selective cutting. Choose the canopy closure in three categories: Closed (100%), Intermediately closed/selective harvest (75%), Open (25%).",
                           tags$br(),
                           h1(" ")
                    )
                    
    ))
  )
}

GAM_server<-function(id){
  moduleServer(id,function(input, output,session) {
    
    rich_gam_dyn_shiny <- readRDS(file = "rich_gam_dyn.RDS",.GlobalEnv)
    woody_gam_dyn_shiny<- readRDS(file = "woody_gam_dyn.RDS",.GlobalEnv)
    cover_gam_dyn_shiny<-readRDS(file = "cover_gam_dyn.RDS",.GlobalEnv)
    specialist_gam_dyn_shiny<-readRDS(file = "specialist_gam_dyn.RDS",.GlobalEnv)
    
    NUTS_env_shiny <- read_sf("gis_data/polygons_startingconditions_CRUTS34_EMEP2018_endingscenarios_WGS84.shp")
    
    #### map rendering ######
    #map code: render the map and include the pop-up text bubble
    
    output$map <- renderLeaflet({
      leaflet(NUTS_env_shiny) %>%
        addTiles()%>%
        addPolygons( layerId = ~NUTS_ID, popup= paste("<b>",NUTS_env_shiny$NUTS_NAME,"<b>",
                                                      "<br>MAT =", NUTS_env_shiny$MAT, "°C",
                                                      "<br>MAP =", NUTS_env_shiny$MAP,"mm",
                                                      "<br>Ndep =", NUTS_env_shiny$Ndep,"kg N/ha/y",
                                                      "<br>pH =", NUTS_env_shiny$pH), weight = 1.2)
      
    })
    
    #observe code: dynamic response in text and other items when  clicking on the map
    observe({
      event <- input$map_shape_click
      
      output$ID_NUTS<-renderText(NUTS_env_shiny$NUTS_ID[NUTS_env_shiny$NUTS_ID == event$id]) #at least this gives the ID of the value that I need
      # visuals_table<-reactive() #at least this seems to subset into a data
      
      updateSliderInput(session, inputId = "MAT_start", 
                        value = ifelse(is.null(event$id),input$MAT_start,NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(MAT)))
      updateSliderInput(session, inputId = "Ndep_start", 
                        value = ifelse(is.null(event$id),input$Ndep_start,NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep))) 
      MAP_start <- ifelse(is.null(event$id),MAP_start,NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(pH))
      pH_start<-ifelse(is.null(event$id),pH_start,NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(pH))
      
      region_remarkcode<-reactive({
        if(is.null(event$id)){
          "Select a highlighted region on the map."
        }else{
          HTML(paste0("The selected region is ", 
                      NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(NUTS_NAME), 
                      " (",NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(CNTR_CODE),
                      "). The sliders have moved according to the expected conditions in these scenarios. You have the option to move the sliders to alter the conditions within this region. Click on a region again to return to the expected conditions according to the selected scenario."))
        }
      })
      output$Select_region<-renderUI({region_remarkcode()
      })
      
      Ndep_remarkcode<-reactive({
        
        if(is.null(event$id)){
          updateSliderInput(session, inputId = "Ndep_end", value = 20) 
          
          "Select a region."}else{
            
            
            if(input$Ndep_scenario == "Business as usual"){
              if(input$Ndep_start > NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round()){
                
                updateSliderInput(session, inputId = "Ndep_end", value = input$Ndep_start) 
                HTML(paste0("Business as usual (no change in N deposition) leads to an excess of ",input$Ndep_start - NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round(), " kg N ha",tags$sup("-1 "),"y",tags$sup(-1), " according to the " ,tags$a(href="https://ec.europa.eu/environment/air/clean_air/outlook.htm", "Clean Air Outlook")," of the EU.
                         The Clean Air Outlook target for this region is ", NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round(), " kg N ha",tags$sup("-1 "),"y",tags$sup(-1)))
              }else{
                updateSliderInput(session, inputId = "Ndep_end", value = input$Ndep_start) 
                
                HTML(paste0("The current N deposition for this region already meets the policy target of ", NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round(), " kg N ha",tags$sup("-1 "),"y",tags$sup(-1), " according to the " ,tags$a(href="https://ec.europa.eu/environment/air/clean_air/outlook.htm", "Clean Air Outlook")," of the EU."))
              }   
              
            }else{
              updateSliderInput(session, inputId = "Ndep_end", value = NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round()) 
              
              
              HTML(paste0("The N deposition target value for this region from 2030 onwards is ",NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(Ndep_CLE)%>%round(), " kg N ha",tags$sup("-1 "),"y",tags$sup(-1), " according to the " ,tags$a(href="https://ec.europa.eu/environment/air/clean_air/outlook.htm", "Clean Air Outlook")," of the EU."))
              
            }
          }
        })
      
      output$Ndep_remark<-renderUI({Ndep_remarkcode()
      })
      
      MAT_remarkcode<-reactive({
        
        if(is.null(event$id)){
          updateSliderInput(session, inputId = "MAT_end", value = 0) 
          "Select a region."}else{
            
            if(input$SSP_scenario ==  "SSP 1 - Sustainability"){
              MAT_SSP1 = NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(MAT_SSP126)%>%round(digits = 1)
              updateSliderInput(session, inputId = "MAT_end", value = MAT_SSP1 - input$MAT_start) 
              HTML(paste0(tags$a(href="https://www.worldclim.org/data/cmip6/cmip6climate.html", "The MAT target value")," for this region for the period 2041 - 2060 under the SSP126 scenario is ", MAT_SSP1,"°C.
                        In this green scenario, the world shifts gradually, but pervasively, toward a more sustainable path, emphasizing more inclusive development that respects perceived environmental boundaries. 
Management of the global commons slowly improves, educational and health investments accelerate the demographic transition, and the emphasis on economic growth shifts toward a broader emphasis on human well-being.
Driven by an increasing commitment to achieving development goals, inequality is reduced both across and within countries. 
                         Consumption is oriented toward low material growth and lower resource and energy intensity."))
            } else{
              if(input$SSP_scenario ==  "SSP 2 - Middle of the road"){
                MAT_SSP2 = NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(MAT_SSP245)%>%round(digits = 1)
                updateSliderInput(session, inputId = "MAT_end", value = (MAT_SSP2 - input$MAT_start) ) 
                HTML(paste0(tags$a(href="https://www.worldclim.org/data/cmip6/cmip6climate.html", "The MAT target value")," for this region for the period 2041 - 2060 under the SSP245 scenario is ", MAT_SSP2,"°C.
                        The world follows a path in which social, economic, and technological trends do not shift markedly from historical patterns. Development and income growth proceeds unevenly, with some countries making relatively good progress while others fall short of expectations. Global and national institutions work toward but make slow progress in achieving sustainable development goals. Environmental systems experience degradation, although there are some improvements and overall the intensity of resource and energy use declines. Global population growth is moderate and levels off in the second half of the century. Income inequality persists or improves only slowly and challenges to reducing vulnerability to societal and environmental changes remain."))
              }else{
                if(input$SSP_scenario ==  "SSP 3 - Regional rivalry"){
                  MAT_SSP3 = NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(MAT_SSP370)%>%round(digits = 1)
                  updateSliderInput(session, inputId = "MAT_end", value = (MAT_SSP3- input$MAT_start) ) 
                  HTML(paste0(tags$a(href="https://www.worldclim.org/data/cmip6/cmip6climate.html", "The MAT target value")," for this region for the period 2041 - 2060 under the SSP370 scenario is ", MAT_SSP3,"°C.
A resurgent nationalism, concerns about competitiveness and security, and regional conflicts push countries to increasingly focus on domestic or, at most, regional issues. Policies shift over time to become increasingly oriented toward national and regional security issues. Countries focus on achieving energy and food security goals within their own regions at the expense of broader-based development. Investments in education and technological development decline. Economic development is slow, consumption is material-intensive, and inequalities persist or worsen over time. Population growth is low in industrialized and high in developing countries. A low international priority for addressing environmental concerns leads to strong environmental degradation in some regions."))               
                } else{
                  if(input$SSP_scenario ==  "SSP 5 - Fossil-fueled development"){
                    MAT_SSP5 = NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(MAT_SSP585)%>%round(digits = 1)
                    updateSliderInput(session, inputId = "MAT_end", value = (MAT_SSP5 - input$MAT_start) ) 
                    HTML(paste0(tags$a(href="https://www.worldclim.org/data/cmip6/cmip6climate.html", "The MAT target value ")," for this region for the period 2041 - 2060 under the SSP585 scenario is ", MAT_SSP5,"°C.
This world places increasing faith in competitive markets, innovation and participatory societies to produce rapid technological progress and development of human capital as the path to sustainable development. Global markets are increasingly integrated. There are also strong investments in health, education, and institutions to enhance human and social capital. At the same time, the push for economic and social development is coupled with the exploitation of abundant fossil fuel resources and the adoption of resource and energy intensive lifestyles around the world. All these factors lead to rapid growth of the global economy, while global population peaks and declines in the 21st century. Local environmental problems like air pollution are successfully managed. There is faith in the ability to effectively manage social and ecological systems, including by geo-engineering if necessary."))               }
                }
                
              }           
              
            }
            
          }
      })
      
      output$MAT_remark<-renderUI(MAT_remarkcode())}) 
    
    
    ##### Model calculations ######
    
    #static settings for models
    start_year = 2020 #matches the IPCC report
    end_year = 2050 #matches the IPCC report
    
    start_year_diff = 0
    end_year_diff = end_year-start_year
    
    value_canopy_start<-reactive({ifelse(input$canopy_start == "Closed",100,
                                         ifelse(input$canopy_start == "Open",25,75))}) #classifications as closed (100%) and open (25%) forest and intermediate (75)
    value_canopy_end<-reactive({ifelse(input$canopy_end == "Closed",100,
                                       ifelse(input$canopy_end == "Open",25,75))})#classifications as closed (100%) and open (25%) forest (75)
    
    MAP_start= 800
    pH_start= 5.5
    
    #dynamic settings for models here  
    #define input dataframe.All changes in environmental variables are linearly, so slope (delta) and intercepts of these linear relationships are calculated
    inputs_shiny<-reactive({tibble::tibble(Survey_Year_seq= seq(start_year_diff,end_year_diff,by= 1), 
                                           Survey_Year_diff = end_year_diff,
                                           Survey_Year = seq(start_year,end_year,by= 1),
                                           delta_N = (input$Ndep_end - input$Ndep_start)/(2030-start_year),#Ndep scenario stops in 2030
                                           intercept_N = input$Ndep_start - delta_N*start_year_diff,
                                           delta_MAT = (input$MAT_end)/(end_year_diff-start_year_diff),
                                           intercept_MAT =input$MAT_start - delta_MAT*start_year_diff,
                                           delta_canopy = (value_canopy_end()-value_canopy_start())/(end_year_diff-start_year_diff),
                                           intercept_canopy = value_canopy_start()-delta_canopy*start_year_diff
    )%>%
        dplyr::mutate(N_linear = (delta_N*Survey_Year_seq+intercept_N), N_rate = ifelse(Survey_Year < 2030,N_linear,input$Ndep_end), MAT_time = delta_MAT*Survey_Year_seq+intercept_MAT,tree_cover_Fisch = delta_canopy*Survey_Year_seq+intercept_canopy)%>% #MAP_time = delta_MAP*Survey_Year_seq+intercept_MAP,)%>%
        dplyr::mutate(Survey_Date = "new",Plot_size = 100, MAP_time = MAP_start, pH = pH_start)}) 
    
    #predictions of GAM models are calculated here. The dataframe with inputs is stitched to these resulting dataframes for use in plotting 
    
    rich_pred_shiny<- reactive({predict.gam(rich_gam_dyn_shiny,inputs_shiny(),exclude = "s(Survey_Year)", type = "response", se.fit = FALSE)%>% #fit with survey year excluded
        as_tibble()%>% 
        dplyr::rename(richness_fit = value)%>% #when se.fit = false, the fitted value is not returned as "fit" but as "value"
        cbind(predict.gam(rich_gam_dyn_shiny,inputs_shiny(), type = "response", se.fit = TRUE))%>% #se.fit with survey year included
        dplyr::select(-fit)%>%
        dplyr::rename(richness_se = se.fit)%>%
        as_tibble()%>% 
        cbind(inputs_shiny())%>%
        as_tibble()
    })
    
    cover_pred_shiny<- reactive({predict.gam(cover_gam_dyn_shiny,inputs_shiny(), exclude= "s(Survey_Year)",type = "response", se.fit = FALSE)%>% #fit with survey year excluded
        as_tibble()%>%
        dplyr::rename(cover_fit = value)%>% #fitted value is saved under "value" when se.fit = FALSE
        cbind(predict.gam(cover_gam_dyn_shiny,inputs_shiny(), type = "response", se.fit = TRUE))%>% #se.fit with survey year included
        dplyr::select(-fit)%>%
        dplyr::rename(cover_se = se.fit)%>%
        as_tibble()%>% 
        cbind(inputs_shiny())%>%
        as_tibble()
    })
    
    woody_pred_shiny<- reactive({predict.gam(woody_gam_dyn_shiny,inputs_shiny(),exclude = "s(Survey_Year)", type = "response", se.fit = FALSE)%>% #predict with Survey year not included 
        as_tibble()%>%
        dplyr::rename(woody_fit = value)%>% #fitted value is saved under "value" when se.fit = FALSE
        cbind(predict.gam(woody_gam_dyn_shiny,inputs_shiny(), type = "response", se.fit = TRUE))%>% #se.fit with survey year included
        dplyr::select(-fit)%>%
        dplyr::rename(woody_se = se.fit)%>%
        as_tibble()%>% 
        cbind(inputs_shiny())%>%
        as_tibble()
      
      
    })
    
    specialist_pred_shiny<- reactive({predict(specialist_gam_dyn_shiny,inputs_shiny(),exclude="s(Survey_Year)", type = "response", se.fit=FALSE)%>% #predict with survey year excluded 
        as_tibble()%>%
        dplyr::rename(specialist_fit = value)%>% #fitted value is saved under "value" when se.fit = FALSE
        cbind(predict.gam(specialist_gam_dyn_shiny,inputs_shiny(), type = "response", se.fit = TRUE))%>% #se.fit with survey year year included
        dplyr::select(-fit)%>%
        dplyr::rename(specialist_se = se.fit)%>%
        as_tibble()%>% 
        cbind(inputs_shiny())%>%
        as_tibble()
      
      
    })
    
    #### Visual outputs here #####  
    
    #dynamic titles to show the years of starting end future conditions  
    output$title_start<-renderUI({
      h4(paste("Conditions in ",start_year))
    })
    output$title_end<-renderUI({
      h4(paste("Conditions in ",end_year))
    })
    
    ##### Plots of predictions from GAM models ######
    output$richness<-renderPlot({
      ggplot2::ggplot(rich_pred_shiny(),aes(x=Survey_Year,y = richness_fit))+
        geom_ribbon(aes(ymin=richness_fit-1.96*richness_se, ymax =richness_fit+1.96*richness_se), alpha = 0.1)+
        geom_line(size = 1)+
        ylab("number of species")+
        xlab("Year")+
        theme_bw(base_size = 18)+
        expand_limits(y=c(0,30))+
        ggtitle("Species richness")
    })
    
    output$cover<-renderPlot({
      ggplot2::ggplot(cover_pred_shiny(),aes(x=Survey_Year,y = 100*cover_fit))+ #fit was from beta regression, so *100 for percentage 
        geom_ribbon(aes(ymin=(cover_fit-1.96*cover_se)*100, ymax =(cover_fit+1.96*cover_se)*100), alpha = 0.1)+
        geom_line(size = 1)+
        ylab("Total cover (%)")+
        xlab("Year")+
        theme_bw(base_size = 18)+
        expand_limits(y=c(0,100))+
        ggtitle("Understorey vegetation cover")
    })
    
    output$woody_ratio<-renderPlot({
      ggplot2::ggplot(woody_pred_shiny(),aes(x=Survey_Year,y = 100*woody_fit))+
        geom_ribbon(aes(ymin=(woody_fit-1.96*woody_se)*100, ymax =(woody_fit+1.96*woody_se)*100), alpha = 0.1)+
        geom_line(size = 1)+
        ylab("Proportion of woody species (%)")+
        xlab("Year")+
        theme_bw(base_size = 18)+
        expand_limits(y=c(0,40))+
        ggtitle("Woody species vs herbaceous species")
    })
    
    output$percentage_specialists<-renderPlot({
      ggplot2::ggplot(specialist_pred_shiny(),aes(x=Survey_Year,y = 100*specialist_fit))+
        geom_ribbon(aes(ymin=(specialist_fit-1.96*specialist_se)*100, ymax =(specialist_fit+1.96*specialist_se)*100), alpha = 0.1)+
        geom_smooth(se=F, method = "loess",  colour = "black", formula = y ~ x,  size = 1)+
        geom_line(size = 1)+
        ylab("Proportion of forest specialists(%)")+
        xlab("Year")+
        theme_bw(base_size = 18)+
        expand_limits(y=c(0,60))+
        ggtitle("Forest specialists vs forest generalists")
    })
  })
}

GBR_ui2<-function(id){
  ns<-NS(id)
  tabPanel("UnderSCORE-GBR",
           sidebarLayout(
             sidebarPanel(width = 4,
                          #style = "height: 225vh; overflow-y: auto;",
                          tags$h3("1. Select a region"),
                          uiOutput(ns("Select_region_top")),
                          leafletOutput(ns("map"), height = 400),
                          br(),
                          tags$h3("2. Setup site-specific conditions"),
                          p("Set up site-specific conditions at your forest site. Soil pH has moved according to your selected region, but you can adjust it. The other parameters need to be specified."),
                          br(),
                          fluidRow(
                            column(width = 6,
                                   sliderInput(inputId = ns("pH"), label = "Soil pH", value = 5, min = 3, max = 10, step = 0.1),
                                   sliderInput(inputId = ns("CN"), label = "Carbon-Nitrogen Ratio", value = 14.5, min = 5, max = 25, step = 0.1),
                                   numericInput(inputId = ns("Canopy_old"), label = "Overstorey Canopy Cover (%)", value = 68, min = 0, max = 100), 
                                   selectInput(inputId = ns("plot_size"), label = "Plot size (m²)", choices = c("≤ 200", "200-400", "≥ 400")),
                                   selectInput(inputId = ns("dominant_tree_old"), label = "Dominant Tree Species", 
                                               choices = c("Quercus robur", "Fagus sylvatica", "Alnus sp. Or Acer sp.", 
                                                           "Fraxinus excelsior", "Tilia cordata"))),
                            column(width=6,
                                   numericInput(inputId = ns("Richness_old"), label = "Understorey Species Richness (≥ 0)", value = 25, min = 0, max = 120, step = 1),
                                   helpText("Species Richness is correlated with plot size, please enter the number of species within the plot size you choosed."),
                                   numericInput(inputId = ns("Woody_old"), label = "Understorey Proportion of Woody Species (%)", value = 10, min = 0, max = 100, step = 1),
                                   numericInput(inputId = ns("UnderstoreyCover_old"), label = "Understorey Total Vegetation Cover (%)", value = 72, min = 0, max = 100, step = 1),
                                   numericInput(inputId =ns("Specialist_old"), label = "Understorey Proportion of Forest Specialists (%)", value = 52, min = 0, max = 100, step = 1)
                            )
                          ),
                          tags$h3("3. Select a scenario"),
                          p("Set up parameters linked to global change and forest management scenarios:"),
                          fluidRow(
                            selectInput(inputId = ns("Ndep_scenario"), label = HTML("N deposition scenario"), 
                                        choices = c("Business as usual", "Clean Air Outlook EU")),
                            selectInput(inputId =ns("SSP_scenario"), label = HTML("MAT and MAP in SSP scenario"), 
                                        choices = c("SSP 1 - Sustainability", "SSP 2 - Middle of the road", 
                                                    "SSP 3 - Regional rivalry", "SSP 5 - Fossil-fueled development")),
                            selectInput(inputId = ns("dominant_tree_new"), label = "Dominant Tree Species", 
                                        choices = c("Quercus robur", "Fagus sylvatica", "Alnus sp. Or Acer sp.", 
                                                    "Fraxinus excelsior", "Tilia cordata")),
                            helpText("Different dominant tree species lead to different light availability and litter quality at forest site."))
                          ,
                          p(strong("ATTENTION:"), "Click 'Confirm Simulation' to start. Select a region on the map first."),
                          actionButton(inputId =ns("Simulate"), label = "Confirm Simulation", icon = icon("check"), 
                                       style = "color: #0073C2FF; margin-left: 15px; margin-bottom: 5px;")
             ),
             mainPanel(
               tags$h3("4. Model outputs"),
               column(12, h5(strong("Table 1. Climatic and site-specific conditions at your site")),
                      DTOutput(ns("Select_region")),
                      br()
               ),
               column(12, h5(strong("Forecasting trajectories of understorey from 2030 until 2050")),
                      uiOutput(ns("Select_scenarios")),
                      fluidRow(
                        column(width = 6, plotOutput(outputId = ns("richness_new"))),
                        column(width = 6, plotOutput(outputId = ns("cover_new"))),
                        column(width = 6, plotOutput(outputId =ns("woody_new"))),
                        column(width = 6, plotOutput(outputId = ns("specialist_new")))
                      )
               ),
               column(12, h4(strong("Your settings from a policy perspective")),
                      strong("Nitrogen deposition"), 
                      uiOutput(ns("Nscenario_remark")), 
                      br(),
                      strong("Climate Change"), 
                      uiOutput(ns("SSP_remark")),
                      br(),
                      h5(strong("Forest management scenarios")),
                      p("Forest management (e.g. clear cut, dominant tree species shift) affects canopy closure, light availability and litter quality. Adjust settings as needed.")
               ),
               downloadButton(outputId = ns("downloadReport"), label = "Download Trajectory Report", 
                              icon = icon("download"), style = "color: #0073C2FF; margin-left: 15px; margin-bottom: 5px;")
             )
           )
  )
}

GBR_server2 <- function(id) {
  moduleServer(id,function(input, output, session){
    value_plot_size<-reactive({ifelse(input$plot_size == "≤ 200",100,
                                      ifelse(input$plot_size == "200-400",300,500))})
    
    #value_canopy_new<-reactive({ifelse(input$Canopy_new == "Closed(100%)",1,ifelse(input$Canopy_new == "Open(25%)",0.25,0.75))})#classifications as closed (100%) and open (25%) forest (75)
    
    
    SCA_old<-reactive({
      req(input$dominant_tree_old)
      
      SCA_values<-list(
        "Quercus robur"=2,
        "Fagus sylvatica"=4,
        "Alnus sp. Or Acer sp."=3,
        "Fraxinus excelsior"=2,
        "Tilia cordata"=4 
      )
      SCA_values[[input$dominant_tree_old]]
    })
    
    SCA_new<-reactive({
      req(input$dominant_tree_new)
      
      SCA_values<-list(
        "Quercus robur"=2,
        "Fagus sylvatica"=4,
        "Alnus sp. Or Acer sp."=3,
        "Fraxinus excelsior"=2,
        "Tilia cordata"=4 
      )
      SCA_values[[input$dominant_tree_new]]
    })
    
    
    LQ_old<-reactive({
      req(input$dominant_tree_old)
      
      LQ_values<-list(
        "Quercus robur"=2,
        "Fagus sylvatica"=2,
        "Alnus sp. Or Acer sp."=3,
        "Fraxinus excelsior"=4,
        "Tilia cordata"=2 
      )
      LQ_values[[input$dominant_tree_old]]
    })
    
    
    LQ_new<-reactive({
      req(input$dominant_tree_new)
      
      LQ_values<-list(
        "Quercus robur"=2,
        "Fagus sylvatica"=2,
        "Alnus sp. Or Acer sp."=3,
        "Fraxinus excelsior"=4,
        "Tilia cordata"=2 
      )
      LQ_values[[input$dominant_tree_new]]
    })
    
    
    MAT_new <- reactive({
      req(selected_map_values(), input$SSP_scenario)
      
      MAT_values <- list(
        "SSP 1 - Sustainability" = selected_map_values()$TP1_2050,
        "SSP 2 - Middle of the road" = selected_map_values()$TP2_2050,
        "SSP 3 - Regional rivalry" = selected_map_values()$TP3_2050,
        "SSP 5 - Fossil-fueled development" = selected_map_values()$TP5_2050
      )
      
      MAT_values[[input$SSP_scenario]]
    })
    
    
    MAP_new<-reactive({
      req(selected_map_values(),input$SSP_scenario)
      ifelse(input$SSP_scenario=="SSP 1 - Sustainability",selected_map_values()$PP1_2050,
             ifelse(input$SSP_scenario=="SSP 2 - Middle of the road",selected_map_values()$PP2_2050,
                    ifelse(input$SSP_scenario=="SSP 3 - Regional rivalry",selected_map_values()$PP3_2050,
                           selected_map_values()$PP5_2050)))
    })
    
    
    N_rate_new<-reactive({
      req(input$Ndep_scenario, selected_map_values()) 
      ifelse(input$Ndep_scenario == "Business as usual",selected_map_values()$Ndep,selected_map_values()$NCLE_2050)})
    
    ####1. output map and show information of climatic on the map####
    NUTS_env_shiny <- read_sf("gis_data/climatecondition_scenarios_30_CRUTS406_10_EMEP_NAPCP_WORLDCLIM_WGS84.shp") #MAT MAP are 30 years means
    output$map <- renderLeaflet({
      leaflet(NUTS_env_shiny) %>%
        addTiles()%>%
        addPolygons(layerId = ~NUTS_ID, popup= paste("<b>",NUTS_env_shiny$NUTS_NAME,"<b>",
                                                     "<br>MAT =",formatC(NUTS_env_shiny$MAT, format = "f", digits = 2), "°C",
                                                     "<br>MAP =", formatC(NUTS_env_shiny$MAP,format="f",digits=2),"mm",
                                                     "<br>Ndep =", formatC(NUTS_env_shiny$Ndep,format="f",digits=2),"kg N/ha/y",
                                                     "<br>pH =", formatC(NUTS_env_shiny$pH,format="f",digits=1)), weight = 1.2)
    })
    ####2. updating value of climatic conditions old and scenarios and policy interpretation####
    
    selected_map_values<-eventReactive(input$map_shape_click,{
      event<-input$map_shape_click 
      selected_row<-NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)
      return(selected_row)
    }) #select all data included in NUTS shape filr for a map_shape_click NUT ID
    
    observe(print(selected_map_values()))
    
    observeEvent(input$map_shape_click,{
      ######2.1. MAT,MAP,Ndep initial state and pH updated with map click######
      event <- input$map_shape_click 
      fmt <- function(value) formatC(value, format = "f", digits = 2)
      selected_row <- NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)
      
      ######2.4.1 Ndep scenario text update based on click on map  ####
      
      output$Nscenario_remark<-renderUI({
        if(is.null(event$id)){"Select a region."}else {
          # Filter the data frame to get the relevant row based on event$id
          # Check if the selected row exists and has data
          HTML(paste0(
            "According to the ", 
            tags$a(href = "https://ec.europa.eu/environment/air/clean_air/outlook.htm", "Clean Air Outlook"), 
            " of the European Union, the Clean Air Outlook target (CLE scenario) for this region from 2050 onwards is ", 
            fmt(selected_row$NCLE_2050),
            " kg N ha", tags$sup("-1"), " y", tags$sup(-1), "; ",
            "Business as usual (BAU scenario, no change in N deposition) leads to an excess of ", 
            fmt(selected_row$NCLE_2050 - input$Ndep_old),
            " kg N ha", tags$sup("-1"), " y", tags$sup(-1), " in 2050."
          ))
        }
      })
      
      ######2.4.2 MAT MAP scenario updates and text hints below ####
      output$SSP_remark<-renderUI({
        if(is.null(event$id)){"Select a region."}else{
          if(input$SSP_scenario ==  "SSP 1 - Sustainability"){
            HTML(paste0(
              "The MAT target values under the SSP1 - Sustainability scenario for the period of 2041-2060 is ",
              fmt(selected_row$TP1_2050)," °C; the MAP target values under the SSP1 scenario is ",
              fmt(selected_row$PP1_2050)," (mm). In this green scenario, the world shifts gradually toward a more sustainable path, emphasizing inclusive development that respects environmental boundaries. Global commons management improves, investments in education and health accelerate the demographic transition, and the emphasis on economic growth shifts toward human well-being. Inequality is reduced both across and within countries."
            ))} else{
              if(input$SSP_scenario ==  "SSP 2 - Middle of the road"){
                HTML(paste0("The MAT target values under the SSP2 - Middle of the Road scenario for the period of 2041-2060 is ",
                            fmt(selected_row$TP2_2050)," °C; the MAP target values under the SSP2 scenario is ",
                            fmt(selected_row$PP2_2050)," (mm). In the SSP2 scenario, the world follows a path where social, economic, and technological trends continue without significant deviation from historical patterns. Development progresses unevenly, with slow progress toward sustainability goals. Environmental systems experience degradation, but resource and energy use intensity declines."))
              }else{
                if(input$SSP_scenario ==  "SSP 3 - Regional rivalry"){
                  HTML(paste0("The MAT target values under the SSP3 - Regional rivalry scenario for the period of 2041-2060 is ", 
                              fmt(selected_row$TP3_2050)," °C; the MAP target values under the SSP3 scenario is ",
                              selected_row$PP3_2050," (mm). In this scenario, increasing nationalism and regional conflicts shift focus towards security and domestic issues. Economic growth is slow, consumption remains material-intensive, and inequalities persist. Population growth varies, being low in industrialized countries and high in developing regions. Environmental concerns receive low priority, leading to degradation in some regions."))               
                } else{
                  if(input$SSP_scenario ==  "SSP 5 - Fossil-fueled development"){
                    HTML(paste0("The MAT target values under the SSP5 - Fossil-fueled development scenario for the period of 2041-2060 is ", 
                                fmt(selected_row$TP5_2050)," °C; the MAP target values under the SSP5 scenario is ",
                                fmt(selected_row$PP5_2050)," (mm). In the SSP5 scenarios, this world places increasing faith in markets, innovation, and technological progress drives rapid economic development. Global markets become highly integrated, with strong investments in health, education, and social capital. However, this is coupled with intensive use of fossil fuels and resources, leading to economic growth but also potential environmental challenges. Geo-engineering may be used to manage these challenges if necessary."))               }
                }
              }           
              
            }
        }
      })
      
      updateSliderInput(session, inputId = "pH",
                        value = ifelse(is.null(event$id),
                                       input$pH,NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(pH)))
      
      ######2.2. Nitrogen CLE, MAT,MAP Scenarios value####
      #extract nitrogen scenarios and mat map scenarios value from NUT shapefile
      
      ######2.3. Updating select region text hints after click the map####
      region_remarkcode<-reactive({#region text hints under map
        if(is.null(event$id)){
          "Select a highlighted region on the map."
        }else{
          HTML(paste0("The selected region is ", 
                      NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(NUTS_NAME), 
                      " (",NUTS_env_shiny%>%as_tibble()%>%subset(NUTS_ID == event$id)%>%pull(NUTS_ID),
                      "). The current climatic conditions (MAP, MAT, N deposition) for this region have automatically been setup when you click on the map." ))
        }
      })
      
      output$Select_region_top<-renderUI({region_remarkcode()
      }) 
      
      region_remarkcode2 <- reactive({
        if (is.null(event$id)) {
          # Return an empty table with proper structure
          tibble(
            Category = character(),
            Drivers = character(),
            Value = character()
          )
        } else {
          tibble(
            MAT = selected_map_values()$MAT,
            MAP = selected_map_values()$MAP,
            Ndep = selected_map_values()$Ndep,
            Canopy = input$Canopy_old,
            SCA = SCA_old(), # Reactive or function returning a value
            LQ = LQ_old(),   # Reactive or function returning a value
            pH = input$pH,
            CN = input$CN
          ) %>%
            dplyr::mutate(across(everything(), as.character)) %>%
            pivot_longer(
              cols = c("MAT", "MAP", "Ndep", "Canopy", "SCA", "LQ", "pH", "CN"),
              names_to = "Drivers",
              values_to = "Value"
            ) %>%
            dplyr::mutate(
              Category = case_when(
                Drivers %in% c("MAT", "MAP", "Ndep") ~ "Regional-scale drivers",
                Drivers %in% c("Canopy", "SCA", "LQ", "pH", "CN") ~ "Site-specific drivers"
              ),
              Drivers = case_when(
                Drivers == "MAT" ~ "Mean annual temperature(°C)",
                Drivers == "MAP" ~ "Mean annual precipitation(mm)",
                Drivers == "Ndep" ~ "Nitrogen deposition(N kg/ha/y)",
                Drivers == "Canopy" ~ "Canopy cover(%)",
                Drivers == "SCA" ~ "Shade-casting ability",
                Drivers == "LQ" ~ "Litter quality",
                Drivers == "CN" ~ "Total Carbon-to-Nitrogen ratio",
                TRUE ~ Drivers
              )
            ) %>%
            dplyr::select(Category, Drivers, Value)
        }
      })
      
      output$Select_region <- renderDT({
        table_data <- region_remarkcode2()
        
        # Check if the table is empty and handle appropriately
        if (nrow(table_data) == 0) {
          # Render a message if no data is available
          datatable(data.frame(Message = "Select a region on the map and set up parameters on the left panel."),
                    options = list(dom = 't')) 
        } else {
          # Render the table with data
          datatable(
            table_data,
            options = list(
              pageLength = 8, 
              dom = 't'       
            ),
            rownames = FALSE 
          ) %>% 
            formatRound(columns = c("Value"), digits = 2) 
        }
      })
    })
    
    output$Select_scenarios<-renderUI({
      HTML(paste0("Forecasting under future Nitrogen (", 
                  input$Ndep_scenario, 
                  "), MAT and MAP (",input$SSP_scenario,
                  "), and dominant tree species (", input$dominant_tree_new, 
                  ") scenarios. The current state refers to the initial understorey properties you selected. Predicting with 95% confidence interval." ))
    })
    
    start_year = 2024 #scenarios in 2030 
    end_year = 2050 #scenarios in 2050 
    start_year_diff = 0
    end_year_diff =end_year-start_year
    
    #define input dataframe.All changes in environmental variables are linearly, so slope (delta) and intercepts of these linear relationships are calculated
    
    inputs_shiny <- reactive({
      
      # Canopy scenarios with predefined tree cover proportions
      canopy_new <- tibble(
        Fisher_tree_cover_new = c(0.25, 0.75, 1),
        canopy_scenarios = c("open", "intermediate", "closed")
      )
      
      # Generate a sequence of years for interpolation
      p <- tibble(
        Survey_Year_seq = seq(start_year_diff, end_year_diff, by = 1),  # Sequence of years (differences)
        Survey_Year = seq(start_year, end_year, by = 1),  # Actual survey years (real calendar years)
        
        # Old values fetched from reactive inputs or predefined functions
        MAT_old = selected_map_values()$MAT,
        MAP_old = selected_map_values()$MAP,
        N_rate_old = selected_map_values()$Ndep,
        Fisher_tree_cover_old = input$Canopy_old / 100,  # Convert canopy percentage to proportion
        SCA_old = SCA_old(),  # Structural complexity (old)
        SCA_new = SCA_new(),  # Structural complexity (new)
        LQ_old = LQ_old(),  # Landscape quality (old)
        LQ_new = LQ_new(),  # Landscape quality (new)
        pH_H2O = input$pH,  # Soil pH
        CN_ratio = input$CN,  # Carbon-nitrogen ratio
        Understorey_Richness_old = input$Richness_old,  # Understorey species richness (old)
        Fisher_under_cover_old = input$UnderstoreyCover_old / 100,  # Understorey cover proportion (old)
        woody_pro_old = input$Woody_old / 100,  # Woody cover proportion (old)
        specialist_pro_old = input$Specialist_old / 100,  # Specialist species proportion (old)
        Plot_size = value_plot_size(),  # Plot size
        # New/projection values
        N_rate_new = N_rate_new(),
        MAT_new = MAT_new(),
        MAP_new = MAP_new()
      ) %>%
        as_tibble()
      p2<-p%>%
        merge(canopy_new)%>%
        dplyr::mutate(
          delta_N_rate = ((N_rate_new - N_rate_old) / end_year_diff) * Survey_Year_seq,
          delta_MAT = ((MAT_new - MAT_old) / end_year_diff) * Survey_Year_seq,
          delta_MAP = ((MAP_new - MAP_old) / end_year_diff) * Survey_Year_seq,
          delta_tree_cover = ((Fisher_tree_cover_new - Fisher_tree_cover_old) / end_year_diff) * Survey_Year_seq,
          delta_SCA = ((SCA_new - SCA_old) / end_year_diff) * Survey_Year_seq,
          delta_LQ = ((LQ_new - LQ_old) / end_year_diff) * Survey_Year_seq
        ) %>%
        
        # Filter to only include selected survey years (specific years for analysis)
        dplyr::filter(Survey_Year %in% c(2024, 2030, 2035, 2040, 2045, 2050))
      
      return(p2)
    })
    
    observe(print(inputs_shiny()))
    
    condition_table <- reactive({
      tibble(
        MAT = selected_map_values()$MAT,
        MAP = selected_map_values()$MAP,
        Ndep = selected_map_values()$Ndep,
        Canopy = input$Canopy_old,
        SCA = SCA_old(), # Reactive or function returning a value
        LQ = LQ_old(),   # Reactive or function returning a value
        pH = input$pH,
        CN = input$CN
      ) %>%
        pivot_longer(
          cols = c("MAT", "MAP", "Ndep", "Canopy", "SCA", "LQ", "pH", "CN"),
          names_to = "Drivers",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Category = case_when(
            Drivers %in% c("MAT", "MAP", "Ndep") ~ "Regional-scale drivers",
            Drivers %in% c("Canopy", "SCA", "LQ", "pH", "CN") ~ "Site-specific drivers"
          ),
          Drivers = case_when(
            Drivers == "MAT" ~ "Mean annual temperature(°C)",
            Drivers == "MAP" ~ "Mean annual precipitation(mm)",
            Drivers == "Ndep" ~ "Nitrogen deposition(N kg/ha/y)",
            Drivers == "Canopy" ~ "Canopy cover(%)",
            Drivers == "SCA" ~ "Shade-casting ability",
            Drivers == "LQ" ~ "Litter quality",
            Drivers == "CN" ~ "Total Carbon-to-Nitrogen ratio",
            TRUE ~ Drivers
          )
        ) %>%
        dplyr::select(Category, Drivers, Value)
    })
    
    observe(print(condition_table()))
    
    scenario_table <- reactive({
      req( input$dominant_tree_new, input$SSP_scenario, input$Ndep_scenario)
      tibble(
        #Canopy = input$Canopy_new,
        Dominant =input$dominant_tree_new,
        SSP =input$SSP_scenario,
        Nscenario=input$Ndep_scenario
      ) %>%
        pivot_longer(cols = everything(), names_to = "Scenarios", values_to = "Value") %>%
        # Categorize Scenarios
        dplyr::mutate(
          Scenarios = case_when(
            Scenarios %in% c("Nscenario") ~ "Nitrogen scenarios",
            Scenarios %in% c("SSP") ~ "Climate scenarios",
            #Scenarios %in% c("Canopy")~"Canopy Closure",
            Scenarios %in% c("Dominant") ~ "Dominant tree species",
            TRUE ~ NA_character_  # Handle any unexpected scenarios
          )
        ) %>%
        # Select final columns
        dplyr::select(Scenarios, Value)
    })
    
    observe(print(scenario_table()))
    
    pred_shiny <- eventReactive(input$Simulate,{
      # prepare JSON data
      json_data <- toJSON(list(data = inputs_shiny()), auto_unbox = TRUE)
      
      # print JSON data
      print("Request JSON:")
      #print(json_data)
      
      # send POST request
      response <- POST(
        url="http://shinymodel.ugent.be",
        body = json_data,
        encode = "json",
        add_headers("Content-Type" = "application/json")
      )
      
      # check response status
      if (status_code(response) != 200) {
        stop("Failed to get a valid response from the API.")
      }
      
      # get results
      response_content <- content(response, as = "text", encoding = "UTF-8")
      parsed_response <- fromJSON(response_content)
      # 
      parsed_response2<-parsed_response%>%
        dplyr::select(Survey_Year,canopy_scenarios,Fisher_under_cover_old,cover_fit,specialist_pro_old,
                      specialist_fit,woody_pro_old,woody_fit,Understorey_Richness_old,
                      richness_fit)%>%
        dplyr::mutate(Fisher_under_cover=ifelse(Survey_Year==2024,Fisher_under_cover_old*100,cover_fit*100),
                      min_cover=Fisher_under_cover-1.96*0.1411*100, #cover residual standard deviation=0.1411
                      max_cover=Fisher_under_cover+1.96*0.1411*100,
                      specialist_pro=ifelse(Survey_Year==2024,specialist_pro_old*100,specialist_fit*100),
                      min_specialist=specialist_pro-1.96*0.1072*100,
                      max_specialist=specialist_pro+1.96*0.1072*100,
                      woody_pro=ifelse(Survey_Year==2024,woody_pro_old*100,woody_fit*100),
                      min_woody=woody_pro-1.96*0.0605*100,
                      max_woody=woody_pro+1.96*0.0605*100,
                      Understorey_Richness=ifelse(Survey_Year==2024,Understorey_Richness_old,richness_fit),
                      min_richness=Understorey_Richness-1.96*7.20,
                      max_richness=Understorey_Richness+1.96*7.20)%>%
        as_tibble()
      
      return(parsed_response2)
    })
    
    #observe(pred_shiny())
    
    
    plot_fx <- function(data, y_col, y_min, y_max, y_lab, color_col, title_text) {
      
      # Calculate y-axis range (based on y_min and y_max columns)
      y_min_value <- min(data[[y_min]], na.rm = TRUE)  # Minimum value of y_min
      y_max_value <- max(data[[y_max]], na.rm = TRUE)  # Maximum value of y_max
      y_range_padding <- (y_max_value - y_min_value) * 0.1  # Add 10% padding for better visualization
      
      # Create the plot
      p <- ggplot() +
        
        # Plot data points for years other than 2024
        geom_point(
          data = data %>% filter(Survey_Year != 2024),
          aes(x = Survey_Year, y = !!sym(y_col), color = !!sym(color_col),shape=!!sym(color_col)),
          size = 4, alpha = 0.8,
          position = position_dodge(width =  0.3)
        ) +
        
        # Add connecting lines between points
        geom_line(
          data = data %>% filter(Survey_Year != 2024),
          aes(x = Survey_Year, y = !!sym(y_col), color = !!sym(color_col), group = !!sym(color_col)),
          linewidth = 1
        ) +
        
        # Add ribbons to show the range between y_min and y_max
        geom_ribbon(
          data = data %>% filter(Survey_Year != 2024),
          aes(x = Survey_Year, ymin = !!sym(y_min), ymax = !!sym(y_max), fill = !!sym(color_col)),
          alpha = 0.1
        ) +
        
        # Highlight the year 2024
        geom_point(
          data = data %>% filter(Survey_Year == 2024),
          aes(x = Survey_Year, y = !!sym(y_col)),
          color = "#868686FF", shape = 17, size = 4
        ) +
        
        geom_text_repel(
          data = data %>% filter(Survey_Year == 2024 & !!sym(color_col) == "closed"),
          aes(x = Survey_Year, y = !!sym(y_col), label = "Current state"),
          size = 4, color = "black", nudge_x=2,nudge_y =  2, inherit.aes = FALSE
        ) +
        # Define x-axis breaks
        scale_x_continuous(breaks = c(2024, 2030, 2035, 2040, 2045, 2050)) +
        
        # Define y-axis limits with padding
        scale_y_continuous(limits = c(y_min_value - y_range_padding, y_max_value + y_range_padding)) +
        
        # Customize color palette
        scale_color_manual(values = c("closed" = "#0073C2FF", "open" = "#EFC000FF", "intermediate" = "#CD534CFF")) +
        scale_fill_manual(values = c("closed" = "#0073C2FF", "open" = "#EFC000FF", "intermediate" = "#CD534CFF")) +    
        # Apply a clean theme
        theme_bw(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 330, hjust = 0, vjust = 0.5))+
        theme(legend.position = "bottom",
              legend.background = element_blank())  +
        
        # Add title and axis labels
        ggtitle(title_text) +
        labs(y = y_lab, x = "Year",color = "Canopy Scenarios", shape = "Canopy Scenarios")+
        guides(fill = "none",color = guide_legend(order = 1), shape = guide_legend(order = 1))   # 合并 color 和 fill 为一个图例
      # Return the plot object
      return(p)
    }
    
    
    richnessplot<-eventReactive(input$Simulate,{plot_fx(data=pred_shiny(),y_col="Understorey_Richness",
                                                        y_min="min_richness", y_max="max_richness",color_col="canopy_scenarios",y_lab = "Number of Species(%)",
                                                        title_text ="Species Richness")})
    
    output$richness_new<-renderPlot({richnessplot()})
    
    coverplot<-eventReactive(input$Simulate,{plot_fx(data=pred_shiny(),y_col="Fisher_under_cover",
                                                     y_min="min_cover", y_max="max_cover",color_col="canopy_scenarios", y_lab = "Total Vegetation Cover(%)",
                                                     title_text ="Understorey Total Vegetation Cover")})
    output$cover_new<-renderPlot({coverplot()})
    
    woodyplot<-eventReactive(input$Simulate,{plot_fx(data=pred_shiny(),y_col="woody_pro",
                                                     y_min="min_woody", y_max="max_woody",color_col="canopy_scenarios",y_lab = "Proportion of Woody Species(%)",
                                                     title_text ="Proportion of Woody Species")
    })
    
    output$woody_new<-renderPlot({woodyplot()})
    
    specialistplot<-eventReactive(input$Simulate,{plot_fx(data=pred_shiny(),y_col="specialist_pro",
                                                          y_min="min_specialist", y_max="max_specialist",color_col="canopy_scenarios",
                                                          y_lab="Proportion of forest specialists(%)",
                                                          title_text ="Proportion of Forest Specialists")})
    
    output$specialist_new<-renderPlot({specialistplot()})
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste("UnderScore_GBR_Trajectory_Report", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Copy the RMarkdown file to a temporary directory before processing it
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list()
        rmarkdown::render(input = "report.Rmd", 
                          output_file = file,
                          params = list(
                            conditiontable= condition_table(), #condition table
                            scenariotable=scenario_table(),
                            richnessplot=richnessplot(),
                            coverplot=coverplot(),
                            woodyplot=woodyplot(),
                            specialistplot=specialistplot()
                          ),
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}

ui<-navbarPage(
  #theme = bs_theme(present="minty"),
  "UnderSCORE：Understorey Decision Support Tool",
  tabPanel("Home",
           fluidPage(
             tags$head(
               tags$style(HTML("
          .centered-content {
            max-width: 950px;
            margin: 0 auto;
            padding: 20px;
          }
        "))),
             div(class = "centered-content",
                 h4("To get started, click", strong("\"UnderSCORE-GAM\"")," or ",strong("\"UnderSCORE-GBR\"")," above, or read the instructions below for more information."),
                 br(),
                 p("Forest understorey communities harbour more than 80% of the vascular plant diversity in temperate forests and play a functional role ",a ("(Landuyet, et al. 2017)",href="https://doi.org/10.1111/gcb.14756"),". To safeguard the diversity and functioning of temperate forests understoreys, a ", a(strong("Decision Support System (DSS) "),href="https://en.wikipedia.org/wiki/Decision_support_system"),"can help decision makers to develop mitigation strategies for forest management under global change."),                           
                 p("This understorey decision support tool ",a(strong("(UnderScore)"),href="https://pastforward.ugent.be/underSCORE.html")," was constructed by two modules, including ",strong("UnderSCORE-GAM")," for regional scales and ",strong("UnderSCORE-GBR")," for local site scales."),
                 br(),
                 p(strong("UnderSCORE-GAM"), "was developed based on a",a(" Generalized Additive Model (GAM)",href="https://en.wikipedia.org/wiki/Generalized_additive_model")," using vegetation survey data from 1814 European ancient forest plots, which allows forecasting of the regional average trend of forest understorey dynamics under climate change and forest management interventions from the year 2030 until the year 2050 ",
                   a("(Wen et al. 2022).",href = "https://doi.org/10.1016/j.foreco.2022.120465"),"You can provide or choose the numbers in the data entry sectoion of the", strong("UnderSCORE-GAM")," tab following ", strong("TWO STEPS:")),
                 p(strong("1. Select a region:")," click the region of your forest on the highlighted map, which will automatically set regional climate conditions at your forest;"),
                 p(strong("2. Select a scenario:")," you can choose different Nitrogen, climate, and canopy closure scenarios."),
                 br(),
                 p(strong("UnderSCORE-GBR")," was developed based on a machine learning model ",a("(Gradient boosting regression, GBR)",href="https://en.wikipedia.org/wiki/Gradient_boosting")," using field-collected soil data and vegetation survey data from 1363 European temperate forest plots ",
                   a("(Wen et al. 2024),",href = "https://doi.org/10.1016/j.foreco.2024.122091"),
                   " which allows forecasting trajectories of forest understorey at your forest site from the year 2030 until the year 2050 at a plot-level. You can provide or choose the values in the data entry section of the", strong("UnderSCORE-GBR")," tab following ",strong("FOUR STEPS:")),
                 p(strong("1. Select a region"),": click the region of your forest site on the higlighted map, which will automaticly setup regional climate conditions at your sites;"),
                 p(strong("2. Setup site-specific conditions"),"at your forest site: nine parameters need to be chosen or entered, including 1) abiotic variables: soil pH, CN ratio, plot size, 2) overstory variables: canopy cover, Shade casting ability, litter quality, 3) understorey layer variables: species richness, total vegetation cover, the proportion of woody species, and proportion of forest specialists;"),
                 p(strong("3. Select a scenario")," in the future: related to climate, nitrogen deposition, and management scenarios, including MAT and MAP scenario, N deposition scenario, canopy closure, and dominant tree species."),
                 p(strong("4. Enter the 'Confirm Simulation' button to start the simulation.")),
                 p(strong("UnderSCORE-GBR")," also allows you to download a PDF version of the report."),
                 p("Please let us know if you have any feedback or if you encounter an error by creating an", a("issue on GitHub",href="https://github.com/bibibiwen/underscore-dsstool/issues"), "or writing an email", a("(bingbin.wen@ugent.be)",href="bingbin.wen@ugent.be")," to us."),
                 hr(),
                 p(strong("Reference:")),
                 p("Wen, B., Blondeel, H., Landuyt, D., Verheyen, K., A model-based scenario analysis of the impact of forest management and environmental change on the understorey of temperate forests in Europe, Forest Ecology and Management, Volume 522, 2022, 120465, ISSN 0378-1127,",
                   a("https://doi.org/10.1016/j.foreco.2022.120465.", href = "https://doi.org/10.1016/j.foreco.2022.120465")),
                 p("Wen, B., Blondeel, H., Baeten, L., Perring, M. P., Depauw, L., Maes, S. L., De Keersmaeker, L., Van Calster, H., Wulf, M., Naaf, T., Kirby, K., Bernhardt-Römermann, M., Dirnböck, T., Máliš, F., Kopecký, M., Vild, O., Macek, M., Hédl, R., Chudomelová, M., … Landuyt, D. (2024). Predicting trajectories of temperate forest understorey vegetation responses to global change. Forest Ecology and Management, Volume 566, 2024, 122091, ISSN 0378-1127",
                   a("https://doi.org/10.1016/j.foreco.2024.122091.", href = "https://doi.org/10.1016/j.foreco.2024.122091")),
                 p("Landuyt, D., Lombaerde, E. De, Perring, M. P., Hertzog, L. R., Ampoorter, E., Maes, S. L., Frenne, P. De, Ma, S., Proesmans, W., Blondeel, H., Sercu, B. K., Wang, B., Wasof, S., & Verheyen, K. (2019). The functional role of temperate forest understorey vegetation in a changing world. Global Change Biology, 25(11), 3625–3641.",
                   a("https://doi.org/10.1111/gcb.14756",href="https://doi.org/10.1111/gcb.14756")),
                 br(),
                 p(strong("Privacy&Impact: ")),
                 p("We don't collect any personally identifiable data, nor do we use cookies or local browser storage. RStudio collects data in line with their Privacy Policy for the neccesary functioning of their cloud products, including our hosting provider:",a ("shinyapps.io",href="https://www.shinyapps.io/")),
                 br(),                 
                 p(strong("Credits:")),
                 p("Bingbin Wen (creator, coder, author, ",a("bingbin.wen@ugent.be)",href="bingbin.wen@ugent.be")),
                 p("Haben Blondeel (coder, author)"),
                 p("Dries Landuyt (author)"),
                 p("Kris Verheyen (supervisor)"),
                 br(),
                 p("Created November 2024"),
                 br(),
                 tags$style(HTML(".image-container {
                              display: flex; 
                              align-items: center;}
                              .image-container a {
                              margin: 0 10px;}
                              .image-container img {
                              height: 40px;}")),
                 # Div container for the images
                 div(class = "image-container",
                     tags$a(
                       href = "https://github.com/bibibiwen/underscore-dsstool",
                       tags$img(src = "github-mark.svg")
                     ),
                     tags$a(
                       href = "https://www.ugent.be/bw/environment/en/research/fornalab",
                       tags$img(src = "fornalab.svg")
                     )
                 )
             )
           )
  ),
  tabPanel("UnderSCORE-GAM",
           GAM_ui("app1")),
  tabPanel("UnderSCORE-GBR",
           GBR_ui2("app2")))
  

server <- function(input, output, session) {
  GAM_server("app1")
  GBR_server2("app2")
}


# Include the server function here (from the previous code I provided)

shinyApp(ui, server)
