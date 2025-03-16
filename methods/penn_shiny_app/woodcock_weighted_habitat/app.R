#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(future)
library(raster)
library(sf)
library(leaflet)
library(tidyverse)
library(DT)
library(leaflet.esri)
library(shinyBS)
library(shinyLP)
library(shinythemes)


# Define UI
ui <- shinyUI(
    fluidPage(
        
        list(tags$head(HTML('<link rel="icon", href="pgc.png",
                        type="image/png" />'))),
        div(style="padding: 1px 0px; width: '100%'",
            titlePanel(
                title="", windowTitle="W-PAST"
            )
        ),
        navbarPage(title=div(img(src="pgc.png", width="30", height="30"), "W-PAST"),
                   inverse = F, # for diff color view
                   theme = shinytheme("united"),
                   id = "inTabset",
                   
                   tabPanel("Instructions", icon = icon("home"),
                            
                            jumbotron("Woodcock Priority Area Siting Tool", "Identifying priority areas for new woodcock management",
                                      button = FALSE),
                            fluidRow(
                                column(6, panel_div(class_type = "primary", panel_title = "Directions",
                                                    content = HTML("<ul>
                                                    <li>W-PAST uses landscape-scale variables (including landscape configuration, land use/land cover, topography, and soil moisture) to predict where woodcock habitat management is likely to be successful.</li>
                                                    <li>The model makes separate predictions for woodcock during the breeding and migratory seasons. Users can weight these suitability layers based on the relative importance of breeding and migratory habitat to their management strategy.</li>
                                                    <li><b>The model does not account for current fine-scale habitat conditions, and therefore a high ranking in the app does not necessarily suggest that an area is currently occupied by woodcock.</b></li>
                                                    <li><b>Instead, the model highlights areas with a high likelihood of occupancy by woodcock given appropriate management.</b></li>
                                                    <li>The model is most useful at a > 1 kilometer scale (i.e. the width of 4 pixels); inference at a finer scale is not recommended.</li>
                                                                   </ul>"))),
                                column(6, panel_div("success", "User Guides", HTML("For Land Managers: <a href='W-PAST_manual.pdf' target = 'blank' download='W-PAST_manual.pdf'> User Manual </a> <br/> Watch the video tutorial here: <a href='https://youtu.be/U7brDAFVqyU' target='_top'>How to use W-PAST</a>"))), #HTML("<br/> For Researchers: Technical Report (link will be added when complete)") downloadLink("download_manual", "User Manual")
                                column(6, panel_div("info", "Credits", HTML("<i>Version 2.0 (updated 2024-10-11)</i><br/>
                                This app was created by the University of Maine and the Pennsylvania Game Commision using data from the <a href='https://www.citizenscience.gov/catalog/182/#' target='_top'>American Woodcock Singing-ground Survey</a>, Pennsylvania Game Commission surveys, and migratory stopover locations from the <a href='https://www.woodcockmigration.org/' target='_top'>Eastern Woodcock Migration Research Cooperative</a>.<br/><br/> 
                                                                            Need help? Email: <a href='mailto:liamaberigan@gmail.com?Subject=W-PAST%20Help' target='_top'>Liam Berigan</a>"))))
                            ,  # end of fluidRow
                            fluidRow(
                                column(6, div(class = "row", div(class = "col-sm-14 col-md-12", div(class = "thumbnail", div(class = "caption", actionButton('jumpToApp', 'Access the app here', class = "btn btn-primary")))))), #)
                                
                                
                                #### FAVICON TAGS SECTION ####
                                 tags$head(tags$link(rel="shortcut icon", href="pgc.png")),
                                # 
                                # bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                                #         p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                                #         iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
                                # )
                                
                            )),
                   tabPanel("App", icon = icon("cog"),
                            sidebarLayout(
                                sidebarPanel(width = 4, #
                                    tabsetPanel(type = "pills",
                                                selected = "Layer & Weight",
                                                tabPanel("Layer & Weight",
                                                         hr(),
                                                         selectInput(inputId = "mode",
                                                                     label = "Layer view",
                                                                     choices = c("Statewide layer", "Gamelands: Landscape suitability index", "Gamelands: Average pixel value"),
                                                                     selected = "Statewide layer"),
                                                         radioButtons("weight", label = "Migratory & breeding habitat weight", choices = list("Migratory: 100%, Breeding: 0%" = 1, 
                                                                                                                                                 "Migratory: 90%, Breeding: 10%" = 2, 
                                                                                                                                                 "Migratory: 80%, Breeding: 20%" = 3, 
                                                                                                                                                 "Migratory: 70%, Breeding: 30%" = 4,
                                                                                                                                                 "Migratory: 60%, Breeding: 40%" = 5,
                                                                                                                                                 "Migratory: 50%, Breeding: 50%" = 6,
                                                                                                                                                 "Migratory: 40%, Breeding: 60%" = 7,
                                                                                                                                                 "Migratory: 30%, Breeding: 70%" = 8,
                                                                                                                                                 "Migratory: 20%, Breeding: 80%" = 9,
                                                                                                                                                 "Migratory: 10%, Breeding: 90%" = 10,
                                                                                                                                                 "Migratory: 0%, Breeding: 100%" = 11), selected = 6),
                                                         hr(),
                                                         htmlOutput("footer1"),
                                                         br(),
                                                         htmlOutput("footer2"),
                                                         br(),
                                                         htmlOutput("footer3"),
                                                         br(),
                                                         htmlOutput("footer4")
                                                         ),
                                                tabPanel("Gamelands within view",
                                                         hr(),
                                                         dataTableOutput("table"),
                                                         #downloadLink("download", "Download Metadata")
                                                         )
                                    )
                                ),
                                mainPanel(width = 8,
                                    leafletOutput(outputId = "map", height = 800), 
                                    hr()#,
                                    #htmlOutput("footer1"),
                                    #htmlOutput("footer2"),
                                    #htmlOutput("footer3")
                                )
                            )
                            
                            
                   ))
    ))


# Define server logic
server <- function(input, output, session) { 
    print(sessionInfo())
    print(find.package("leaflet.esri"))
    #Clean house
    #saveRDS(0, "render_maps.rds")
    
    #If maps are not pre-rendered, render maps
    #render_maps <- readRDS("render_maps.rds")
    # if(render_maps == 0){
    #     source("prep_app.R", local = TRUE)
    #     print("Maps rendered")
    #     saveRDS(1, "render_maps.rds")
    # } 
    leafmap_m10r0 <- "leafmap_m10r0.rds"
    leafmap_m9r1 <- "leafmap_m9r1.rds"
    leafmap_m8r2 <- "leafmap_m8r2.rds"
    leafmap_m7r3 <- "leafmap_m7r3.rds"
    leafmap_m6r4 <- "leafmap_m6r4.rds"
    leafmap_m5r5 <- "leafmap_m5r5.rds"
    leafmap_m4r6 <- "leafmap_m4r6.rds"
    leafmap_m3r7 <- "leafmap_m3r7.rds"
    leafmap_m2r8 <- "leafmap_m2r8.rds"
    leafmap_m1r9 <- "leafmap_m1r9.rds"
    leafmap_m0r10 <- "leafmap_m0r10.rds"
    
    gamemap_m10r0_mean <- "gamemap_m10r0_mean.rds"
    gamemap_m9r1_mean <- "gamemap_m9r1_mean.rds"
    gamemap_m8r2_mean <- "gamemap_m8r2_mean.rds"
    gamemap_m7r3_mean <- "gamemap_m7r3_mean.rds"
    gamemap_m6r4_mean <- "gamemap_m6r4_mean.rds"
    gamemap_m5r5_mean <- "gamemap_m5r5_mean.rds"
    gamemap_m4r6_mean <- "gamemap_m4r6_mean.rds"
    gamemap_m3r7_mean <- "gamemap_m3r7_mean.rds"
    gamemap_m2r8_mean <- "gamemap_m2r8_mean.rds"
    gamemap_m1r9_mean <- "gamemap_m1r9_mean.rds"
    gamemap_m0r10_mean <- "gamemap_m0r10_mean.rds"
    
    gamemap_m10r0_si <- "gamemap_m10r0_si.rds"
    gamemap_m9r1_si <- "gamemap_m9r1_si.rds"
    gamemap_m8r2_si <- "gamemap_m8r2_si.rds"
    gamemap_m7r3_si <- "gamemap_m7r3_si.rds"
    gamemap_m6r4_si <- "gamemap_m6r4_si.rds"
    gamemap_m5r5_si <- "gamemap_m5r5_si.rds"
    gamemap_m4r6_si <- "gamemap_m4r6_si.rds"
    gamemap_m3r7_si <- "gamemap_m3r7_si.rds"
    gamemap_m2r8_si <- "gamemap_m2r8_si.rds"
    gamemap_m1r9_si <- "gamemap_m1r9_si.rds"
    gamemap_m0r10_si <- "gamemap_m0r10_si.rds"
    
    gamelands <- readRDS("gamelands.rds")
    
    leafmaps <- list("leafmap_m10r0", "leafmap_m9r1", "leafmap_m8r2", "leafmap_m7r3", "leafmap_m6r4", "leafmap_m5r5", "leafmap_m4r6", "leafmap_m3r7", "leafmap_m2r8", "leafmap_m1r9", "leafmap_m0r10")
    
    gamemaps_mean <- list("gamemap_m10r0_mean", "gamemap_m9r1_mean", "gamemap_m8r2_mean", "gamemap_m7r3_mean", "gamemap_m6r4_mean", "gamemap_m5r5_mean", 
                          "gamemap_m4r6_mean", "gamemap_m3r7_mean", "gamemap_m2r8_mean", "gamemap_m1r9_mean", "gamemap_m0r10_mean")
    
    gamemaps_si <- list("gamemap_m10r0_si", "gamemap_m9r1_si", "gamemap_m8r2_si", "gamemap_m7r3_si", "gamemap_m6r4_si", "gamemap_m5r5_si", 
                        "gamemap_m4r6_si", "gamemap_m3r7_si", "gamemap_m2r8_si", "gamemap_m1r9_si", "gamemap_m0r10_si")
    
    gamelands_colnames_mean <- c("m10r0_mean", "m9r1_mean", "m8r2_mean", "m7r3_mean", "m6r4_mean", "m5r5_mean", "m4r6_mean", "m3r7_mean", "m2r8_mean", "m1r9_mean", "m0r10_mean")
    gamelands_colnames_si <- c("m10r0_si", "m9r1_si", "m8r2_si", "m7r3_si", "m6r4_si", "m5r5_si", "m4r6_si", "m3r7_si", "m2r8_si", "m1r9_si", "m0r10_si")
    
    ##Reactive inputs: when the users change the inputs, this function will register it and return the correct prerendered leaflet map
    
    #This reactive input flags whenever the mode or weight changes, and triggers updates in the rest of the map reactives
    weighted_leafmap_reactive <- reactive({
        if(input$mode == "Statewide layer"){
            readRDS(get(leafmaps[[as.numeric(input$weight)]]))
            
        } else if(input$mode == "Gamelands: Landscape suitability index"){
            readRDS(get(gamemaps_si[[as.numeric(input$weight)]]))
            
        } else if(input$mode == "Gamelands: Average pixel value"){
            readRDS(get(gamemaps_mean[[as.numeric(input$weight)]]))
        }
    })
    
    #reactive lat long and zoom, that only update when weighting changes
    lat_reactive <- eventReactive(weighted_leafmap_reactive(), input$map_center[[2]]) #input$weight
    long_reactive <- eventReactive(weighted_leafmap_reactive(), input$map_center[[1]])
    zoom_reactive <- eventReactive(weighted_leafmap_reactive(), input$map_zoom)
    
    #reactive gameland table: when extent changes, recalculate the table
    game_table_reactive <- reactive({
        if(is.null(input$map_bounds$west)){
            print("recalcing table null")
            gamelands %>%
                transmute(Gameland = name, `Landscape Suitability Index` = round(unlist(pull(st_drop_geometry(gamelands), gamelands_colnames_si[as.numeric(input$weight)]))), `Average pixel value <br/> (0 - 1)` = signif(unlist(pull(st_drop_geometry(gamelands), gamelands_colnames_mean[as.numeric(input$weight)])), 3), `Gameland Acreage` = round(gamelands$area_ac)) %>%
                #transmute(Gameland = name, `Total estimated habitat <br/> (ha)` = signif(unlist(pull(gamelands, gamelands_colnames_total[as.numeric(input$weight)])), 3), `Average pixel value <br/> (0 - 1)` = signif(unlist(pull(gamelands, gamelands_colnames_mean[as.numeric(input$weight)])), 3)) %>%
                return()
        } else{
            print("recalcing table success")
            #try({st_crop(gamelands, y = c(xmin = input$map_bounds$west, ymin = input$map_bounds$south, xmax = input$map_bounds$east, ymax = input$map_bounds$north)) -> gamelands_smol}) ->
            #try_results
            #if (inherits(try_results, "try-error")) function(){
            #    gamelands -> gamelands_smol
            #} 
            gamelands_smol <- tryCatch(st_crop(st_make_valid(gamelands), y = c(xmin = input$map_bounds$west, ymin = input$map_bounds$south, xmax = input$map_bounds$east, ymax = input$map_bounds$north)), 
                                       error = function(i){gamelands})
            
            if(nrow(gamelands_smol)==0){
                tibble(Gameland = "None", `Landscape Suitability Index` = NA, `Average pixel value <br/> (0 - 1)` = NA, `Gameland Acreage` = NA) %>%
                    return()
            } else{
                gamelands_smol %>%
                    st_drop_geometry() %>%
                    transmute(Gameland = name,  `Landscape Suitability Index` = round(unlist(pull(gamelands_smol, gamelands_colnames_si[as.numeric(input$weight)]))), `Average pixel value <br/> (0 - 1)` = signif(unlist(pull(gamelands_smol, gamelands_colnames_mean[as.numeric(input$weight)])), 3), `Gameland Acreage` = round(gamelands_smol$area_ac)) %>%
                    #transmute(Gameland = name, `Total estimated habitat <br/> (ha)` = signif(unlist(pull(gamelands_smol, gamelands_colnames_total[as.numeric(input$weight)])), 3), `Average pixel value <br/> (0 - 1)` = signif(unlist(pull(gamelands_smol, gamelands_colnames_mean[as.numeric(input$weight)])), 3)) %>%
                    return()
            }
            
        }
    })
    
    #Observe: when the weighting or map type change, update the leaflet map
    
    observe({
        output$map <- renderLeaflet({
            rendering_leaf <- weighted_leafmap_reactive()
            lat_reacted <- lat_reactive()
            long_reacted <- long_reactive()
            zoom_reacted <- zoom_reactive()
            
            print(lat_reacted)
            print(long_reacted)
            print(zoom_reacted)
            
            if(is.null(lat_reacted)){
                rendering_leaf %>%
                    setView(-77.75574, 41.12902, zoom = 7) ->
                    rendering_leaf
            } else {
                rendering_leaf %>%
                    setView(long_reacted, lat_reacted, zoom = zoom_reacted) ->
                    rendering_leaf
            }
            rendering_leaf
        })
    })
    
    headerCallback <- c(
        "function(thead, data, start, end, display){",
        "  var tooltips = ['Gameland names','Average pixel value multiplied by the acreage of the gameland','Average value of all pixels in gameland, with 0 having the least potential and 1 having the most potential for management.'];",
        "  for(var i=0; i<3; i++){",
        "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
        "  }",
        "}"
    )
    
    #Observe: when the extent changes, update the table
    observe({
        game_table_reacted <- game_table_reactive()
        
        if(input$mode == "Gamelands: Average pixel value"){
            game_table_reacted %>%
                datatable(rownames = FALSE,
                          filter = "top",
                          options = list(pageLength = 309, 
                                         dom = 'ti', 
                                         scrollY = 400,
                                         columnDefs = list(list(className = 'dt-center', targets = c(0, 1, 2, 3))),
                                         order = list(list(0, 'asc')),
                                         headerCallback = JS(headerCallback)), 
                          caption = 'Pennsylvania State Gamelands within map extent',
                          selection = "single",
                          escape = FALSE) %>%
                renderDataTable() ->
                output$table
        } else {
            game_table_reacted %>%
                datatable(rownames = FALSE, 
                          filter = "top",
                          options = list(pageLength = 309, 
                                         dom = 'ti', 
                                         scrollY = 400,
                                         columnDefs = list(list(className = 'dt-center', targets = c(0, 1, 2, 3))),
                                         order = list(list(0, 'asc')),
                                         headerCallback = JS(headerCallback)), 
                          caption = 'Pennsylvania State Gamelands within map extent',
                          selection = "single", 
                          escape = FALSE) %>%
                renderDataTable() ->
                output$table
        }
    })
    
    #Observe: when the user selects a row of the table, highlight the corresponding gameland
    observeEvent(input$table_rows_selected,{
        game_table_reacted <- game_table_reactive()
        
        game_table_reacted[input$table_rows_selected, 1] %>%
            print()
        
        leafletProxy("map", data = filter(gamelands, name ==  game_table_reacted[input$table_rows_selected, 1])) %>%
            clearGroup("selected") %>%
            addPolygons(color = "#782F6A", fillColor = "#782F6A", group = "selected", opacity = 0.8, options = pathOptions(clickable = FALSE)) ->
            leaflet_proxied
        leaflet_proxied
        
        
    })
    
    #Observe: Prompt the user to the app interface when they hit the button
    observeEvent(input$jumpToApp, {
        updateTabsetPanel(session, "inTabset",
                          selected = "App")
    })
    
    #Provide metadata for the user to download
    output$download_manual <- downloadHandler(
        filename = "W-PAST_manual.pdf",
        content = function(file) {
            file.copy("W-PAST_manual.pdf", file)
        },
        contentType = "application/pdf"
    )
    output$footer1 <- renderText("<sup>1</sup> Color ramps are drawn on a percentile scale from 0 to 100 (e.g. a pixel with a percentile value of 56 is more suitable than 56% of the other pixels on the map).")
    output$footer2 <- renderText("<sup>2</sup> Landscape suitability index is calculated by multiplying the average pixel value by the acreage of the gameland. Average pixel value represents the average value of all pixels in the gameland, with 0 having the least potential and 1 having the most potential for management.")
    output$footer3 <- renderText("<sup>3</sup> Statewide predictive layers are intended for use at a > 1 kilometer scale (i.e. the width of 4 pixels); inference at a finer scale is not recommended.")
    output$footer4 <- renderText("<sup>4</sup> The 'High suitability' and 'Moderate suitability' metrics describe the proportion of a gameland that is highly or moderatly well-suited for targeted woodcock management. Cells are classified as high potential when they fall within the 67th-100th percentiles of the model predictions, and moderate potential when they fall within the 34th-66th percentiles. This information can be accessed by clicking on individual gamelands.")
}

# Run the application 
shinyApp(ui = ui, server = server) #options = c(display.mode="showcase")

# function(file){
#     #write.table(data.frame("Habitat values are displayed along a "), file = file, sep = "\t",
#     #            row.names = TRUE, col.names = NA)
#     write(x = "Metadata for the Pennsylvania Woodcock Predictive Habitat Model:
#             
# This tool is designed to identify landscapes which are most appropriate for woodcock habitat management.
# High values and rankings on these maps are not necessarily an indicator that areas are currently occupied by woodcock; only that they could given proper early successional habitat management. 
# 
# \nModeling techniques
# This R Shiny application combines breeding and migratory habitat models using user-defined weights, to allow the user to prioritize management for migratory or breeding habitat.
# Both models were developed using a Random Forest machine learning algorithm, which uses preexisting data on woodcock migratory and breeding habitat use to determine what habitat characteristics are likely to support woodcock.
# For the migratory model, woodcock migratory stopover locations were collected by the Eastern Woodcock Migration Research Cooperative (www.woodcockmigration.org) in 2017-2021. These locations were recorded using GPS transmitters, attached to woodcock by the Pennsylvania Game Commission and other cooperating agencies and organizations.
# For the breeding model, woodcock habitat use was determined through display surveys conducted during the American Woodcock Singing-ground Survey (migbirdapps.fws.gov/mbdc/databases/awsgs/aboutwcsgs.htm) and similiar surveys conducted by the Pennsylvania Game Commission from 2016-2020.
# Habitat characteristics used in the model include multi-scale landscape composition & configuration, as well as elevation, slope, level 3 EPA ecoregions, forest cover, forest successional class, soil drainage, and topographic wetness index.
# 
# Display settings
# Statewide layer- Full predictive layer for the state of Pennsylvania at 270m resolution. Values are stretched using a percentile scale (e.g. a pixel with a percentile value of 56 contains more habitat than 56% of the other pixels on the map).
# Gamelands: Landscape suitability index- The average value of pixels within each Pennsylvania Game Commission State Gameland multiplied by the acreage of that gameland.
# Gamelands: Average pixel value- The average value of pixels within each Pennsylvania Game Commission State Gameland, displayed on a percentile scale. The highest possible pixel value is 1 (contains the maximum possible amount of both migratory and breeding habitat) and the lowest possible pixel value is 0.
#     
# Questions? Contact Liam Berigan (liam.berigan@maine.edu)", file = file) }