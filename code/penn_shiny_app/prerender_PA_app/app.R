#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(raster)
library(sf)
library(leaflet)
library(tidyverse)
library(DT)


# Define UI
ui <- fluidPage(
    titlePanel("Pennsylvania Woodcock Predictive Habitat Model"), 
    sidebarLayout(
        sidebarPanel(
            downloadLink("leafmap_m10r0", "Download"),
            downloadLink("leafmap_m9r1", "Download"),
            downloadLink("leafmap_m8r2", "Download"),
            downloadLink("leafmap_m7r3", "Download"),
            downloadLink("leafmap_m6r4", "Download"),
            downloadLink("leafmap_m5r5", "Download"),
            downloadLink("leafmap_m4r6", "Download"),
            downloadLink("leafmap_m3r7", "Download"),
            downloadLink("leafmap_m2r8", "Download"),
            downloadLink("leafmap_m1r9", "Download"),
            downloadLink("leafmap_m0r10", "Download"),
            
            downloadLink("gamemap_m10r0_mean", "Download"),
            downloadLink("gamemap_m9r1_mean", "Download"),
            downloadLink("gamemap_m8r2_mean", "Download"),
            downloadLink("gamemap_m7r3_mean", "Download"),
            downloadLink("gamemap_m6r4_mean", "Download"),
            downloadLink("gamemap_m5r5_mean", "Download"),
            downloadLink("gamemap_m4r6_mean", "Download"),
            downloadLink("gamemap_m3r7_mean", "Download"),
            downloadLink("gamemap_m2r8_mean", "Download"),
            downloadLink("gamemap_m1r9_mean", "Download"),
            downloadLink("gamemap_m0r10_mean", "Download"),
            
            downloadLink("gamemap_m10r0_si", "Download"),
            downloadLink("gamemap_m9r1_si", "Download"),
            downloadLink("gamemap_m8r2_si", "Download"),
            downloadLink("gamemap_m7r3_si", "Download"),
            downloadLink("gamemap_m6r4_si", "Download"),
            downloadLink("gamemap_m5r5_si", "Download"),
            downloadLink("gamemap_m4r6_si", "Download"),
            downloadLink("gamemap_m3r7_si", "Download"),
            downloadLink("gamemap_m2r8_si", "Download"),
            downloadLink("gamemap_m1r9_si", "Download"),
            downloadLink("gamemap_m0r10_si", "Download"),
        ),
        mainPanel()
    ))

# Define server logic
server <- function(input, output) { 
    print(sessionInfo())
    
    source("prep_app.R", local = TRUE)
    print("Maps rendered")

    gamelands <- readRDS("gamelands.rds")

    leafmap_m10r0 <- "leafmap_m10r0.rds" %>% readRDS()
    leafmap_m9r1 <- "leafmap_m9r1.rds" %>% readRDS()
    leafmap_m8r2 <- "leafmap_m8r2.rds" %>% readRDS()
    leafmap_m7r3 <- "leafmap_m7r3.rds" %>% readRDS()
    leafmap_m6r4 <- "leafmap_m6r4.rds" %>% readRDS()
    leafmap_m5r5 <- "leafmap_m5r5.rds" %>% readRDS()
    leafmap_m4r6 <- "leafmap_m4r6.rds" %>% readRDS()
    leafmap_m3r7 <- "leafmap_m3r7.rds" %>% readRDS()
    leafmap_m2r8 <- "leafmap_m2r8.rds" %>% readRDS()
    leafmap_m1r9 <- "leafmap_m1r9.rds" %>% readRDS()
    leafmap_m0r10 <- "leafmap_m0r10.rds" %>% readRDS()
    
    gamemap_m10r0_mean <- "gamemap_m10r0_mean.rds" %>% readRDS()
    gamemap_m9r1_mean <- "gamemap_m9r1_mean.rds" %>% readRDS()
    gamemap_m8r2_mean <- "gamemap_m8r2_mean.rds" %>% readRDS()
    gamemap_m7r3_mean <- "gamemap_m7r3_mean.rds" %>% readRDS()
    gamemap_m6r4_mean <- "gamemap_m6r4_mean.rds" %>% readRDS()
    gamemap_m5r5_mean <- "gamemap_m5r5_mean.rds" %>% readRDS()
    gamemap_m4r6_mean <- "gamemap_m4r6_mean.rds" %>% readRDS()
    gamemap_m3r7_mean <- "gamemap_m3r7_mean.rds" %>% readRDS()
    gamemap_m2r8_mean <- "gamemap_m2r8_mean.rds" %>% readRDS()
    gamemap_m1r9_mean <- "gamemap_m1r9_mean.rds" %>% readRDS()
    gamemap_m0r10_mean <- "gamemap_m0r10_mean.rds" %>% readRDS()
    
    gamemap_m10r0_si <- "gamemap_m10r0_si.rds" %>% readRDS()
    gamemap_m9r1_si <- "gamemap_m9r1_si.rds" %>% readRDS()
    gamemap_m8r2_si <- "gamemap_m8r2_si.rds" %>% readRDS()
    gamemap_m7r3_si <- "gamemap_m7r3_si.rds" %>% readRDS()
    gamemap_m6r4_si <- "gamemap_m6r4_si.rds" %>% readRDS()
    gamemap_m5r5_si <- "gamemap_m5r5_si.rds" %>% readRDS()
    gamemap_m4r6_si <- "gamemap_m4r6_si.rds" %>% readRDS()
    gamemap_m3r7_si <- "gamemap_m3r7_si.rds" %>% readRDS()
    gamemap_m2r8_si <- "gamemap_m2r8_si.rds" %>% readRDS()
    gamemap_m1r9_si <- "gamemap_m1r9_si.rds" %>% readRDS()
    gamemap_m0r10_si <- "gamemap_m0r10_si.rds" %>% readRDS()
    
    
    #Provide metadata for the user to download
    output$leafmap_m10r0 <- downloadHandler(
        filename = "leafmap_m10r0.rds",
        content = function(file){
            saveRDS(leafmap_m10r0, file = file)
        }
    )
    
    output$leafmap_m9r1 <- downloadHandler(
        filename = "leafmap_m9r1.rds",
        content = function(file){
            saveRDS(leafmap_m9r1, file = file)
        }
    )
    
    output$leafmap_m8r2 <- downloadHandler(
        filename = "leafmap_m8r2.rds",
        content = function(file){
            saveRDS(leafmap_m8r2, file = file)
        }
    )
    
    output$leafmap_m7r3 <- downloadHandler(
        filename = "leafmap_m7r3.rds",
        content = function(file){
            saveRDS(leafmap_m7r3, file = file)
        }
    )
    
    output$leafmap_m6r4 <- downloadHandler(
        filename = "leafmap_m6r4.rds",
        content = function(file){
            saveRDS(leafmap_m6r4, file = file)
        }
    )
    
    output$leafmap_m5r5 <- downloadHandler(
        filename = "leafmap_m5r5.rds",
        content = function(file){
            saveRDS(leafmap_m5r5, file = file)
        }
    )
    
    output$leafmap_m4r6 <- downloadHandler(
        filename = "leafmap_m4r6.rds",
        content = function(file){
            saveRDS(leafmap_m4r6, file = file)
        }
    )
    
    output$leafmap_m3r7 <- downloadHandler(
        filename = "leafmap_m3r7.rds",
        content = function(file){
            saveRDS(leafmap_m3r7, file = file)
        }
    )
    
    output$leafmap_m2r8 <- downloadHandler(
        filename = "leafmap_m2r8.rds",
        content = function(file){
            saveRDS(leafmap_m2r8, file = file)
        }
    )
    
    output$leafmap_m1r9 <- downloadHandler(
        filename = "leafmap_m1r9.rds",
        content = function(file){
            saveRDS(leafmap_m1r9, file = file)
        }
    )
    
    output$leafmap_m0r10 <- downloadHandler(
        filename = "leafmap_m0r10.rds",
        content = function(file){
            saveRDS(leafmap_m0r10, file = file)
        }
    )
    
    
    
    
    output$gamemap_m10r0_mean <- downloadHandler(
        filename = "gamemap_m10r0_mean.rds",
        content = function(file){
            saveRDS(gamemap_m10r0_mean, file = file)
        }
    )
    
    output$gamemap_m9r1_mean <- downloadHandler(
        filename = "gamemap_m9r1_mean.rds",
        content = function(file){
            saveRDS(gamemap_m9r1_mean, file = file)
        }
    )
    
    output$gamemap_m8r2_mean <- downloadHandler(
        filename = "gamemap_m8r2_mean.rds",
        content = function(file){
            saveRDS(gamemap_m8r2_mean, file = file)
        }
    )
    
    output$gamemap_m7r3_mean <- downloadHandler(
        filename = "gamemap_m7r3_mean.rds",
        content = function(file){
            saveRDS(gamemap_m7r3_mean, file = file)
        }
    )
    
    output$gamemap_m6r4_mean <- downloadHandler(
        filename = "gamemap_m6r4_mean.rds",
        content = function(file){
            saveRDS(gamemap_m6r4_mean, file = file)
        }
    )
    
    output$gamemap_m5r5_mean <- downloadHandler(
        filename = "gamemap_m5r5_mean.rds",
        content = function(file){
            saveRDS(gamemap_m5r5_mean, file = file)
        }
    )
    
    output$gamemap_m4r6_mean <- downloadHandler(
        filename = "gamemap_m4r6_mean.rds",
        content = function(file){
            saveRDS(gamemap_m4r6_mean, file = file)
        }
    )
    
    output$gamemap_m3r7_mean <- downloadHandler(
        filename = "gamemap_m3r7_mean.rds",
        content = function(file){
            saveRDS(gamemap_m3r7_mean, file = file)
        }
    )
    
    output$gamemap_m2r8_mean <- downloadHandler(
        filename = "gamemap_m2r8_mean.rds",
        content = function(file){
            saveRDS(gamemap_m2r8_mean, file = file)
        }
    )
    
    output$gamemap_m1r9_mean <- downloadHandler(
        filename = "gamemap_m1r9_mean.rds",
        content = function(file){
            saveRDS(gamemap_m1r9_mean, file = file)
        }
    )
    
    output$gamemap_m0r10_mean <- downloadHandler(
        filename = "gamemap_m0r10_mean.rds",
        content = function(file){
            saveRDS(gamemap_m0r10_mean, file = file)
        }
    )
    
    
    
    
    output$gamemap_m10r0_si <- downloadHandler(
        filename = "gamemap_m10r0_si.rds",
        content = function(file){
            saveRDS(gamemap_m10r0_si, file = file)
        }
    )
    
    output$gamemap_m9r1_si <- downloadHandler(
        filename = "gamemap_m9r1_si.rds",
        content = function(file){
            saveRDS(gamemap_m9r1_si, file = file)
        }
    )
    
    output$gamemap_m8r2_si <- downloadHandler(
        filename = "gamemap_m8r2_si.rds",
        content = function(file){
            saveRDS(gamemap_m8r2_si, file = file)
        }
    )
    
    
    output$gamemap_m7r3_si <- downloadHandler(
        filename = "gamemap_m7r3_si.rds",
        content = function(file){
            saveRDS(gamemap_m7r3_si, file = file)
        }
    )
    
    output$gamemap_m6r4_si <- downloadHandler(
        filename = "gamemap_m6r4_si.rds",
        content = function(file){
            saveRDS(gamemap_m6r4_si, file = file)
        }
    )
    
    output$gamemap_m5r5_si <- downloadHandler(
        filename = "gamemap_m5r5_si.rds",
        content = function(file){
            saveRDS(gamemap_m5r5_si, file = file)
        }
    )
    
    output$gamemap_m4r6_si <- downloadHandler(
        filename = "gamemap_m4r6_si.rds",
        content = function(file){
            saveRDS(gamemap_m4r6_si, file = file)
        }
    )
    
    output$gamemap_m3r7_si <- downloadHandler(
        filename = "gamemap_m3r7_si.rds",
        content = function(file){
            saveRDS(gamemap_m3r7_si, file = file)
        }
    )
    
    output$gamemap_m2r8_si <- downloadHandler(
        filename = "gamemap_m2r8_si.rds",
        content = function(file){
            saveRDS(gamemap_m2r8_si, file = file)
        }
    )
    
    output$gamemap_m1r9_si <- downloadHandler(
        filename = "gamemap_m1r9_si.rds",
        content = function(file){
            saveRDS(gamemap_m1r9_si, file = file)
        }
    )
    
    output$gamemap_m0r10_si <- downloadHandler(
        filename = "gamemap_m0r10_si.rds",
        content = function(file){
            saveRDS(gamemap_m0r10_si, file = file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server) #options = c(display.mode="showcase")
