# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

vars<- c("hydrolab_turbidity_ntu", "sentinel_acolite_turbidity_mean_fnu","landsat_acolite_turbidity_mean_fnu","tss")
# Initiate Connection

ui<- fluidPage(
  
  
  titlePanel("Turbidity"),
  
  sidebarPanel(selectInput("xcol", "X Variable", vars),
               selectInput("ycol", "Y Variable", vars),
               actionButton("refresh", "refresh data")),
  
  mainPanel(plotOutput("turbidityPlot"))
)


server<- function(input, output){
  
  dfCleaned<- eventReactive(input$refresh,{
    hydro<- read_sheet("https://docs.google.com/spreadsheets/d/18TUgYYC6s6ZbgMIdV4XYZ72J_nrS0BfzQUxzfqFy2YY/edit#gid=0")
    tss<- read_sheet("https://docs.google.com/spreadsheets/d/1-FOOyM14op9ojekoXiJ7Lx3qxvw4_lSBugtlzLVjdjU/edit#gid=0")
    hydro<- hydro %>%  
      mutate(hydrolab_turbidity_ntu = as.numeric(unlist(hydrolab_turbidity_ntu)),
             sentinel_acolite_turbidity_mean_fnu = as.numeric(unlist(sentinel_acolite_turbidity_mean_fnu)),
             landsat_acolite_turbidity_mean_fnu = as.numeric(unlist(landsat_acolite_turbidity_mean_fnu)))
    tss<- tss %>%
      group_by(date_sample, sites)%>%
      summarise(tss = mean(tss_g_l, na.rm = T))
    hydro %>%
      select(date_sample,
             sites,
             hydrolab_turbidity_ntu,
             sentinel_acolite_turbidity_mean_fnu,
             landsat_acolite_turbidity_mean_fnu ) %>%
      left_join(tss, c("date_sample", "sites")) 
  })

  

  
 
  
  output$turbidityPlot<- renderPlot({
    
    ggplot(dfCleaned(), aes_string(x = input$xcol, y = input$ycol))+
      geom_point(aes(color = sites))
    
    
    
  })
}

shinyApp(ui, server)





