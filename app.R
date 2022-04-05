# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)








# Initiate Connection
hydro <- read_sheet("https://docs.google.com/spreadsheets/d/18TUgYYC6s6ZbgMIdV4XYZ72J_nrS0BfzQUxzfqFy2YY/edit#gid=0")
tss<- read_sheet("https://docs.google.com/spreadsheets/d/1-FOOyM14op9ojekoXiJ7Lx3qxvw4_lSBugtlzLVjdjU/edit#gid=0")

# Data Cleaning
tss<- tss %>%
  group_by(date_sample, sites)%>%
  summarise(tss = mean(tss_g_l, na.rm = T))
df<- hydro %>%
  select(date_sample,
         sites,
         hydrolab_turbidity_ntu,
         sentinel_acolite_turbidity_mean_fnu,
         landsat_acolite_turbidity_mean_fnu ) %>%
  left_join(tss, c("date_sample", "sites")) %>%
  filter(is.na(tss) == F)

df$hydrolab_turbidity_ntu<- unlist(df$hydrolab_turbidity_ntu)
df$sentinel_acolite_turbidity_mean_fnu<- as.numeric(unlist(df$sentinel_acolite_turbidity_mean_fnu))
df$landsat_acolite_turbidity_mean_fnu<- as.numeric(unlist(df$landsat_acolite_turbidity_mean_fnu))


vars<- names(df)[3:6]
ui<- fluidPage(



  titlePanel("Turbidity"),

  sidebarPanel(selectInput("xcol", "X Variable", vars),
               selectInput("ycol", "Y Variable", vars)),

  mainPanel(plotOutput("turbidityPlot"))
)


server<- function(input, output){
  dfCleaned<- reactive({
    df %>%
      select(sites, input$xcol, input$ycol)
  })


  output$turbidityPlot<- renderPlot({

    ggplot(dfCleaned(), aes_string(x = input$xcol, y = input$ycol))+
      geom_point(aes(color = sites))



  })
}

shinyApp(ui, server)

#sdfs





