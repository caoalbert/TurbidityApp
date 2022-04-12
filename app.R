# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)

# Access Google token
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Name Plotting Variables
vars<- c("hydrolab_turbidity_ntu", "sentinel_acolite_turbidity_mean_fnu","landsat_acolite_turbidity_mean_fnu","tss")


ui<- dashboardPage( 
  dashboardHeader(title = "JPL Remote Sensing Results"),
  dashboardSidebar(),
  dashboardBody(
    
    
    fluidPage(titlePanel("Turbidity"),
            
              sidebarPanel(selectInput("xcol", "X Variable", vars),
                           selectInput("ycol", "Y Variable", vars),
                           actionButton("refresh", "refresh data")),
              mainPanel(plotOutput("turbidityPlot"))),
    fluidPage( tabPanel("summary", "Model Summary"))
    )
)
  
  
  
 


server<- function(input, output){
  
  # Use refresh button to prepare data
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
  

  # Create the plot
  output$turbidityPlot<- renderPlot({
   
    ggplot(dfCleaned(), aes_string(x = input$xcol, y = input$ycol))+
      geom_point(aes(color = sites))+
      geom_smooth(method = "lm", se = F)+
      theme_bw()+
      ggtitle(paste0(str_to_title(str_replace_all(input$ycol, "_", " ")),
                     " vs. ", 
                     str_to_title(str_replace_all(input$xcol, "_", " "))))+
      xlab(str_to_title(str_replace_all(input$xcol, "_", " ")))+
      ylab(str_to_title(str_replace_all(input$ycol, "_", " ")))+
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
  
  
  
  output$summary<- renderPrint({
    fit<- lm(input$ycol~input$xcol,df = dfCleaned())
    summary(fit)
  })
}

shinyApp(ui, server)





