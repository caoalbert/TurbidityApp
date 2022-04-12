# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)
library(shinydashboard)
library(corrplot)

# Access Google token
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Name Plotting Variables
vars<- c("hydrolab_turbidity_ntu", 
         "sentinel_acolite_turbidity_mean_fnu",
         "landsat_acolite_turbidity_mean_fnu","tss", 
         "on_site_probe_turbidity",
         "in_lab_probe_turbidity",
         "sentinel_gee_turbidity_mean_ntu")


ui<- dashboardPage( 
  dashboardHeader(title = "JPL Remote Sensing Results"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Turbidity", tabName = "Turbidity"),
                menuItem("nextTab", tabName = "nextTab"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Turbidity", h2("Turbidity"),
              fluidPage(
                title = "Turbidity",
                fluidRow(
                  column(4,
                         sidebarPanel(selectInput("xcol", "X Variable", vars),
                                      selectInput("ycol", "Y Variable", vars),
                                      actionButton("refresh", "refresh data"),
                                      checkboxInput("noLev", "Remove Outliers", FALSE),
                                      width = 10),
                         plotOutput("correlationPlot")),
                  column(8,
                         plotOutput("turbidityPlot"),
                         verbatimTextOutput("summary"))))),
      tabItem(tabName = "nextTab", h2("new tab"))
    )
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
             landsat_acolite_turbidity_mean_fnu = as.numeric(unlist(landsat_acolite_turbidity_mean_fnu)),
             on_site_probe_turbidity = as.numeric(unlist(on_site_probe_turbidity)),
             in_lab_probe_turbidity = as.numeric(unlist(in_lab_probe_turbidity)),
             sentinel_gee_turbidity_mean_ntu = as.numeric(unlist(sentinel_gee_turbidity_mean_ntu)))
    tss<- tss %>%
      group_by(date_sample, sites)%>%
      summarise(tss = mean(tss_g_l, na.rm = T))
    hydro %>%
      select(date_sample,
             sites,
             hydrolab_turbidity_ntu,
             sentinel_acolite_turbidity_mean_fnu,
             landsat_acolite_turbidity_mean_fnu,
             on_site_probe_turbidity,
             in_lab_probe_turbidity,
             sentinel_gee_turbidity_mean_ntu) %>%
      left_join(tss, c("date_sample", "sites")) 
  })
  


  
  # Build the linear model and remove outliers if selected
  f <- reactive({
    as.formula(paste(input$ycol, "~", input$xcol))
  })
  m1 <- reactive({
    lm(f(), data = dfCleaned())
  })
  
  dfCleaned2<- reactive({
    if(input$noLev){
      summary1<- summary(m1())
      n<- summary1$df[2]+1
      HighLeverage<- cooks.distance(m1()) > (4/n)
      LargeResiduals<- rstudent(m1()) > 3
      dfCleaned() %>%
        slice(as.numeric(names(which(!HighLeverage & !LargeResiduals == T))))
    }else{
      dfCleaned()
    }
  })
  m2 <- reactive({
    lm(f(), data = dfCleaned2())
  })
  output$summary<- renderPrint(summary(m2()))
  
  
  
  # Create the plot
  output$turbidityPlot<- renderPlot({
    
    ggplot(dfCleaned2(), aes_string(x = input$xcol, y = input$ycol))+
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
  
  # Create Correlation Plot
  output$correlationPlot<- renderPlot({
    corrplotDf<- dfCleaned()
    l1<- str_split(colnames(corrplotDf), "_")
    name1<- c()
    for(i in 1:length(l1)){
      a<- paste0(l1[[i]][1], str_to_title(l1[[i]][2]),"")
      name1<- c(name1,a)
    }
    colnames(corrplotDf)<- name1
    
    corrplot(cor(corrplotDf[,-c(1,2)], use = "pairwise.complete.obs"), method = "number", type = "lower", tl.cex = 0.8)
  })
}

shinyApp(ui, server)




