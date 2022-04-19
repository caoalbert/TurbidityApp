# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)
library(shinydashboard)
library(corrplot)
library(expss)
library(plotly)
source("loadFib.R")
source("loadTurbidity.R")
source("loadArg.R")
source("clustering_analysis.R")

# Access Google token
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Name Plotting Variables
ui<- dashboardPage( 
  dashboardHeader(title = "JPL Remote Sensing Results"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Turbidity", tabName = "Turbidity"),
                menuItem("Clutering Analysis", tabName = "clustering"),
                menuItem("IDEXX vs. Plate", tabName = "ecoli"),
                menuItem("Coliform by Site", tabName = "bySite"))),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
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
                         plotlyOutput("turbidityPlot", height = "500px"),
                         verbatimTextOutput("summary"))))),
      tabItem(tabName = "clustering",h2("Clustering Analysis on Sentinel vs. On Site Prob"),
              fluidPage(
                fluidRow(
                  column(6,
                         plotOutput("selk"),
                         verbatimTextOutput("regre")),
                  column(6,
                         plotOutput("results"),
                         plotOutput("clusterlm"))))),
      tabItem(tabName = "ecoli", h2("IDEXX vs. Plate Method"),
              fluidPage(
                fluidRow(
                  column(3,
                         sidebarPanel(actionButton("refreshEcoli", "Reload E.coli - IDEXX"),
                                      actionButton("refresharg", "Reload E.coli - Plate"),
                                      width = 15)),
                         mainPanel(
                           tabsetPanel(
                             id = "tabset",
                             tabPanel("Antiobiotic Resistant E.coli", 
                                      plotOutput("ecoliPlot"),
                                      verbatimTextOutput("ecolilm")),
                             tabPanel("Antiobiotic Resistant E.coli - categorical",
                                      tableOutput("conti"),
                                      verbatimTextOutput("chisq")),
                             tabPanel("Total E.coli", 
                                      plotOutput("tec"),
                                      verbatimTextOutput("teclm"))))))),
      tabItem(tabName = "bySite", h2("Comparing Coliform Concentrtaion by Sites"),
              fluidPage(
                mainPanel(plotOutput("flo"),
                          plotOutput("yel"))))
    )
  )
)


server<- function(input, output){
  # Data Import
  dfCleaned<- eventReactive(input$refresh,{
    loadTurbidity()
  })
  fib<- eventReactive(input$refreshEcoli,{
    loadFib()
  })
  arg<- eventReactive(input$refresharg,{
    loadArg()
  })
  plate_idexx<- reactive({
    arg() %>% 
      inner_join(fib(), c("sites", "date_sample")) %>%
      mutate(Plate = cat_antibiotics,
             IDEXX = cat_ecoli_resistant)
  })
  
  # Turbidity - Build the linear model and remove outliers if selected
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
  
  # Create Turbidity Plot
  output$turbidityPlot<- renderPlotly({
    ggplot(dfCleaned2(), aes_string(x = input$xcol, y = input$ycol))+
      geom_point(aes(color = sites, text = date_sample), size =0.75)+
      geom_smooth(method = "lm", se = F, size = 0.5)+
      scale_x_continuous(limits = c(0, NA))+
      theme_bw()+
      ggtitle(paste0(str_to_title(str_replace_all(input$ycol, "_", " ")),
                     " vs. ", 
                     str_to_title(str_replace_all(input$xcol, "_", " "))))+
      xlab(str_to_title(str_replace_all(input$xcol, "_", " ")))+
      ylab(str_to_title(str_replace_all(input$ycol, "_", " ")))+
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none")
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
  
  
  output$selk<- renderPlot({
    
    output1
    
  })
  
  output$results<- renderPlot({
    output2
  })
  
  output$regre<- renderPrint({
    summary(cluster_model)
  })
  output$clusterlm<- renderPlot({
    output3
  })
  
  # Create Antibiotic Resistant E.coli Concentration Percentage Plot
  output$ecoliPlot<- renderPlot({
    ggplot(plate_idexx(), aes(x = percent_resistant, y = percent_ecoli_resistant))+
      geom_point(aes(color = sites))+
      geom_smooth(method = "lm", se = F)+
      xlab("Antibiotic Resistant E.coli from Plate Method (%)")+
      ylab("Antibiotic Resistant E.coli from IDEXX Method (%)")+
      ggtitle("Percentage of Antiobitic Resistant E.coli")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
  output$ecolilm<- renderPrint({
    m1<- lm(data = plate_idexx(), percent_ecoli_resistant~percent_resistant)
    summary(m1)
  })
  
  # Contingency Table for Categorical Analysis
  output$conti<- renderTable({
    a<- cross_cases(plate_idexx(), Plate, IDEXX)
    a<- a %>%
      mutate_at(colnames(a)[2:4], as.integer)
    a[1:3,]
  })
  output$chisq<- renderPrint({
    chisq.test(plate_idexx()$Plate, plate_idexx()$IDEXX)
  })
  
  # Total E.coli Plot
  output$tec<- renderPlot({
    ggplot(plate_idexx(), aes(y = correction_flo_tc, x = without_ab_conc))+
      geom_point(aes(color = sites))+
      geom_smooth(method = "lm", se = F)+
      ylab("E.coli from IDEXX Method (CFU per 100mL)")+
      xlab("E.coli from Plate Method (CFU per 100mL)")+
      ggtitle("E.coli Concentration")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
  output$teclm<- renderPrint({
    m1<- lm(data=plate_idexx(), correction_flo_tc~without_ab_conc)
    summary(m1)
  })
  
  # Tab3 Site Comparison
  output$flo<- renderPlot({
    df_flo<- fib() %>% 
      group_by(sites) %>% 
      summarise(mean_tcesbl = mean(correction_flo_tc_esbl),
                mean_flotc = mean(correction_flo_tc))
    a<- as.data.frame(cbind(df_flo$sites, df_flo$mean_flotc, rep("Flo_TC", nrow(df_flo))))
    b<- as.data.frame(cbind(df_flo$sites, df_flo$mean_tcesbl, rep("Flo_TC_ESBL", nrow(df_flo))))
    df_flo2<- rbind(a,b)
    colnames(df_flo2)<- c("Sites", "Coliform", "Type")
    df_flo2<- df_flo2 %>%
      mutate(Coliform = as.numeric(Coliform))
    ggplot(df_flo2, aes(fill = Type, y = Coliform, x = Sites))+
      geom_bar(position = "stack", stat = "identity")+
      theme_bw()+
      ggtitle("Mean E.coli Concentration by Sites")+
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
  output$yel<- renderPlot({
    df_yel<- fib() %>% 
      group_by(sites) %>% 
      summarise(mean_yel_tcesbl = mean(correction_yel_tc_esbl),
                mean_yel_tc = mean(correction_yel_tc))
    a<- as.data.frame(cbind(df_yel$sites, df_yel$mean_yel_tc, rep("yel_TC", nrow(df_yel))))
    b<- as.data.frame(cbind(df_yel$sites, df_yel$mean_yel_tcesbl, rep("yel_TC_ESBL", nrow(df_yel))))
    df_yel2<- rbind(a,b)
    colnames(df_yel2)<- c("Sites", "Coliform", "Type")
    df_yel2<- df_yel2 %>%
      mutate(Coliform = as.numeric(Coliform))
    ggplot(df_yel2, aes(fill = Type, y = Coliform, x = Sites))+
      geom_bar(position = "stack", stat = "identity")+
      theme_bw()+
      ggtitle("Mean Coliform Concentration by Sites")+
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.title = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
  
}

shinyApp(ui, server)



  



