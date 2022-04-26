# Import the packages
library(tidyverse)
library(googlesheets4)
library(shiny)
library(shinydashboard)
library(corrplot)
library(expss)
library(plotly)
library(factoextra)
library(leaflet)
source("loadFib.R")
source("loadTurbidity.R")
source("loadArg.R")
source("clustering_analysis.R")
load("coords.RData")

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
                menuItem("Regression Analysis", tabName = "Turbidity"),
                menuItem("Clutering Analysis", tabName = "clustering"),
                menuItem("ANOVA", tabName = "anova"),
                menuItem("IDEXX vs. Plate", tabName = "ecoli"),
                menuItem("E.coli Concentration by Site", tabName = "bySite"))),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "Turbidity", h2("Regression on all features"),
              fluidPage(
                title = "Turbidity",
                fluidRow(
                  column(4,
                         sidebarPanel(selectInput("xcol", "X Variable", vars),
                                      selectInput("ycol", "Y Variable", vars),
                                      actionButton("refreshEcoli", "Reload E.coli - IDEXX"),
                                      actionButton("refresharg", "Reload E.coli - Plate"),
                                      actionButton("refresh", "Reload Turbidty & TSS"),
                                      checkboxInput("noLev", "Remove Outliers", FALSE),
                                      width = 10)),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Correlation Plot",
                                plotOutput("correlationPlot",height = "800px", width = "850px")),
                    tabPanel("Regression Output",
                              plotlyOutput("turbidityPlot", height = "500px"),
                              verbatimTextOutput("summary"),
                              plotOutput("residual"))))))),
      tabItem(tabName = "anova", h2("ANOVA for on site probe turbidity"),
              fluidPage(
                mainPanel(
                  verbatimTextOutput("anova_output")
                )
              )),
      
      tabItem(tabName = "clustering",h2("Clustering Analysis on Sentinel vs. On Site Prob"),
              fluidPage(
                fluidRow(
                  column(6,
                         plotOutput("plot_select_k"),
                         verbatimTextOutput("regression")),
                  column(6,
                         plotOutput("plot_clusters"),
                         plotOutput("plot_largest_cluster_regression"))))),
      tabItem(tabName = "ecoli", h2("IDEXX vs. Plate Method"),
              fluidPage(
                fluidRow(
                  mainPanel(
                    tabsetPanel(id = "tabset",
                                tabPanel("Antiobiotic Resistant E.coli", 
                                         plotOutput("ecoliPlot"),
                                         verbatimTextOutput("ecolilm")),
                                tabPanel("Antiobiotic Resistant E.coli - categorical",
                                         tableOutput("conti"),
                                         verbatimTextOutput("chisq")),
                                tabPanel("Total E.coli", 
                                         plotOutput("tec"),
                                         verbatimTextOutput("teclm"))))))),
      tabItem(tabName = "bySite", h2("Comparing E.coli Concentration by Sites"),
              fluidPage(
                mainPanel(leafletOutput("sitemap", width = 600, height = 400),
                          br(),
                          plotlyOutput("flo"),
                          br(),
                          plotlyOutput("yel"))))
    )
  )
)

server<- function(input, output){
  # Data Import
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
  dfCleaned<- eventReactive(input$refresh,{
    to_join<- plate_idexx() %>%
      select(sites, 
             date_sample,
             percent_resistant,
             without_ab_conc) %>%
      rename(plate_method_abrp = percent_resistant,
             plate_method_tc = without_ab_conc)
    loadTurbidity() %>% 
      left_join(to_join, c("sites", "date_sample"))
  })
  
  # Regression Analysis Tab
    # Model Construction/Removal of Outliers
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
  output$residual<- renderPlot({
    plot(fitted(m2()), resid(m2()), main = "Residual vs. Fitted")
    abline(h=0)
  })
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
      a<- paste0(l1[[i]][1], str_to_title(l1[[i]][2]),str_to_title(l1[[i]][3]),"")
      name1<- c(name1,a)
    }
    colnames(corrplotDf)<- name1
    corrplot(cor(corrplotDf[,-c(1,2,3)], use = "pairwise.complete.obs"), method = "number", type = "lower", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
  })
  
  # Clustering Analysis Tab
  output$plot_select_k<- renderPlot({
    plot_select_k
  })
  output$plot_clusters<- renderPlot({
    plot_clusters
  })
  output$regression<- renderPrint({
    summary(lm_largest_cluster)
  })
  output$plot_largest_cluster_regression<- renderPlot({
    plot_largest_cluster_regression
  })
  
  # ANOVA Tab
  output$anova_output<- renderPrint({
    df_cat<- within(dfCleaned(),{
                    ammonia_cat<- NA
                    ammonia_cat[ammonia < 0]<- "negative"
                    ammonia_cat[ammonia > 0]<- "positive"
                    phosphrous_cat<- NA
                    phosphrous_cat[phosphrous <= 0.2]<- "low"
                    phosphrous_cat[phosphrous > 0.2]<- "high"
                    plate_method_tc_cat<- NA
                    plate_method_tc_cat[plate_method_tc <= 100]<- "low"
                    plate_method_tc_cat[plate_method_tc > 100]<- "high"
                    plate_method_abrp_cat<- NA
                    plate_method_abrp_cat[plate_method_abrp == 0]<- "zero"
                    plate_method_abrp_cat[plate_method_abrp > 0]<- "non-zero"})
    #plate_method_abrp_cat ignored because not enough sample
    m1<- aov(on_site_probe_turbidity ~ phosphrous_cat+as.factor(nitrate)+ammonia_cat+plate_method_tc_cat, data = df_cat)
    summary(m1)
  })
  
  # IDEXX vs. Plate Method
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
      mutate_at(colnames(a)[2:3], as.integer)
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
      ylab("E.coli from IDEXX Method (MPN per 100mL)")+
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
  
  # Concentration by Site Tab
  output$flo<- renderPlotly({
    ggplot(plate_idexx(), aes(y = percent_resistant, x = sites))+
      geom_boxplot()+
      theme_bw()+
      labs(title = "Percent Antibiotic Resistant E.coli by Site")+
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none")
  })
  output$yel<- renderPlotly({
    ggplot(plate_idexx(), aes(y = without_ab_conc, x = sites))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("E.coli Concentration by Site")+
      theme(plot.title = element_text(hjust = 0.5, size = 12),
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none")
  })
  output$sitemap<- renderLeaflet({
    leaflet(coords) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addMarkers(lng = ~Long, lat = ~Lat, label = ~Sites, icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons8/ios7/512/Travel-Beach-icon.png',iconSize = c(24, 24)))
    
  })
}

shinyApp(ui, server)



  



