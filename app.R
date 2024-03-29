
#ui.r

library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)

source("working_loadat.R")
source("variant_time.R")
source("seqs_metrics.R")
#source("region_data.R")




ui <- dashboardPage(
  dashboardHeader(title = "SARS-CoV-2 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Variant Metrics", tabName = "variants", icon = icon("wave-square")),
      menuItem("Sequencing Metrics", tabName = "seq", icon = icon("dna")),
      menuItem("Region Metrics", tabName = "region", icon = icon("compass"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
                h2("SARS-CoV-2 Dashboard for DCLS use"),
                p("This dashboard was developed to aid in the visualization of SARS-CoV-2 data produced both by DCLS, 
                  and other state agencies. All of the data used to produce the visulizations within this dashboard were pulled from 
                  GISAID. This data may be different than other state and federal agencies due to the frequency of updates 
                  and the source in which the data has been pulled from.")
                
              ),
      tabItem(tabName = "variants",
              tabsetPanel(
                tabPanel("All Virginia Data",
                  fluidRow(
                    column(width = 12, plotlyOutput("allvtimeplot"))),
                    column(width = 3,selectInput("time", label = h5("Select Time Period"),
                                choices = c("Weekly", "Monthly", "Quarterly"))),
                    column(width = 3,selectInput("vars", label = h5("Select Data Type"),
                                choices = c("WHO Variant", "Pango Lineage")))),

                tabPanel("DCLS Data",
                         fluidRow(
                           column(width = 12, plotlyOutput("dclstimeplot"))),
                           column(width = 3, selectInput("time2", label = h5("Select Time Period"),
                                       choices = c("Weekly", "Monthly", "Quarterly"))),
                           column(width = 3, selectInput("vars2", label = h5("Select Data Type"),
                                                         choices = c("WHO Variant", "Pango Lineage"))))

              )),
              
      tabItem(tabName = "seq", 
              tabsetPanel(
                tabPanel(
                  "Cumulative Sequences",
                  fluidRow(
                    column(width = 12, plotlyOutput("seqs")),
                    selectInput("type", label = h5("Select Data Source"),
                                choices = c("All Data", "DCLS")))
                ),
                tabPanel(
                  "Variants of Concern",
                  fluidRow(
                    column(width = 12, plotlyOutput("vocs")),
                    selectInput("type2", label = h5("Select Data Source"),
                                choices = c("All Data", "DCLS")))
                ),
                tabPanel(
                  "Variants of Interest",
                  fluidRow(
                    column(width = 12, plotlyOutput("vois")),
                    selectInput("type3", label = h5("Select Data Source"),
                                choices = c("All Data", "DCLS"))))
                            )
              ),
      tabItem(tabName = "region",
              tabsetPanel(
                tabPanel(
                  "Variants per region",
                  fluidRow(
                    column(width = 12, plotlyOutput("regs")),
                    selectInput("region", label = h5("Select Data Source"),
                                choices = c("Health District", "Health Region"))))
              ))
      )
  )
)




#server.r#
server <- function(input,output){ 
##Variants Panel##
  alltimeplot = reactive({
    if ("Weekly" %in% input$time && "Pango Lineage" %in% input$vars) return(g_weekfig_lin)
    if ("Monthly" %in% input$time && "Pango Lineage" %in% input$vars) return(g_monthfig_lin)
    if ("Quarterly" %in% input$time && "Pango Lineage" %in% input$vars) return(g_quartfig_lin)
    if ("Weekly" %in% input$time && "WHO Variant" %in% input$vars) return(g_weekfig_var)
    if ("Monthly" %in% input$time && "WHO Variant" %in% input$vars) return(g_monthfig_var)
    if ("Quarterly" %in% input$time && "WHO Variant" %in% input$vars) return(g_quartfig_var)
  })
  output$allvtimeplot <- renderPlotly({
    dataplots = alltimeplot()
    print(dataplots)
  })
  
  
  dclstimeplot = reactive({
    if ("Weekly" %in% input$time2 && "Pango Lineage" %in% input$vars2) return(d_weekfig_lin)
    if ("Monthly" %in% input$time2 && "Pango Lineage" %in% input$vars2) return (d_monthfig_lin)
    if ("Quarterly" %in% input$time2 && "Pango Lineage" %in% input$vars2) return (d_quartfig_lin)
    if ("Weekly" %in% input$time2 && "WHO Variant" %in% input$vars2) return(d_weekfig_var)
    if ("Monthly" %in% input$time2 && "WHO Variant" %in% input$vars2) return(d_monthfig_var)
    if ("Quarterly" %in% input$time2 && "WHO Variant" %in% input$vars2) return(d_quartfig_var)
  })
  output$dclstimeplot = renderPlotly({
    dataplots2 = dclstimeplot()
    print(dataplots2)
  })
  

  
  
  
##Sequences Panel##  
 cseqs = reactive({
   if("All Data" %in% input$type) return(g_seqs)
   if("DCLS" %in% input$type) return(d_seqs)
 })
  
  output$seqs = renderPlotly({
    seqplot1 = cseqs()
    print(seqplot1)
  })
  
  vocseqs = reactive({
    if("All Data" %in% input$type2) return(allvoc)
    if("DCLS" %in% input$type2) return(dvoc)
  })
  output$vocs = renderPlotly({
    seqplot2 = vocseqs()
    print(seqplot2)
  })
  
  voiseqs = reactive({
    if("All Data" %in% input$type3) return(allvoi)
    if("DCLS" %in% input$type3) return(dvoi)
  })
  output$vois = renderPlotly({
    seqplot3 = voiseqs()
    print(seqplot3)
  })
  ##### Region graphs
  
  # region = reactive({
  #   if("Health District" %in% input$region) return (dis_fig)
  #   if("Health Region" %in% input$region) return(reg_fig)
  # })
  # output$regs = renderPlotly({
  #   regplot1 = region()
  #   print(regplot1)
  # })
  
  
  
}

shinyApp(ui,server)
