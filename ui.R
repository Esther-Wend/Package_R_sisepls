#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyWidgets)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)
library(DataExplorer)
library(data.table)
library(Hmisc)
library(MASS)
library(arsenal)
library(readxl)
library(caret)
library(PLSV2)
library(dplyr)

options(encoding = "UTF-8")
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Regression PLS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Exploration des données", tabName = "visualization", icon = icon("poll")),
      menuItem("PLS-Fit",tabName = "PLS-fit",icon = icon("table")),
      menuItem("PLS-predict",tabName = "PLS-predict",icon = icon("table")),
      menuItem("Graphique", tabName = "Graphique", icon = icon("poll"))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      # First tab content
      tabItem(tabName = "readData",
              fluidPage(
                mainPanel(uiOutput("tb") 
                ))),
      
      # Second tab content
      tabItem(tabName = "visualization",
              fluidPage(
                mainPanel(uiOutput("tb2")
                ))),
      
      ## Partie FIt
      tabItem(tabName = "PLS-fit",
              uiOutput("fitpls"
              )),
      
      ### Partie Prédiction
      tabItem(tabName = "PLS-predict",
              uiOutput("predictpls"
              )),
      
      ### Partie graphqiue
      tabItem(tabName = "Graphique",
              mainPanel(uiOutput("graphes")))
    )))