#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)

# Define UI for application that draws a histogram
library(shiny)

shinyUI(navbarPage(
  title = 'EasyEyes Analysis',
  tabPanel('Error Report',     DT::dataTableOutput('ex1')),
  tabPanel('Per Condition',        tableOutput('ex2')),
  tabPanel('Plots', plotOutput("ex3"),plotOutput("ex4"))
))
