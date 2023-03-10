#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!("shiny" %in% installed.packages()[,"Package"]))install.packages("shiny")
if(!("dplyr" %in% installed.packages()[,"Package"]))install.packages("dplyr")
if(!("readr" %in% installed.packages()[,"Package"]))install.packages("readr")
if(!("ggplot2" %in% installed.packages()[,"Package"]))install.packages("ggplot2")
if(!("stringr" %in% installed.packages()[,"Package"]))install.packages("stringr")
if(!("emojifont" %in% installed.packages()[,"Package"]))install.packages("emojifont")
if(!("DT" %in% installed.packages()[,"Package"]))install.packages("DT")
if(!("foreach" %in% installed.packages()[,"Package"]))install.packages("foreach")
library(shiny)
require(foreach)
require(dplyr)
require(readr)
require(ggplot2)
require(stringr)
require(emojifont)
require(DT)
source("EasyeyesApp.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(
      new, 
      filter = "top",
      escape = FALSE,
      width = "180%",
      options = list(
        autoWidth = FALSE,
        paging = FALSE,
        scrollX=TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0)),
          list(targets = c(2), 
               className = 'details-control',
               render = JS(
                 "function(data, type, row, meta) {",
                 "return type === 'display' && data.length > 20 ?",
                 "data.substr(0, 20) + '...' : data;",
                 "}")),
          list(targets = c(3), render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 6 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
            "}"), className = 'information-control1'),
          list(targets = c(4), render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 6 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
            "}"), className = 'information-control2'),
          list(width = '120px', targets = c(8))
        )
      ),
      callback = JS("
                   table.column(2).nodes().to$().css({cursor: 'pointer'});
                  var format1 = function(d) {
                  return'<p>' + d[2] + '</p>'
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format1(row.data())).show();
                  }
                  })
                  table.column(3).nodes().to$().css({cursor: 'pointer'});
                  var format2 = function(d) {
                  return'<p>' + d[3] + '</p>'
                  };
                  table.column(4).nodes().to$().css({cursor: 'pointer'});
                  var format3 = function(d) {
                  return'<p>' + d[4] + '</p>'
                  };
                  table.on('click', 'td.information-control1', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format2(row.data())).show();
                  }
                  });
                  table.on('click', 'td.information-control2', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format3(row.data())).show();
                  }
                  });"
                    
      ))
  )
  
  # -1 means no pagination; the 2nd element contains menu labels
  output$ex2 <- renderTable(
    summary_table
  )
  
  output$ex3 <- renderPlot(p_mean)
  output$ex4 <- renderPlot(p_median)
})
