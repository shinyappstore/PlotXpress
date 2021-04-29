# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotXpress: Shiny app for plotting and comparing the data from dual luciferase assays
# Created by Joachim Goedhart (@joachimgoedhart) Elias Brandorff and Marc Galland, first version 2020
# Takes the output of the Promoega GloMax as input together with a tidy CSV file that specifies conditions
# Alternatively the data can be uploaded in a tidy format
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copyright (C) 2020  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(DT)
library(readr)
library(stringr)

# library(RCurl)

add_mean <- function(x) {
  avg <- mean(x)
  triplet <- data.frame(avg, avg, avg)
  names(triplet) <- c("y","ymin","ymax") #this is what ggplot is expecting
  return (triplet)
}

add_median <- function(x) {
  med <- median(x)
  triplet <- data.frame(med, med, med)
  names(triplet) <- c("y","ymin","ymax") #this is what ggplot is expecting
  return (triplet)
}

df_example <- read_excel("DualLuc_example_data.xlsx", sheet = "Results")
df_design <- read.csv("Tidy_design.csv")
# df_design_Hek <- read_excel("Design_example_Hek.xlsx")
# df_design_neuron <- read_excel("Design_example_neuron.xlsx")

#Define dataframe for
column <- rep(1:12, each=8)
row <- rep(LETTERS[1:8],12)
x0 <- str_pad(column, 2, pad = "0")
Wells <- paste0(row,x0)
df_plate <- data.frame(column,row,Wells)


# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

###### UI: User interface #########

ui <- fluidPage(
  
  titlePanel("PlotXpress"),
  sidebarLayout(
    sidebarPanel(width=3,
                 
                 conditionalPanel(
                   condition = "input.tabs=='Data upload'",
                   h3("Data upload"),
                   
                   radioButtons(
                     "data_type", label = "",
                     choices = list("Promega Glomax data and separate experimental design" = 1, "Data in a single tidy csv" = 2), selected =  1),
                   
                   
                   conditionalPanel(
                     condition = "input.data_type=='1'",
                   
                                     radioButtons(
                                       "data_input", "1. Promega Glomax data",
                                       choices = list("Example Promega GloMax data" = 1, "Upload Promega GloMax xlsx file" = 3), selected =  1),
                                     conditionalPanel(
                                       condition = "input.data_input=='1'", h5("Example data from a Dual Luciferase assay acquired with Promega GloMax")
                                       
                                     )),
                   
                   
                   conditionalPanel(
                     condition = "input.data_type=='2'",
                     
                     h4("Upload a tidy CSV")),
                   
                   
                   
                   conditionalPanel(
                     condition = "input.data_input=='3' && input.data_type=='1'", fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),

                     NULL
                   ),
                   conditionalPanel(
                     condition = "input.data_type=='2'", fileInput("upload_tidy", NULL, multiple = FALSE, accept = c(".txt", ".csv")),
                     selectInput("Firefly", label = "Intensity data:", choices = list("-"), selected = "-"),
                     selectInput("Renilla", label = "Reference data (Optional):", choices = list("-"), selected = "-"),
                     selectInput("Condition", label = "Conditions:", choices = list("-"), selected = "-"),                     
                     selectInput("Treatment1", label = "Treatment1 (Optional):", choices = list("-"), selected = "-"),                     
                     selectInput("Treatment2", label = "Treatment2 (Optional):", choices = list("-"), selected = "-"),                     
                     
                     NULL
                   ),
                   # 
                   # ### csv via URL as input      
                   # conditionalPanel(
                   #   condition = "input.data_input=='5'",
                   #   #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"), 
                   #   textInput("URL", "URL", value = ""), 
                   #   NULL
                   # ),

                   
                   conditionalPanel(
                     condition = "input.data_type=='1'",
                   
                               # selectInput("signal", label = "Select data to display:", choices = list("renilla","firefly","renilla/firefly","firefly/renilla")),
            
                               # conditionalPanel(
                               #   condition = "input.tidyInput==false", (downloadButton("downloadData", "Download in tidy format (csv)"))),
                               
                               hr(),
                               radioButtons(
                                 "design_input", "2. Experimental design",
                                 choices = list("Example design (of Glomax example data)" = 1, "Upload in tidy format (CSV)"=2), selected =  1),
                               
                               conditionalPanel(
                                 condition = "input.design_input=='2'",
                                 fileInput("upload_design", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                                 a("Design template (CSV) available here", href="https://github.com/ScienceParkStudyGroup/PlotXpress/blob/master/Tidy_design.csv", target="_blank"), 
                                 # hr(),

            
            
                                 NULL
                               ),
                               # selectInput("show_condition", "Show the experimental condition:", choices = "all", selected = "all"),
                               hr()
                     ),
                     h3("Data selection"),
                               selectInput("filter_column", "Filter based on this parameter:", choices = "-", selected = "-"),
                               selectInput("remove_these_conditions", "Deselect these conditions:", "", multiple = TRUE),
            
                               hr(),
                               downloadButton("downloadData", "Download combined data in tidy format (CSV)")
            
                             ,
                   NULL
                 ),
                             
                 conditionalPanel(
                   condition = "input.tabs=='Plot'",
                   h4("Data presentation"),
                   selectInput("zero", "Select reference condition:", choices = "-"
                               # )
                   ),
                   selectInput("compare", label = "Compare:", choices = list("condition", "treatment1", "treatment2"), selected = "condition"),
                   selectInput("facet_row", label = "Split rows by:", choices = list(".", "treatment1", "treatment2","condition"), selected = "treatment1"),
                   selectInput("facet_col", label = "Split columns by:", choices = list(".", "treatment1", "treatment2","condition"), selected = "treatment2"),
                   
                   checkboxInput(inputId = "show_control",
                                 label = "Show reference data",
                                 value = FALSE),
                   
                   
                   sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.7),

                   
                   sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),  
                   
                   radioButtons("summaryInput", "Statistics", choices = list("Mean" = "mean", "Median" = "median", "None" = "none"), selected = "mean"),
                   # checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimum n=10)"), value = FALSE),
                   # conditionalPanel(
                   #   condition = "input.add_CI == true && input.summaryInput !='box'",
                   #   checkboxInput(inputId = "ugly_errors", label = "Classic error bars", value = FALSE)),
                   
                   #Uncomment for grey box that indicates range
                   # conditionalPanel(
                   #   condition = "input.summaryInput == 'median' || input.summaryInput == 'mean'",
                   #   
                   #   checkboxInput(inputId = "add_bar", label = HTML("Add a box that shows the range"), value = FALSE)),
                   # 
                   # sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),
                   # 
                   # radioButtons(inputId = "ordered",
                   #              label= "Order of the conditions:",
                   #              choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"),
                   #              selected = "none"),
                   
                   h4("Plot Layout"),      
                   
                   checkboxInput(inputId = "rotate_plot",
                                 label = "Rotate plot 90 degrees",
                                 value = FALSE),
                   
                   checkboxInput(inputId = "no_grid",
                                 label = "Remove gridlines",
                                 value = FALSE),
                   
                   checkboxInput(inputId = "change_scale",
                                 label = "Change scale",
                                 value = FALSE),
                   conditionalPanel(condition = "input.change_scale == true",
                                    checkboxInput(inputId = "scale_log_10",
                                                  label = "Log scale",
                                                  value = FALSE),
                                    
                                    textInput("range", "Range of values (min,max)", value = "")),
                   
                   # checkboxInput("color_data", "Use color for the data", value=FALSE),
                   # checkboxInput("color_stats", "Use color for the stats", value=FALSE),
                   # 
                   # conditionalPanel(
                   #   condition = "input.color_data == true || input.color_stats == true",
                   #   ########## Choose color from list
                   #   selectInput("colour_list", "Colour:", choices = ""),
                   #   
                   #   radioButtons("adjustcolors", "Color palette:", choices = 
                   #                  list(
                   #                    "Standard" = 1,
                   #                    "Okabe&Ito; CUD" = 6,
                   #                    "Tol; bright" = 2,
                   #                    "Tol; muted" = 3,
                   #                    "Tol; light" = 4,
                   #                    "User defined"=5),
                   #                selected =  6),
                   #   
                   #   conditionalPanel(condition = "input.adjustcolors == 5",
                   #                    textInput("user_color_list", "Names or hexadecimal codes separated by a comma (applied to conditions in alphabetical order):", value = "turquoise2,#FF2222,lawngreen"), 
                   #                    
                   #                    h5("",
                   #                       a("Click here for more info on color names",
                   #                         href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                   #                    
                   #   )),
                   # 
                   numericInput("plot_height", "Height (# pixels): ", value = 700),
                   numericInput("plot_width", "Width (# pixels):", value = 1000),
                   
                   h4("Labels/captions"),
                   
                   checkboxInput(inputId = "add_title",
                                 label = "Add title",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.add_title == true",
                     textInput("title", "Title:", value = "")
                   ),
                   
                   checkboxInput(inputId = "label_axes",
                                 label = "Change labels",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.label_axes == true",
                     textInput("lab_x", "X-axis:", value = ""),
                     textInput("lab_y", "Y-axis:", value = "")),
                   checkboxInput(inputId = "adj_fnt_sz",
                                 label = "Change font size",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.adj_fnt_sz == true",
                     numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
                     numericInput("fnt_sz_ax", "Size axis labels:", value = 18)),
                   # checkboxInput(inputId = "add_description",
                   #               label = "Add figure description",
                   #               value = FALSE),
                   NULL
                   
                 ),
                 
                 
                 
                 
                 conditionalPanel(
                   condition = "input.tabs=='About'",
                   
                   #Session counter: https://gist.github.com/trestletech/9926129
                   h4("About"),  "There are currently", 
                    verbatimTextOutput("count"),
                    "session(s) connected to this app.",
                   hr(),
                   h4("Find our other dataViz apps at:"),a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Data Summary'",
                   h4("Data summary") ,
                   checkboxGroupInput("stats_select", label = h5("Statistics for table:"), 
                                      choices = list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median"),
                                      selected = "sem"),
                   actionButton('select_all1','select all'),
                   actionButton('deselect_all1','deselect all'),
                   numericInput("digits", "Digits:", 2, min = 0, max = 5)
                   #        ,
                   #        selectInput("stats_hide2", "Select columns to hide", "", multiple = TRUE, choices=list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median")
                 )   
                 
    ),
    mainPanel(
      
      tabsetPanel(id="tabs",
                  tabPanel("Data upload",
                           
                           conditionalPanel(
                             condition = "input.data_type=='1'",

                             h4("Readings from the 96-well plate"),selectInput("signal", label = NULL, choices = list("renilla","firefly","renilla/firefly","firefly/renilla")),
                           plotOutput("coolplot"),                               

                           # h4("Sample description per well - plot"),
                           # plotOutput("designplot"),
                           h4("Sample description per well:"),
                           dataTableOutput("data_uploaded",
                                           NULL)),
                           conditionalPanel(
                             condition = "input.data_type=='2'", h4("Tidy data uploaded:"),dataTableOutput("tidy_data_uploaded"))
                             
                           
                  ),
                  

                  
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"),downloadButton("downloadPlotPNG", "Download png-file"), plotOutput("dataplot")
                  #          downloadButton("downloadPlotSVG", "Download svg-file"), 
                  #          downloadButton("downloadPlotEPS", "Download eps-file"), 
                           
                  #          actionButton("settings_copy", icon = icon("clone"),
                  #                       label = "Clone current setting"),
                  #          actionButton("legend_copy", icon = icon("clone"),
                  #                       label = "Copy Legend"),
                  #          
                  #          div(`data-spy`="affix", `data-offset-top`="10", plotOutput("coolplot", height="100%"),
                  #              htmlOutput("LegendText", width="200px", inline =FALSE),
                  #              #                                            htmlOutput("HTMLpreset"),
                                # NULL)
                   ), 
                  # tabPanel("Data Summary",dataTableOutput('data_summary')
                  # ),
                  tabPanel("About", includeHTML("about.html")
                  )
      )
    )
  )         
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  isolate(vals$count <- vals$count + 1)
  
  
  f.selected <- '-'
  r.selected <- '-'
  c.selected <- '-'
  t1.selected <- '-'
  t2.selected <- '-'
  
  
  
  
  ###### DATA INPUT ###################
  
  
df_upload_design <- reactive({
    
    if (input$design_input == 1) {
      data <- df_design

    } else if (input$design_input == 2) {

       file_in <- input$upload_design
    if (is.null(input$upload_design)) {
      isolate({data <- df_design})
    } else {
      #Isolate extenstion and convert to lowercase
      filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
      fileext <- tolower(filename_split[length(filename_split)])
      
      if (fileext=="xls" || fileext=="xlsx") {
        
        # names <- excel_sheets(path = input$upload$datapath)
        # updateSelectInput(session, "sheet_names", choices = names)
        data <- read_excel(file_in$datapath , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
      } else if (fileext == "txt" || fileext=="csv") {
        
        data <- read.csv(file=file_in$datapath, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"),header = TRUE, stringsAsFactors = FALSE)

      }
    }
    }
    data <- data %>% separate(Wells, c("row", "column"), sep=1, remove = FALSE)
    return(data)
  })
  
df_upload_data <- reactive({
  
  ### Glomax Data input
  if(input$data_type == 1) {
    
    if (input$data_input == 1) {
      data <- df_example
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x=1))
        # } else if (input$submit_datafile_button == 0) {
        #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        
        #Isolate extenstion and convert to lowercase
        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        
        if (fileext=="xls" || fileext=="xlsx") {
          
          # names <- excel_sheets(path = input$upload$datapath)
          # updateSelectInput(session, "sheet_names", choices = names)
          data <- read_excel(file_in$datapath, sheet = "Results" , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
        } 
        
        # })
      }}
    
    
    ### Tidy Data input - general
  } else if (input$data_type == 2) {
    file_in <- input$upload_tidy
    # Avoid error message while file is not uploaded yet
    if (is.null(input$upload_tidy)) {
      return(data.frame(x = "Upload a CSV file with at least two columns: one with conditions and one with values (intensities)"))
      # } else if (input$submit_datafile_button == 0) {
      #   return(data.frame(x = "Press 'submit datafile' button"))
    } else {
      data <- read.csv(file=file_in$datapath, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
    }
    f.selected <<- 'firefly'
    r.selected <<- 'renilla'
    c.selected <<- 'condition'
    t1.selected <<- 'treatment1'
    t2.selected <<- 'treatment2'
  }
  
    return(data)
})
  

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })


######### DEFINE DOWNLOAD BUTTONS ###########

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("PlotXpress_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotXpress_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)

df_tidy_data <- reactive({     

    df <- df_upload_data()
    if (dim(df)[1]<2) {
      return(data.frame(df_plate, firefly=1,renilla=1))
    }
    
    ######## SUBSET data from Promega output ###########
  
    # Perhaps need to do this to get last 12 columns: df[,(ncol(df)-11):ncol(df)]
    # Probably better is to find the cell with A and strat reading from column next to that
    
    #Subset firefly data
    firefly <- df[19:26,6:17] %>% unlist()
    
    #Subset renilla data
    renilla <- df[40:47,6:17] %>% unlist()

    df_filter <- data.frame(df_plate,firefly,renilla)

    return(df_filter)
  
    
})

# df_tidy_design <- reactive ({
#   
#   # df <- df_upload_design()
#   
#   if (input$design_input != 4) {
# 
#     
#     #Read the conditions for each well from the uploaded design file
#     condition <-  as.data.frame(df_upload_design())[1:8,2:13]  %>% unlist(use.names = FALSE)
#     
#     df <- data.frame(df_plate,condition)
#     
#     df <- df %>% separate(condition, into =c("condition", "treatment1", "treatment2"), sep="_")
#     
#     #Replace NA by 'empty' treatments
#     df <- df %>% replace_na(list(treatment1="-",treatment2 = "-"))
#     
#   } else if (input$design_input == 4) {
#     
#     df <- df_upload_design()
#     
#   }
#   observe({print(head(df))})
#   
#   return(df)
#   
# })


df_combined <- reactive({
  
  
  ###### Combine design and data for Glomax data upload
  if (input$data_type==1) {
    df_tidy_data <- df_tidy_data() %>% select(Wells,firefly,renilla)
    df <- full_join(df_upload_design(), df_tidy_data, by='Wells')
  } else {
    (df <- df_upload_data())
  }
})

df_filtered <- reactive({
  
  ##### FILTER Conditions ######
  if (!is.null(input$remove_these_conditions) && input$filter_column != "-") {
    
    filter_column <- input$filter_column
    remove_these_conditions <- input$remove_these_conditions
    
    observe({print(remove_these_conditions)})
    
    #Remove the columns that are selected (using filter() with the exclamation mark preceding the condition)
    # https://dplyr.tidyverse.org/reference/filter.html
    df_combined <- df_combined() %>% filter(!.data[[filter_column[[1]]]] %in% !!remove_these_conditions)
  } else {
    df_combined <- df_combined()
  }
  
  #Remove columns that are double
  # df_upload_design <- df_upload_design %>% select(-c(row,column))
  
  
  
  
  
})



df_processed <- reactive({   
  
  if (input$data_type == 2) {
  
  F_choice <- input$Firefly
  R_choice <- input$Renilla
  Condition <- input$Condition
  Treatment1 <- input$Treatment1
  Treatment2 <- input$Treatment2
  
  df <- df_filtered() %>% select(firefly = !!F_choice ,
                                    condition= !!Condition)

  
  if (!!R_choice =="-") {
    observe({print("no renilla reference")})
    df$renilla <- 1
  } else if (!!R_choice !="-") {
      x <- df_filtered() %>% select(renilla = !!R_choice)
      df$renilla <- x$renilla
    }
  
  if (!!Treatment1 =="-") {
    df$treatment1 <- '-'
    
  } else if (!!Treatment1 !="-") {
    x <- df_filtered() %>% select(treatment1 = !!Treatment1)
    df$treatment1 <- x$treatment1
  }
  
  if (!!Treatment2 =="-") {
    df$treatment2 <- '-'
  } else if (!!Treatment2 !="-") {
    x <- df_filtered() %>% select(treatment2 = !!Treatment2)
    df$treatment2 <- x$treatment2
  }

  } else if (input$data_type == 1) {
    df <- df_filtered()
  }
  #######Process the data ######
  observe({print(head(df))})
  
  #Caclulcate the ratio of readout over internal control
  df <- df %>% mutate(FR=firefly/renilla) 
  
  #Calculate the average of each group of conditions/treatments
  df <- df %>% group_by(condition,treatment1,treatment2) %>% mutate(avg=mean(FR))
  
  # select the control condition for calculating the 'Fold Change'
  control_condition <- input$zero
  
  if (control_condition != "-") {
      df_norm <- df %>% group_by(condition,treatment1,treatment2) %>% summarize(norm=mean(FR)) %>% filter(condition==!!control_condition)
      df_norm$condition <- NULL
      
      # Combine the normalization values (averages of the control) with the dataframe && omit conditions that equal "NA"
      df <- df %>% full_join(df_norm, by=c("treatment1","treatment2")) %>% na.omit(condition)
      
      df <- df %>% mutate(`Fold Change` = FR/norm)
  }
  
  return(df)
  
  
})

##### Get Variables from the input ##############

observe({
  df <- df_upload_data()
  # var_names  <- names(df)
  # var_list <- c("-", var_names)
  

#   
  # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
  nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
                             is.logical(x) ||
                             is.character(x),
                           df))
  nms_var <- names(Filter(function(x) is.integer(x) ||
                            is.numeric(x) ||
                            is.double(x),
                          df))
#   
    numbers_list <- c("-",nms_var)
    factors_list <- c("-",nms_fact)
#   mapping_list_num <- c("No",nms_var)
#   mapping_list_fact <- c("No",nms_fact)
#   mapping_list_all <- c("No",var_names)
    # facet_list_factors <- c(".",var_names)
#   
  updateSelectInput(session, "Firefly", choices = numbers_list, selected = f.selected)
  updateSelectInput(session, "Renilla", choices = numbers_list, selected = r.selected)
  updateSelectInput(session, "Condition", choices = factors_list, selected = c.selected)
  updateSelectInput(session, "Treatment1", choices = factors_list, selected = t1.selected)
  updateSelectInput(session, "Treatment2", choices = factors_list, selected = t2.selected)
  
  
  if (input$data_type ==1) {
    df2 <- df_upload_design()
  } else if (input$data_type ==2) {
    df2 <- df_upload_data()
  }
  var_names2 <- names(df2)
  var_list <- c("-", var_names2)
  updateSelectInput(session, "filter_column", choices = var_list)
  
  var_names3 <- levels(df2$condition)
  observe({print(var_names3)})
  var_list3 <- c("-", var_names3)
  updateSelectInput(session, "zero", choices = var_list3)
  
  
})


# Toggle facetting based on the number of treatments in the data

# observeEvent(input$data_input, {
#   if (input$data_input=="3")  {
#     if (df_upload_data() != "xx") {
#       if (unique(df$treatment1) == '-') {
#         updateSelectInput(session, "facet_row", selected = ".")
#       }
#       
#     }
#     
#   }
#   else if (input$data_input!="3")  {
# 
#     if (df_upload_data() != "xx") {
#       if (unique(df$treatment1) == '-') {
#         updateSelectInput(session, "facet_row", selected = ".")
#       }
#       
#     }
#     
#     
#   }
# })

########### Get the list of factors from a variable ############

observeEvent(input$filter_column != '-', {
  
  filter_column <- input$filter_column
  
  if (filter_column == "-") {filter_column <- NULL}
  
  koos <- df_combined() %>% select(for_filtering = !!filter_column)
  
  conditions_list <- levels(factor(koos$for_filtering))
  # observe(print((conditions_list)))
  updateSelectInput(session, "remove_these_conditions", choices = conditions_list)
  
})



#### Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  filename = function() {
    paste("PlotXpress_Tidy", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_processed(), file, row.names = FALSE)
  }
)



######## PREPARE PLOT FOR DISPLAY ##########
plotdata <-  reactive({

  df <- df_processed()
  # observe({print(head(df))})
  
  ############################ TODO ##############
  # Grid
  
  if (input$show_control == FALSE) {
    control_condition <- input$zero
    df <- df %>% filter(condition!=!!control_condition)
  }
  
  if (input$zero !="-") {
    p <-  ggplot(df, aes_string(x = input$compare, y = "`Fold Change`")) 
  } else if (input$zero =="-") {
    p <-  ggplot(df, aes_string(x = input$compare, y = "FR")) 
    
  }
  
  p <- p + geom_jitter(shape = 16, width=0.2, height=0.0, cex=input$pointSize, color="black", alpha=input$alphaInput) 
  
  if (input$summaryInput =="mean") {
   # p <-  p + stat_summary(fun = input$summaryInput, fun.min =input$summaryInput, fun.max = input$summaryInput, geom = "errorbar", width = 0.5, size=1) 

   p <- p + stat_summary(data=df, aes_string(x=input$compare),
                         fun.data = add_mean, 
                         geom = "errorbar", width=0.5, size=1)
  }
  
   
  if (input$summaryInput =="median") {
    # p <-  p + stat_summary(fun = input$summaryInput, fun.min =input$summaryInput, fun.max = input$summaryInput, geom = "errorbar", width = 0.5, size=1) 
    
    p <- p + stat_summary(data=df, aes_string(x=input$compare),
                          fun.data = add_median, 
                          geom = "errorbar", width=0.5, size=1)
  }
  


  
  ########### Do some formatting of the lay-out ###########
  
  p <- p+ theme_light(base_size = 16)
  
  
  p <- p+theme(strip.background = element_rect(color='grey50', fill='grey95', size=1, linetype="solid"),strip.text = element_text(size = 12, color = "grey20"))
  
  #remove gridlines (if selected)
  if (input$no_grid == TRUE) {
    p <- p+ theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
  }
  
  # if font size is adjusted
  if (input$adj_fnt_sz) {
    p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
    p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
  }
  
  # if log-scale checked specified
  if (input$scale_log_10)
    p <- p + scale_y_log10() 
  
  #Adjust scale if range (min,max) is specified
  if (input$range != "" &&  input$change_scale == TRUE) {
    rng <- as.numeric(strsplit(input$range,",")[[1]])
    
    # #If min>max invert the axis
    # if (rng[1]>rng[2]) {p <- p+ scale_y_reverse()}
    
    #Autoscale if rangeis NOT specified
  } else if (input$range == "" || input$change_scale == FALSE) {
    rng <- c(NULL,NULL)
  }
  
  p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
  #### If selected, rotate plot 90 degrees CW ####
  if (input$rotate_plot == TRUE) { 
    p <- p + coord_flip(ylim=c(rng[1],rng[2]))
    # This ensures correct order when plot is rotated 90 degrees
    p <- p+scale_x_discrete(limits = rev)
  }
  
  # if title specified
  if (input$add_title)
    p <- p + ggtitle(input$title)
  
  # if labels specified
  if (input$label_axes)
    p <- p + labs(x = input$lab_x, y = input$lab_y)
  
  row <- as.character(input$facet_row)
  col <- as.character(input$facet_col)
  
  facetFormula <- as.formula(paste(input$facet_row, "~", input$facet_col))
  
  p <- p+ facet_grid(facetFormula)
  
  # p <- p + facet_grid(treatment1~treatment2)
  
  return(p)
})

plotplate <- reactive({
  
  df <- df_tidy_data()
  
  # signal <- input$signal
  
  plate_plot <-  ggplot(data=df, aes(x=column,y=reorder(row, desc(row)))) +geom_point(aes_string(color=input$signal), size=15)+coord_fixed()+scale_x_continuous(breaks=seq(1, 12), position = "top")+scale_color_viridis_c()
  plate_plot <- plate_plot + theme_light() + labs(x = NULL, y = NULL)  + theme(legend.position = "none")
  plate_plot
  
  
})

plotdesign <- reactive({
  df <- df_upload_design()
  df$column <- as.numeric(df$column)
  plate_plot <- ggplot(data=df, aes(x=column,y=reorder(row, desc(row)))) +geom_point(aes_string(color="treatment1"), size=22, stroke=0,shape=15, alpha=0.4) + scale_color_grey(start=0.3, end=0.7)
  
  # plate_plot <- plate_plot+geom_point(aes_string(color="treatment2", shape="treatment1"), size=6)

  
  plate_plot <- plate_plot+geom_label(aes(label=condition, fill=treatment2), alpha=0.8)

  plate_plot <- plate_plot + coord_fixed()+scale_x_continuous(breaks=seq(1, 12), position = "top", limits=c(1,12))
  plate_plot <- plate_plot + theme_light() + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")
  plate_plot

  
})



#### DISPLAY UPLOADED DATA (as provided) ##################

output$data_uploaded <- renderDataTable(
  
  #    observe({ print(input$tidyInput) })
  df_upload_design(),
  
  
  rownames = FALSE,
  options = list(pageLength = 96,
                 lengthMenu = c(10, 96, 1000, 10000), columnDefs = list(list(className = 'dt-left', targets = '_all')), scrollX = TRUE),
  editable = FALSE,selection = 'none'
  
  
  
  
  
  
  # rownames = FALSE,
  # options = list(
  #               # paging = FALSE,
  #                #
  #                searching = FALSE,
  #                # autoWidth = TRUE,
  #                #GENERATE ellipses (...) for truncated text
  #                # 
  #                # columnDefs = list(list(targets = "_all",
  #                #                        render = JS(
  #                #                          "function(data, type, row, meta) {",
  #                #                          "return type === 'display' && data.length > 12 ?",
  #                #                          "'<span title=\"' + data + '\">' + data.substr(0, 10) + '..</span>' : data;",
  #                #                          "}")
  #                #                        )),
  #               scrollX = TRUE),
  # # callback = JS('table.page(3).draw(false);'),
  # editable = FALSE,selection = 'none'
)

##### Render the plot ############
output$coolplot <- renderPlot(width = 700, {
  # dft <- df_upload_data()
  # if (dim(dft)[1]<2) {
  #   observe({print("no data")})
  #   return(NULL)
  # }
  plot(plotplate())
})

output$designplot <- renderPlot(width = 700, {
  plot(plotdesign())
})


output$dataplot <- renderPlot(width = width, height = height, {
  plot(plotdata())
})

#### DISPLAY UPLOADED DATA (as provided) ##################

output$tidy_data_uploaded <- renderDataTable(
  
  #    observe({ print(input$tidyInput) })
  df_filtered(),
  rownames = FALSE,
  options = list(pageLength = 10,
                 lengthMenu = c(10, 100, 1000, 10000), columnDefs = list(list(className = 'dt-left', targets = '_all'))),
  editable = FALSE,selection = 'none'
)

# ########### Update count #########
# Reactively update the client.
output$count <- renderText({
  vals$count
})




# # When a session ends, decrement the counter.
session$onSessionEnded(function(){

  isolate(vals$count <- vals$count - 1)
  # End R-session when browser closed
  #      stopApp()
})




######## The End; close server ########################

} #close "server"

# Run the application 
shinyApp(ui = ui, server = server)

