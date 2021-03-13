# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotXpress: Shiny app for plotting and comparing the data from dual luciferase assays
# Created by Joachim Goedhart (@joachimgoedhart) Elias Brandorff and Marc Galland, first version 2020
# Takes non-tidy, spreadsheet type data as input
# Uses a CSV file with the conditions
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


df_example <- read_excel("DualLuc_example_data.xlsx", sheet = "Results")
df_design_all <- read_excel("Design_example.xlsx")
df_design_Hek <- read_excel("Design_example_Hek.xlsx")
df_design_neuron <- read_excel("Design_example_neuron.xlsx")

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
                   h4("Data upload"),
                   radioButtons(
                     "data_input", "",
                     choices = list("Example data" = 1, "Upload Promega GloMax xlsx file" = 3), selected =  1),
                   conditionalPanel(
                     condition = "input.data_input=='1'", h6("Example data from a Dual Luciferase assay acquired with Promega GloMax")
                     
                   ),
                   conditionalPanel(
                     condition = "input.data_input=='3'", fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),

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
                   selectInput("signal", label = "Select signal:", choices = list("renilla","firefly","renilla/firefly","firefly/renilla"
                                                                                 
                   )),

                   # conditionalPanel(
                   #   condition = "input.tidyInput==false", (downloadButton("downloadData", "Download in tidy format (csv)"))),
                   
                   hr(),
                   radioButtons(
                     "design_input", "Experimental design",
                     choices = list("Example design (all)" = 1, "Example design (subset)" = 2,
                                    "Upload in plate format (CSV, TXT, Excel)" = 3, "Upload in tidy format (CSV)"=4), selected =  1),
                   
                   conditionalPanel(
                     condition = "input.design_input=='3' || input.design_input=='4'", fileInput("upload_design", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                     hr(),


                     NULL
                   ),
                   selectInput("show_condition", "Show the experimental condition:", choices = "all", selected = "all"),
                   hr(),
                   selectInput("filter_column", "Filter based on this parameter:", choices = "-", selected = "-"),
                   selectInput("remove_these_conditions", "Deselect these conditions:", "", multiple = TRUE),

                   conditionalPanel(
                     condition = "input.info_data==true",
                     img(src = 'Data_format.png', width = '100%'), h5(""), a("Background info for converting wide data to tidy format", href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")
                   ),
                   hr(),
                   downloadButton("downloadData", "Download combined data (CSV)")

                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Plot'",
                   h4("Data presentation"),
                   selectInput("compare", label = "Compare:", choices = list("condition", "treatment1", "treatment2"), selected = "condition"),
                   selectInput("facet_row", label = "Split rows by:", choices = list(".", "treatment1", "treatment2","condition"), selected = "treatment1"),
                   selectInput("facet_col", label = "Split columns by:", choices = list(".", "treatment1", "treatment2","condition"), selected = "treatment2"),
                   
                   checkboxInput(inputId = "show_control",
                                 label = "Show control data",
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
                  tabPanel("Data upload", h4("Readings from the 96-well plate"),plotOutput("coolplot"),h4("Sample description per well - plot"), plotOutput("designplot"),h4("Sample description per well - table"),dataTableOutput("data_uploaded")),
                  

                  
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
  ###### DATA INPUT ###################
  
  
df_upload_design <- reactive({
    
    if (input$design_input == 1) {
      data <- df_design_all
    } else if (input$design_input == 2) {
      data <- df_design_neuron
    } else if (input$design_input == 3 || input$design_input == 4) {

       file_in <- input$upload_design
    if (is.null(input$upload_design)) {
      isolate({data <- df_design_all})
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
    return(data)
  })
  
df_upload_data <- reactive({
    if (input$data_input == 1) {
      data <- df_example
    }  else if (input$data_input == 2) {
      data <- df_example 
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Click 'Browse...' to select a datafile or drop file onto 'Browse' button"))
        # } else if (input$submit_datafile_button == 0) {
        #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        
        #Isolate extenstion and convert to lowercase
        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        
        # observe({print(fileext)})
        
        # isolate({
        # data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
        

          if (fileext=="xls" || fileext=="xlsx") {

          # names <- excel_sheets(path = input$upload$datapath)
          # updateSelectInput(session, "sheet_names", choices = names)
          data <- read_excel(file_in$datapath, sheet = "Results" , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
        } 
        
        # })
      }
      
    } else if (input$data_input == 5) {
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"))
      } else if (url.exists(input$URL) == FALSE) {
        return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read_csv(input$URL)}
      
      #Read the data from textbox
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
      }
  }
    # updateSelectInput(session, "data_remove", choices = names(data))
    
    #Replace space and dot of header names by underscore
    # data <- data %>%  
    #   select_all(~gsub("\\s+|\\.", "_", .))
    

  
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



##### Read data from Promega GloMax #########
df_tidy_data <- reactive({     

    df <- df_upload_data()
    if (dim(df)[1]<2) {
      return(data.frame(y,x,firefly=1,renilla=1))
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

df_tidy_design <- reactive ({
  
  # df <- df_upload_design()
  
  if (input$design_input != 4) {

    
    #Read the conditions for each well from the uploaded design file
    condition <-  as.data.frame(df_upload_design())[1:8,2:13]  %>% unlist(use.names = FALSE)
    
    df <- data.frame(df_plate,condition)
    
    df <- df %>% separate(condition, into =c("condition", "treatment1", "treatment2"), sep="_")
    
    #Replace NA by 'empty' treatments
    df <- df %>% replace_na(list(treatment1="-",treatment2 = "-"))
    
  } else if (input$design_input == 4) {
    
    df <- df_upload_design()
    
  }
  observe({print(head(df))})
  
  return(df)
  
})


df_combined <- reactive({    
  
  ##### FILTER Conditions ######
  if (!is.null(input$remove_these_conditions) && input$filter_column != "-") {
    
    filter_column <- input$filter_column
    remove_these_conditions <- input$remove_these_conditions
    
    observe({print(remove_these_conditions)})
    
    #Remove the columns that are selected (using filter() with the exclamation mark preceding the condition)
    # https://dplyr.tidyverse.org/reference/filter.html
    df_tidy_design <- df_tidy_design() %>% filter(!.data[[filter_column[[1]]]] %in% !!remove_these_conditions)
  } else {
    df_tidy_design <- df_tidy_design()
  }
  
  #Remove columns that are double
  df_tidy_design <- df_tidy_design %>% select(-c(row,column))
  
  #Join the design with the data
  df <- full_join(df_tidy_design, df_tidy_data(), by='Wells')

  #######Process the data ######
  
  #Caclulcate the ratio of readout over internal control
  df <- df %>% mutate(FR=firefly/renilla) 
  
  #Calculate the average of each group of conditions/treatments
  df <- df %>% group_by(condition,treatment1,treatment2) %>% mutate(avg=mean(FR))
  
  df_norm <- df %>% group_by(condition,treatment1,treatment2) %>% summarize(norm=mean(FR)) %>% filter(condition=="control")
  df_norm$condition <- NULL
  
  # Combine the normalization values (averages of the control) with the dataframe && omit conditions that equal "NA"
  df <- df %>% full_join(df_norm, by=c("treatment1","treatment2")) %>% na.omit(condition)
  
  df <- df %>% mutate(`Fold Change` = FR/norm)

  return(df)
  
  })


##### Get Variables from the input ##############

observe({
  df <- df_tidy_design()
  var_names  <- names(df)
  varx_list <- c("-", var_names)
#   
#   # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
#   nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
#                              is.logical(x) ||
#                              is.character(x),
#                            df))
#   nms_var <- names(Filter(function(x) is.integer(x) ||
#                             is.numeric(x) ||
#                             is.double(x),
#                           df))
#   
#   vary_list <- c("-",nms_var)
#   mapping_list_num <- c("No",nms_var)
#   mapping_list_fact <- c("No",nms_fact)
#   mapping_list_all <- c("No",var_names)
    facet_list_factors <- c(".",var_names)
#   
  # updateSelectInput(session, "facet_row", choices = facet_list_factors)
  # updateSelectInput(session, "facet_col", choices = facet_list_factors)
  updateSelectInput(session, "filter_column", choices = varx_list)
})

########### Get the list of factors from a variable ############

observeEvent(input$filter_column != '-', {
  
  filter_column <- input$filter_column
  
  if (filter_column == "-") {filter_column <- NULL}
  
  koos <- df_tidy_design() %>% select(for_filtering = !!filter_column)
  
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
    write.csv(df_combined(), file, row.names = FALSE)
  }
)



######## PREPARE PLOT FOR DISPLAY ##########
plotdata <-  reactive({
  
  df <- df_combined()
  # observe({print(head(df))})
  

  
  ############################ TODO ##############
  # Grid

  
  if (input$show_control == FALSE) {
    df <- df %>% filter(condition!='control')
  }
  
  # This ensures correct order when plot is rotated 90 degrees
  # if (input$rotate_plot == TRUE) {
  #   df$treatment2 <- factor(df$treatment2, levels=rev(unique(sort(df$treatment2))))
  #   # df$treatment2 <- factor(df$treatment2, levels=rev(levels(df$treatment2)))
  #   
  #   
  #       # df$control <- factor(df$control, levels=rev(unique(sort(df$control))))
  # 
  # }
  
  
  # reorder(y, desc(y)))
  
  p <-  ggplot(df, aes_string(x = input$compare, y = "`Fold Change`")) 
  
  p <- p + geom_jitter(shape = 16, width=0.2, height=0.0, cex=input$pointSize, color="black", alpha=input$alphaInput) 
  
  if (input$summaryInput !="none") {
   p <-  p + stat_summary(fun = input$summaryInput, fun.min =input$summaryInput, fun.max = input$summaryInput, geom = "errorbar", width = 0.5, size=1) 
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
  plate_plot <- plate_plot + theme_light() + labs(x = NULL, y = NULL) 
  plate_plot
  
  
})

plotdesign <- reactive({
  df <- df_tidy_design()
  
  plate_plot <- ggplot(data=df, aes(x=column,y=reorder(row, desc(row)))) +geom_point(aes_string(color="treatment1"), size=22, stroke=0,shape=15, alpha=0.4) + scale_color_manual(values=c("grey10","grey40","grey70","grey90"))
    

    
  # plate_plot <- plate_plot+geom_point(aes_string(color="treatment2", shape="treatment1"), size=6)
  
  plate_plot <- plate_plot+geom_label(aes(label=condition, fill=treatment2), alpha=0.8)
  
  plate_plot <- plate_plot + coord_fixed()+scale_x_continuous(breaks=seq(1, 12), position = "top")
  plate_plot <- plate_plot + theme_light() + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
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
  plot(plotplate())
})

output$designplot <- renderPlot(width = 700, {
  plot(plotdesign())
})


output$dataplot <- renderPlot(width = width, height = height, {
  plot(plotdata())
})

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

