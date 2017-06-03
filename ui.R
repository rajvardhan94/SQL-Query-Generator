#Loading the Shiny and ShinyDasboard library
library(shiny)
install.packages("shinythemes")
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("slate"),#selecting the theme of shinyapp
                  
                  #defining the title of the utility
                  titlePanel(strong("SQL Code generator")),
                  
                  #defining the sidebar layout
                  sidebarLayout( position = "right",
                                 sidebarPanel( 
                                   tabsetPanel(
                                     tabPanel("File select",   
                                              
                                              
                                              #selecting the CSV file from which tha dataframe has to be loaded
                                              fileInput('file1', 'FROM   (Choose CSV File)',
                                                        accept=c('text/csv', 
                                                                 'text/comma-separated-values,text/plain', 
                                                                 '.csv')),
                                              
                                              #Checkbox for including Header of the uploaded file
                                              checkboxInput('header', 'Header', TRUE),
                                              
                                              #Specifying the type of seprator used in the file that is to be Uploaded
                                              radioButtons('sep', 'Separator',
                                                           c(Comma=',',
                                                             Semicolon=';',
                                                             Tab='\t'),
                                                           inline = T),
                                              
                                              #The button to upload the file and initiate the SELECT and WHERE statements
                                              actionButton("uploadButton", "UPLOAD")
                                     ),
                                     
                                     tabPanel("Select/ Where columns", 
                                              
                                              #Giving the Dropdown to Select columns for the SELECT statement
                                              uiOutput("choose_select_col"),
                                              
                                              br(),
                                              
                                              #Dropdown for selecting types of where Columns
                                              uiOutput("choose_where_type"),
                                              
                                              br(),
                                              
                                              #Dropdown for selecting columns for Partition type filters
                                              uiOutput("choose_where_col_partition"),
                                              
                                              #Dropdown for selecting columns for Date type filters
                                              uiOutput("choose_where_col_date"),
                                              
                                              #Dropdown for selecting columns for Categorical type filters      
                                              uiOutput("choose_where_col_cat"),
                                              
                                              #Dropdown for selecting columns for Numeric type filters      
                                              uiOutput("choose_where_col_num"),
                                              
                                              #Dropdown for selecting format to be displayed for date columns(date format in the datasets)
                                              uiOutput("date_format")
                                              
                                     ),
                                     
                                     
                                     tabPanel("Where Codition",
                                              
                                              #Dropdown for selcting date type column's filter range
                                              uiOutput("choice_where_columns_part"),
                                              
                                              uiOutput("choice_where_columns_date"),
                                              
                                              #Dropdown for selcting Categorical type column's filter values    
                                              uiOutput("choice_where_columns_cat"),
                                              
                                              #Dropdown for selcting numeric type column's filter condition      
                                              uiOutput("choice_where_columns_num"),
                                              
                                              #Condition(AND/OR) between Date Columns      
                                              uiOutput("condi_where_columns_date"),
                                              
                                              #Condition(AND/OR) between  categorical Columns      
                                              uiOutput("condi_where_columns_cat"),
                                              
                                              #Condition(AND/OR) between Numeric Columns      
                                              uiOutput("condi_where_columns_num")
                                              
                                              
                                              
                                     ),
                                     
                                     tabPanel("Aggregate",   
                                              
                                              #SUbsetiing option
                                              checkboxInput('subsetdata', 'Subset Data ?', FALSE),
                                              
                                              #Condition(AND/OR) between Numeric Columns      
                                              uiOutput("aggregate_columns"),
                                              
                                              br(),
                                              
                                              #Condition(AND/OR) between Numeric Columns      
                                              uiOutput("aggregate_type"),
                                              
                                              br(),
                                              
                                              #Button to  generate code    
                                              actionButton("genButton", "DOUBLE CLICK TO GENERATE CODE")
                                     )
                                   )
                                 ),
                                 
                                 mainPanel(
                                   h3("Your Code will appear here",align="centre"),
                                   
                                   #printing the select statement
                                   textOutput("select"),
                                   
                                   #printing the selected columns to be printed
                                   uiOutput("selected"),
                                   
                                   #printing the selected columns to be printed
                                   uiOutput("selected_agg"),
                                   
                                   
                                   #printing the FROM statement
                                   textOutput("from"),
                                   
                                   #printing the FROM statement
                                   textOutput("dataname"),
                                   
                                   #printing the WHERE statement
                                   textOutput("where"),
                                   
                                   #Printing the columns with Patition filter
                                   uiOutput("Partition"),
                                   
                                   #Printing the columns with Date filter
                                   uiOutput("Date"),
                                   
                                   #Printing the columns with categorical filter
                                   uiOutput("Cat"),
                                   
                                   #Printing the column with Numeric filter
                                   uiOutput("Num"),
                                   
                                   #printing the group by statement
                                   textOutput("group_by"),
                                   
                                   #Printing the group by column 
                                   uiOutput("group_by_condi"),
                                   
                                   #printing the go statement
                                   textOutput("go")
                                 )
                  )
)
)

