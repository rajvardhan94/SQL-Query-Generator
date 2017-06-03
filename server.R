#Setting the working directory

setwd("C:/Users/anushree.sharma/Desktop")

#loading shiny library

library(shiny)

#exceeding the maximum upload size
options(shiny.maxRequestSize=30*1024^2)


shinyServer(function(input, output)
{
  
  #Step 1: uploading the dataset
  #-------------------------------
  
  #adding dataframe through UPLOAD button
  observe({
    
    
    # your action button condition
    if(input$uploadButton > 0 && input$file1$type=="text/csv" ) {
      # update your data
      Dataframe<<-isolate(read.csv(file=input$file1$name, 
                                   header=input$header, 
                                   sep=input$sep,
                                   stringsAsFactors = F))
    }
  })
  
  # Step 2: Choosing the columns to be selected
  #--------------------------------------------
  
  # Creating Dropdown for select Statement
  
  output$choose_select_col<-renderUI({
    
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(input$file1$type!="text/csv")
      return("Please select txt/csv files only")
    
    
    #Create drop down for "select"
    selectInput("Columns","SELECT", choices = c(colnames(Dataframe)),selected="(All)",multiple = T)
  })
  
  # Step 3: Selecting the column types for "where" condition
  #---------------------------------------------------------
  
  #Selecting type of columns to be filtered (Where columns)
  
  output$choose_where_type<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(input$file1$type!="text/csv")
      return("Please select txt/csv files only")
    
    
    checkboxGroupInput(inputId="col_type",
                       label="WHERE(Select column types to be added in filter)",
                       choices=c("Partition","Date","Categorical","Numeric"),
                       selected = NULL,
                       inline = TRUE,
                       width = NULL)
  })
  
  output$choose_where_col_partition<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(is.element("Partition",input$col_type) == FALSE)
      return()
    
    
    #  Where statement dropdown
    selectInput("WherePartitionColumns",
                "Select columns that are partition column", 
                choices = c(colnames(Dataframe)),
                multiple = T)
    
    
  })
  #Selecting date columns to be filtered (Where columns)
  
  output$choose_where_col_date<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(is.element("Date",input$col_type) == FALSE)
      return()
    
    
    #  Where statement dropdown
    selectInput("WhereDateColumns",
                "Select DATE columns to be filtered", 
                choices = c(colnames(Dataframe)),
                multiple = T)
    
    
  })
  
  
  #Selecting categorical columns to be filtered (Where columns)
  
  output$choose_where_col_cat<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(is.element("Categorical",input$col_type) == FALSE)
      return()
    
    
    #  Where statement dropdown
    selectInput("WhereCatColumns",
                "Select CATEGORICAL columns to be filtered", 
                choices = c(colnames(Dataframe)),
                multiple = T)
    
  })
  
  
  #Selecting Numeric columns to be filtered (Where columns)
  
  output$choose_where_col_num<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(is.element("Numeric",input$col_type) == FALSE)
      return()
    
    
    #  Where statement dropdown
    selectInput("WhereNumColumns",
                "Select NUMERIC columns to be filtered", 
                choices = c(colnames(Dataframe)),
                multiple = T)
    
  })
  
  #Selecting Date Format for date columns  to be filtered (Where columns)
  
  output$date_format<-renderUI({
    
    if(input$uploadButton==0)
      return("Please Upload file to continue")
    
    if(is.element("Date",input$col_type) == FALSE)
      return()
    
    
    #  selecting dropdown for different date format
    selectInput("dateFormat",
                "Select DATE FORMAT of the columns", 
                choices = c("dd/mm/yyyy" = "%d/%m/%Y",
                            "dd/mm/yy" = "%d/%m/%y",
                            "mm/dd/yyyy" = "%m/%d/%Y",
                            "mm/dd/yy" = "%m/%d/%y",
                            "mm/yy"="%m/%y",
                            "mm-yy"="%m-%y",
                            "mm/yyyy"="%m/%Y",
                            "mm-yyyy"="%m-%Y",
                            "yyyy/mm/dd" = "%Y/%m/%d",
                            "yyyy/dd/mm" = "%Y/%d/%m",
                            "yy/mm/dd" = "%Y/%m/%d", 
                            "yy/dd/mm" = "%Y/%d/%m",
                            "bbb dd,yy" ="%b %d,%y",
                            "bbb dd,yyyy" ="%b %d,%Y",
                            "month's name dd,yy" ="%B %d,%y",
                            "month's name dd,yyyy" ="%B %d,%Y",
                            "dd-mm-yyyy" = "%d-%m-%Y",
                            "dd-mm-yy" = "%d-%m-%y",
                            "mm-dd-yyyy" = "%m-%d-%Y",
                            "mm-dd-yy" = "%m-%d-%y",
                            "yyyy-mm-dd" = "%Y-%m-%d",
                            "yyyy-dd-mm" = "%Y-%d-%m",
                            "yy-mm-dd" = "%Y-%m-%d", 
                            "yy-dd-mm" = "%Y-%d-%m"
                ),
                selected = "dd/mm/yyyy")
    
    
  })
  
  #Step 4: Selecting values of column that needs to be filtered
  #-------------------------------------------------------------
  
  # server UI for selected partition where columns Dropdown
  
  output$choice_where_columns_part <- renderUI({
    
    if(is.null(input$WherePartitionColumns))
      return()
    
    WherePartList<<-input$WherePartitionColumns
    p_num <<- length(WherePartList)
    
    lapply(1:p_num, function(i) {
      
      textInput(inputId =paste0("selection_part_",(i)),
                label = paste0("",WherePartList[i]),
                "Ex : BETWEEN '01-2016' AND '02-2016'")
    })
  })
  
  
  # server UI for selected Date where columns Dropdown
  
  output$choice_where_columns_date <- renderUI({
    
    if(is.null(input$WhereDateColumns))
      return()
    
    WhereDateList<<-input$WhereDateColumns
    d_num <<- length(WhereDateList)
    
    lapply(1:d_num, function(i) {
      
      
      dateRangeInput(paste0("selection_date_",(i)),
                     label = paste0("",WhereDateList[i]),
                     start = Sys.Date() - 3, end = Sys.Date() + 3,
                     separator = " to ", format = "dd/mm/yyyy",
                     startview = 'year', language = 'en', weekstart = 1)
      
      
    })
  })
  
  
  # server UI for selected categorical where columns Dropdown
  
  output$choice_where_columns_cat <- renderUI({
    
    if(is.null(input$WhereCatColumns))
      return()
    
    WhereCatList<<-input$WhereCatColumns
    c_num <<- length(WhereCatList)
    
    lapply(1:c_num, function(i) {
      
      selectInput(inputId =paste0("selection_cat_",(i)), #filter_list[i],
                  label = paste0("",WhereCatList[i]),  
                  choices = unique(Dataframe[WhereCatList[i]]), 
                  multiple = T)
    })
  })
  
  
  # server UI for selected NUmeric where columns Dropdown
  
  output$choice_where_columns_num <- renderUI({
    
    if(is.null(input$WhereNumColumns))
      return()
    
    WhereNumList<<-input$WhereNumColumns
    n_num <<- length(WhereNumList)
    
    lapply(1:n_num, function(i) {
      
      textInput(inputId =paste0("selection_num_",(i)),
                label = paste0("",WhereNumList[i]),
                "Example: >= 5")
    })
  })
  
  
  
  
  output$condi_where_columns_part <- renderUI({
    
    
    if(is.null(input$WherePartitionColumns))
      return()
    
    lapply(1:d_num, function(i) {
      if (i==d_num)
        return()
      radioButtons(paste0('condition_part_for_',(i+1)),
                   paste0('condition between \n',WherePartList[i],'\n & \n',WherePartList[i+1]),
                   choices=c('AND','OR'),
                   inline = T)
    })  
  })
  output$condi_where_columns_date <- renderUI({
    
    
    if(is.null(input$WhereDateColumns))
      return()
    
    lapply(1:d_num, function(i) {
      if (i==d_num)
        return()
      radioButtons(paste0('condition_date_for_',(i+1)),
                   paste0('condition between \n',WhereDateList[i],'\n & \n',WhereDateList[i+1]),
                   choices=c('AND','OR'),
                   inline = T)
    })  
  })
  
  
  
  
  output$condi_where_columns_cat <- renderUI({
    
    
    if(is.null(input$WhereCatColumns))
      return()
    
    lapply(1:c_num, function(i) {
      if (i==c_num)
        return()
      radioButtons(paste0('condition_cat_for_',(i+1)),
                   paste0('condition between \n',WhereCatList[i],'\n & \n',WhereCatList[i+1]),
                   choices=c('AND','OR'),
                   inline = T)
    })  
  })
  
  
  output$condi_where_columns_num <- renderUI({
    
    
    if(is.null(input$WhereNumColumns))
      return()
    
    lapply(1:n_num, function(i) {
      if (i==n_num)
        return()
      radioButtons(paste0('condition_num_for_',(i+1)),
                   paste0('condition between \n',WhereNumList[i],'\n & \n',WhereNumList[i+1]),
                   choices=c('AND','OR'),
                   inline = T)
    })  
  })
  
  #Step 5: Select the columns to be filtered
  #------------------------------------------
  
  #columns to be aggregated
  output$aggregate_columns<- renderUI({
    
    
    if(is.null(input$Columns))
      return()
    
    if(input$subsetdata == FALSE)
      return()
    
    
    
    #  aggregate columns dropdown
    selectInput("aggregateColumns",
                "Select columns to be aggregated", 
                choices = input$Columns,#c(colnames(Dataframe)),
                multiple = T)
    
  })
  
  # Step 6: Select the aggrgate function to be used
  #------------------------------------------------
  
  # server UI for selected categorical where columns Dropdown
  
  
  
  output$aggregate_type <- renderUI({
    
    if(is.null(input$aggregateColumns))
      return()
    
    agg_list<<-input$aggregateColumns
    agg_num <<- length(agg_list)
    
    lapply(1:agg_num, function(i) {
      
      selectInput(inputId =paste0("agg_type_",(i)), #filter_list[i],
                  label = paste0("",agg_list[i]),  
                  choices = c("SUM",
                              "AVG",
                              "COUNT", 
                              "MIN", 
                              "MAX",
                              "FIRST",
                              "LAST"), 
                  multiple = F)
    })
  })
  
  
  
  #MAIN Panel
  
  
  #writing the select satatement
  output$select<- renderText("SELECT")
  
  
  #displaying selected columns
  output$selected<- renderUI ({  
    
    if(input$subsetdata == TRUE)
      return()
    
    if(input$genButton==0)
      return()
    
    
    observe({
      cols<<-isolate(c(input$Columns))
      
    })
    
    paste0(cols,collapse=", \n")
  })
  
  
  #displaying selected columns
  output$selected_agg<- renderUI ({  
    
    if(input$subsetdata == FALSE)
      return()
    
    if(is.null(input$aggregateColumns))
      return("Invalid aggregate/Group by conditions")
    
    if(input$genButton==0)
      return()
    
    observe({
      cols<<-isolate(c(input$Columns))
      
      #aggrgate type selection 
      agg_type_list<<-isolate(list(input$agg_type_1,
                                   input$agg_type_2,
                                   input$agg_type_3,
                                   input$agg_type_4,
                                   input$agg_type_5,
                                   input$agg_type_6,
                                   input$agg_type_7,
                                   input$agg_type_8,
                                   input$agg_type_9,
                                   input$agg_type_10
      ))
      
      agg_statement_list<<-list()
      lapply(1:agg_num, function(i){
        agg_statement_list[i]<<-paste0(agg_type_list[[i]],"(",agg_list[i],") AS ",agg_type_list[[i]],"_of_",gsub(".*\\.","",agg_list[i]))
      })
      if(input$genButton==0)
        return()
      cols_agg<<-cols
      
      lapply(1:length(agg_list),function(i){
        cols_agg<<-replace(cols_agg,cols_agg==agg_list[i],agg_statement_list[i])
      }
      )
      
      
    })
    
    
    paste0(cols_agg,collapse=", \n")
  })
  
  #writing the FROM satatement
  output$from<- renderText("FROM")
  
  #generating filename 
  output$dataname<-reactive({gsub("\\..*","",input$file1$name)})
  
  
  #writing the WHERE satatement
  output$where<- renderText("WHERE")
  
  #generating the where filter for Partition columns
  output$Partition <- renderUI({
    
    if(input$genButton==0)
      return()
    
    if(is.element("Partition",input$col_type) == FALSE)
      return()
    
    
    #writing the WHERE satatement filter
    observe({
      selection_part_list<<-isolate(list(input$selection_part_1,
                                         input$selection_part_2,
                                         input$selection_part_3,
                                         input$selection_part_4,
                                         input$selection_part_5,
                                         input$selection_part_6,
                                         input$selection_part_7,
                                         input$selection_part_8,
                                         input$selection_part_9,
                                         input$selection_part_10
      ))
      
      condition_part_list<<-isolate(list("",
                                         input$condition_part_for_2,
                                         input$condition_part_for_3,
                                         input$condition_part_for_4,
                                         input$condition_part_for_5,
                                         input$condition_part_for_6,
                                         input$condition_part_for_7,
                                         input$condition_part_for_8,
                                         input$condition_part_for_9,
                                         input$condition_part_for_10
      ))
    })
    
    
    
    lapply(1:p_num, function(i) {
      
      renderText(paste(condition_part_list[i],
                       WherePartList[i],
                       selection_part_list[[i]],
                       sep=" "))
    })
    
  })
  
  
  
  #generating the where filter for Date columns
  output$Date <- renderUI({
    
    if(input$genButton==0)
      return()
    
    if(is.element("Date",input$col_type) == FALSE)
      return()
    
    #Condition between Partition and Date Columns 
    condition_part_date<-ifelse (is.element("Date",input$col_type) & is.element("Partition",input$col_type),"AND","")
    
    
    #writing the WHERE satatement filter
    observe({
      selection_date_list<<-isolate(list(input$selection_date_1,
                                         input$selection_date_2,
                                         input$selection_date_3,
                                         input$selection_date_4,
                                         input$selection_date_5,
                                         input$selection_date_6,
                                         input$selection_date_7,
                                         input$selection_date_8,
                                         input$selection_date_9,
                                         input$selection_date_10
      ))
      
      condition_date_list<<-isolate(list(condition_part_date,
                                         input$condition_date_for_2,
                                         input$condition_date_for_3,
                                         input$condition_date_for_4,
                                         input$condition_date_for_5,
                                         input$condition_date_for_6,
                                         input$condition_date_for_7,
                                         input$condition_date_for_8,
                                         input$condition_date_for_9,
                                         input$condition_date_for_10
      ))
    })
    
    lapply(1:d_num, function(i) {
      
      
      
      renderText({
        
        paste(condition_date_list[i],
              WhereDateList[i],
              "BETWEEN",
              paste("'",strftime(strptime(selection_date_list[[i]],"%Y-%m-%d"),input$dateFormat)," ' ",
                    collapse = " AND "),sep=" ")
        
      })
    })  
  })
  
  
  
  
  #generating the where filter for Categorical columns
  output$Cat <- renderUI({
    
    if(input$genButton==0)
      return()
    
    if(is.element("Categorical",input$col_type) == FALSE)
      return()
    
    #Condition between Categorical and Date Columns 
    condition_date_cat<-ifelse (is.element("Date",input$col_type) & is.element("Categorical",input$col_type),"AND","")
    
    #writing the WHERE satatement filter
    observe({
      selection_cat_list<<-isolate(list(input$selection_cat_1,
                                        input$selection_cat_2,
                                        input$selection_cat_3,
                                        input$selection_cat_4,
                                        input$selection_cat_5,
                                        input$selection_cat_6,
                                        input$selection_cat_7,
                                        input$selection_cat_8,
                                        input$selection_cat_9,
                                        input$selection_cat_10
      ))
      
      condition_cat_list<<-isolate(list(condition_date_cat,
                                        input$condition_cat_for_2,
                                        input$condition_cat_for_3,
                                        input$condition_cat_for_4,
                                        input$condition_cat_for_5,
                                        input$condition_cat_for_6,
                                        input$condition_cat_for_7,
                                        input$condition_cat_for_8,
                                        input$condition_cat_for_9,
                                        input$condition_cat_for_10
      ))
    })
    
    
    
    lapply(1:c_num, function(i) {
      
      renderText(paste(condition_cat_list[i],
                       WhereCatList[i],
                       ' in (',
                       paste(paste0("'",(paste0(selection_cat_list[[i]],"'",sep="")),sep=""),collapse=","),
                       ') ',sep=" "))
    })
  })
  
  #generating the where filter for Numeric columns
  output$Num <- renderUI({
    
    if(input$genButton==0)
      return()
    
    if(is.element("Numeric",input$col_type) == FALSE)
      return()
    
    #Condition between Numeric and Date Columns  
    condition_date_num<-ifelse (is.element("Numeric",input$col_type) & is.element("Categorical",input$col_type),"AND","")
    
    #writing the WHERE satatement filter
    observe({
      selection_num_list<<-isolate(list(input$selection_num_1,
                                        input$selection_num_2,
                                        input$selection_num_3,
                                        input$selection_num_4,
                                        input$selection_num_5,
                                        input$selection_num_6,
                                        input$selection_num_7,
                                        input$selection_num_8,
                                        input$selection_num_9,
                                        input$selection_num_10
      ))
      
      condition_num_list<<-isolate(list(condition_date_num,
                                        input$condition_num_for_2,
                                        input$condition_num_for_3,
                                        input$condition_num_for_4,
                                        input$condition_num_for_5,
                                        input$condition_num_for_6,
                                        input$condition_num_for_7,
                                        input$condition_num_for_8,
                                        input$condition_num_for_9,
                                        input$condition_num_for_10
      ))
    })
    
    
    
    lapply(1:n_num, function(i) {
      
      renderText(paste(condition_num_list[i],
                       WhereNumList[i],
                       selection_num_list[[i]],
                       sep=" "))
    })
    
  })
  
  #writing the Group by satatement
  output$group_by<- renderText(ifelse(input$subsetdata == FALSE,"","GROUP BY"))
  
  #writing the Group by condition satatement
  output$group_by_condi<- renderUI({
    
    if(input$genButton==0)
      return()
    
    if(is.null(input$Columns))
      return()
    
    if(input$subsetdata == FALSE)
      return()
    
    
    observe({
      groupByCol<<-setdiff(cols,agg_list)
    })
    
    
    paste(groupByCol,collapse = ",\n")
  })
  
  #writing the GO satatement
  output$go<- renderText(";")
  
  
})
