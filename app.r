# Import necessary packages
library(tidyverse)  # Data manipulation and visualization
library(janitor)    # Data cleaning
library(lubridate)  # Date manipulation
library(openxlsx)   # Read/write Excel
library(shiny)      # Web application framework
library(shinythemes)  # Shiny themes
library(WordR)      #Word doc writing
library(reporter)
library(flextable)  #Table formatting
library(ggplot2)

#library(designer)

options(shiny.maxRequestSize = 50 * 1024^2) #Set import file max to 50 MB

set_flextable_defaults(font.family = "Garamond",
                       font.size = 10)

ui <- fluidPage(
  title = "NSC Retention Insights Engine",
  theme = bslib::bs_theme(4),
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  img(src='wentworth yellow logo.png', align = "left", height = '50px'),
  br(), br(), br(),
  tabsetPanel(
    id = "tabs",
    type = "pills",
    tabPanel(
      title = "Disclaimer",
      value = "disclaimer",
      icon = icon("flag-checkered"),
    tags$p(br(),br(),
      "Welcome! This app provides analytics from National Student Clearinghouse (Subsequent Enrollment) files."),
    tags$p("For privacy reasons, we cannot accept personally identifying information from your institution. Please
      agree to the below requirements before"),
    tags$p("using our app."),
    br(),
    checkboxInput("agree", "I agree to the terms and conditions", value = FALSE)
    ),
    #),
    
    tabPanel(
      title = "Step 1: Prep Data",
      value = "description",
      icon = icon("sheet-plastic"),
      tags$p(br(),br(),
             "Open your file named DETLRPT_SE_")),
    
    tabPanel(
      title = "Step 2: Import Data",
      value = "import_nsc_data",
      icon = icon("file-import"),
      inputPanel(
      ),
      fileInput("nsc_file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      #checkboxInput("header", "Header", TRUE),
      #accept = c(".csv", ".xls",".xlsx")),
      
      tableOutput("nsc_import_summary")
    ),
    tabPanel(
      title = "Step 3: Check Data",
      value = "check_nsc_data",
      icon = icon("check")
    ),
    tabPanel(
      title = "Step 4: See Results",
      value = "see_results",
      icon = icon("chart-line"),
      
      tableOutput("nsc_results_by_inst")
    ),
    tabPanel(
      title = "Step 5: Download Results",
      value = "download_results",
      icon = icon("download"),
      downloadButton('download',"Download the data"),
      downloadButton('download_report','Download the report')
    )
  ))

server <- function(input, output, session) {
  
  #Proceed if user agrees to disclaimer
  reactive({observeEvent(input$agree, {
    if(input$agree == TRUE) {
      updateTabsetPanel(session, "tabs", disabled = NULL)
    } else {
      updateTabsetPanel(session, "tabs", disabled = c("import_nsc_data", "check_nsc_data", "see_results", "download_results"))
}
})})
  
  #Import userdata
  data <- reactive({
    req(input$nsc_file)
    readr::read_csv(input$nsc_file$datapath)
  })

  # # Disable tabs if disclaimer is not agreed to
  # observeEvent(input$agree, {
  #   if("Agree" %in% input$agree) {
  #     #updateTabsetPanel(session, "tabs", disabled = NULL)
  #   } else {
  #     updateTabsetPanel(session, "tabs", disabled = c("import_nsc_data", "check_nsc_data", "see_results", "download_results"))
  #   }
  # },ignoreNULL = TRUE)
  # 
   output$nsc_import_summary <- renderTable({
     head(data())
   })
  
  df <- reactive({
    df <- data()
    
    #Clean column names
    df <- clean_names(df)
    
    #Create lists to include data manipulation, warning, etc. notes
    df_basic_stats <- list()
    df_cleaning <- list()
    
    df_list <- list(df_basic_stats, df_cleaning)
    
    #Get stats on dataframe size
    df_basic_stats <- append(df_basic_stats, paste0("Your imported file contained ",nrow(df)," rows, ", ncol(df), " columns, and ",
                                                    length(unique(df$requester_return_field)), " unique students."))
    
    
    #Validate necessary columns
    good_cols <- sum(c("requester_return_field","search_date","enrollment_begin","enrollment_status","college_code_branch","college_name","x2_year_4_year") 
                     %in% colnames(df) == TRUE)
    
    df_cleaning <- append(df_cleaning, ifelse(good_cols == 7, "Your dataset has all of the necessary columns needed for analysis.",
                                              "Your dataset is missing or has misspelled a necessary column for these analyses."))
    
    print(df_basic_stats)
    print(df_cleaning)
    
    #Select relevant columns only
    df <- df %>%
      filter(record_found_y_n == "Y") %>% #Only keep students whose records were found
      select(requester_return_field, #W Number of student
             search_date, #Beginning search datae
             enrollment_begin, #Date student was enrolled at school
             enrollment_status, #E.g. full-time
             enrollment_major_1,
             college_code_branch,
             college_name,
             x2_year_4_year)
    
    #Remove duplicate rows
    df <- unique(df)
    
    
    #Remove rows with "L" for 2-year/4-year designation
    #Very rare
    df <- df %>%
      filter(x2_year_4_year != "L")
    
    #Remove 2-year colleges
    #(For ease in data analysis)
    df <- df %>%
      filter(x2_year_4_year == "4")
    
    df <- df %>%
      select(-x2_year_4_year)
    
    #Convert enrollment date to better format for analysis
    year <- substr(df$enrollment_begin, 1, 4)
    month <- substr(df$enrollment_begin, 5, 6)
    day <- substr(df$enrollment_begin, 7, 8)
    
    df$enrollment_begin <- paste0(month,"-",day,"-",year)
    df$enrollment_begin <- mdy(df$enrollment_begin)
    
    #Add date information re: academic terms 
    a <- openxlsx::read.xlsx("supplemental_files/Codebook.xlsx",
                             sheet = "Academic Terms - By Day")
    
    a <- clean_names(a)
    
    a <- a %>%
      select(-term_by_day)
    
    a$date_by_day <- mdy(a$date_by_day)
    
    colnames(a) <- c("date","term_nsc")
    
    df <- merge(df,a,
                by.x = "enrollment_begin",
                by.y = "date",
                all.x = TRUE)
    
    #Remove data with no dates (It's a small amount.)
    df <- df %>%
      filter(is.na(enrollment_begin)==FALSE)
    
    a <- openxlsx::read.xlsx("supplemental_files/Codebook.xlsx",
                             sheet = "Academic Terms")
    
    a <- clean_names(a)
    
    a <- a %>%
      select(course_term_desc, #Term e.g. "Fall 2020"
             term_sequence)
    
    df <- merge(df,a,
                by.x = "term_nsc",
                by.y = "course_term_desc",
                all.x = TRUE)
    
    #Remove academic term dataframe
    rm(a)
    
    df <- df %>%
      rename(nsc_term_sequence = term_sequence)
    
    #Remove records with missing schools
    #(This should be very rare.)
    df <- df %>%
      filter(is.na(college_code_branch) == FALSE)
    
    #Data cleaning: If student has 2+ records in term from same school, only keep most relevant one
    df$enroll_hierarchy <- ifelse(df$enrollment_status == "W",1, #Withdraw from institution
                                  ifelse(df$enrollment_status == "F", 2, #Full-Time
                                         ifelse(df$enrollment_status == "Q", 3, #3/4 time
                                                ifelse(df$enrollment_status == "H", 4, #1/2 time
                                                       ifelse(df$enrollment_status == "L", 5, #Less than 1/2 time
                                                              ifelse(df$enrollment_status == "A", 6, #Approved leave of absence
                                                                     ifelse(df$enrollment_status == "D", 7, #Deceased
                                                                            ifelse(df$enrollment_status == "",8, #No status
                                                                                   9))))))))
    df <- df %>%
      group_by(requester_return_field, #E.g. Jane Doe, Fall 2015, Worcester Polytechnic Institute
               nsc_term_sequence,
               college_code_branch) %>%
      mutate(top_enroll_status = min(enroll_hierarchy)) %>% #Find most important enrollment status for each student
      filter(enroll_hierarchy == top_enroll_status) %>% #Keep only row(s) with most important enrollment status for each student
      ungroup()
    
    df <- df %>%
      select(-enroll_hierarchy)
    
    #There are still 2+ records for certain students because some schools report the same status multiple times in the same term
    #Keep only one row per student per school (per term!)
    
    df <- df %>%
      #arrange(asc(enrollment_begin)) %>% #Put earliest records at top
      group_by(requester_return_field, #E.g. Jane Doe, Fall 2015, Worcester Polytechnic Institute
               nsc_term_sequence,
               college_code_branch) %>%
      filter(row_number() == 1) #keep only first row of each student

    #Calculate how many schools student attended in same term
    df <- df %>%
      group_by(requester_return_field,
               nsc_term_sequence) %>%
      mutate(colleges_in_term = n_distinct(college_code_branch)) %>% ungroup()
    
    #Calculate students' 1st term in records
    df <- df %>%
      group_by(requester_return_field) %>%
      mutate(start_term_sequence = min(nsc_term_sequence)) %>%
      ungroup()
    print ("checkpoint 1")
    
    print(colnames(df))
    
    
    #Calculate students' 1st college attended in records
    n <- df %>%
      select(requester_return_field, start_term_sequence, nsc_term_sequence, college_code_branch) %>%
      filter(start_term_sequence == nsc_term_sequence) %>% #Keep only student's first term in college
      rename(nsc_first_college_attended = college_code_branch) %>%
      select(-start_term_sequence, -nsc_term_sequence, -start_term_sequence) %>%
      unique()
    
    print("checkpoint 0") 
    
    print("checkpoint 1")
    
    #list(df1 = df, df2 = df_inst)
    print("checkpoint 2")
    
    #Add to main data
    df <- merge(df, n,
                by = "requester_return_field",
                all.x = TRUE)
    
    rm(n)
    
    #Indicate whether student attended college during specific term indicating fall-to-fall retention
    df$fall_to_fall <- ifelse(df$nsc_term_sequence == df$start_term_sequence + 3 & #If term is next fall...
                                df$college_code_branch == df$nsc_first_college_attended & #...and they're still at same school...
                                df$enrollment_status != "W", #...and they're not recorded as Withdraw, then count them as retained.
                              1, 0)
    
    #Indicate whether student persisted in college, but at DIFFERENT college than first one
    df$fall_to_fall_other_school <- ifelse(df$nsc_term_sequence == df$start_term_sequence + 3 & #If term is next fall...
                                             df$college_code_branch != df$nsc_first_college_attended & #...and they're NOT at same school...
                                             df$enrollment_status != "W", #...and they're not recorded as Withdraw, then count them as retained.
                                           1, 0)
    
    print("checkpoint 2.0")
    
    return(df)
  })
  
  # output$nsc_results_by_inst <- renderTable({
  #   #Create new dataframe of data so we can manipulate it
  #   
  #   
  #   df()[1:3,]
  #   
  #   #nsc_data <- reactive({n})
  #   
  # 
  # })
  # print("checkpoint 3")
  

  df_inst <- reactive({
    
    df() %>%
      group_by(requester_return_field) %>%
      summarise(nsc_fall_to_fall = max(fall_to_fall),
                nsc_fall_to_fall_other_school = max(fall_to_fall_other_school),
                nsc_first_college_attended = nsc_first_college_attended) %>%
      group_by(nsc_first_college_attended) %>%
      summarise(nsc_fall_to_fall = scales::percent(mean(nsc_fall_to_fall,na.rm = TRUE),accuracy = .1),
                nsc_fall_to_fall_other_school = scales::percent(mean(nsc_fall_to_fall_other_school,na.rm = TRUE),accuracy = .1),
                students = n_distinct(requester_return_field)) %>%
      filter(students >= 100) %>%
      arrange(desc(students)) 
    
    
  })


  
  
  #nsc_data <- reactive({df})
  
  #nsc_data <- reactive({return(data.frame(n))})
  
      
      output$download <- downloadHandler(
        filename = function() {
          paste("n-", "1", ".xlsx", sep="")
        },
        content = function(file) {
          #openxlsx::write.xlsx(df(), file)
          g= openxlsx::createWorkbook("file")
          openxlsx::addWorksheet(g,"Hoja1")
          openxlsx::writeData(g,"Hoja1",df())
          
          openxlsx::addWorksheet(g,"Hoja2")
          openxlsx::writeData(g,"Hoja2",df()[1:3,])
          
          
          openxlsx::saveWorkbook(g,file)
        }
      )
      
      
      
      #Generate Word report
      output$download_report <- downloadHandler(
        filename = function() {
          "results - new.docx"
        },
        content = function(file) {
          #z <- df()[1:3,]
          z <- df_inst()
          t1 <- flextable(z,cwidth = 1.5 )
          t1 <- autofit(t1)
          FT <- list(t1=t1)
          Plots <- list(mtcars1 = function() print(ggplot(mtcars, aes(x = wt, y = hp, col = as.factor(cyl))) + geom_point()))
          
          # render docx
          renderInlineCode("supplemental_files/report_template.docx",
                           "supplemental_files/results.docx")
          
          
          body_add_flextables("supplemental_files/report_template.docx",
                              "supplemental_files/results.docx", 
                              FT)
          
          doc <- officer::read_docx("supplemental_files/results.docx")
          print(doc, target = file)

        }
      )





  
}


shinyApp(ui, server)
#designer::designApp()

