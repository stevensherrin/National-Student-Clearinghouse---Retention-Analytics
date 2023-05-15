library(tidyverse)
library(openxlsx)
library(janitor)
library(lubridate)
library(gtools)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#########################################################################
####Import National Student Clearinghouse data
#########################################################################

#Select location of NSC data file (e.g. Subsequent Enrollment)
file_path <- file.choose()

#Read csv file
n <- read.csv(file_path)

#n <- read.csv("C:/Users/sherrins/OneDrive - Wentworth Institute of Technology/Data Sources/NSC/Results from NSC/Subsequent Enrollment/Subsequent Enrollment - UG Admits (Fall 2015 to present).csv")

#Clean column names
n <- clean_names(n)

#Select relevant columns only
n <- n %>%
  filter(record_found_y_n == "Y") %>% #Only keep students whose records were found
  select(requester_return_field, #Unique identifier of student
         search_date, #Subsequent Enrollment dates requested
         enrollment_begin, #Date student was enrolled at school
         enrollment_end,
         enrollment_status, #E.g. full-time
         enrollment_major_1,
         college_code_branch,
         college_name,
         x2_year_4_year)

#Keep only 4 year colleges
#(For ease in data analysis)
n <- n %>%
  filter(x2_year_4_year == "4")

n <- n %>%
  select(-x2_year_4_year)

#Remove duplicate rows
n <- unique(n)

#Remove rows with missing enrollment dates
n <- n %>%
    filter(is.na(n$enrollment_begin) == FALSE,
           is.na(n$enrollment_end) == FALSE)

#Convert enrollment date to better format for analysis
convert_date <- function(date_string){
  year <- substr(date_string, 1, 4)
  month <- substr(date_string, 5, 6)
  day <- substr(date_string, 7, 8)
  date_string <- paste0(month,"-",day,"-",year)
  date_string <- mdy(date_string)
  return(date_string)
}

n$search_date <- convert_date(n$search_date)
n$enrollment_begin <- convert_date(n$enrollment_begin)
n$enrollment_end <- convert_date(n$enrollment_end)

#Find date in middle of beginning and end of enrollment
n$enrollment_middle <- n$enrollment_begin + ((n$enrollment_end - n$enrollment_begin) / 2)

#Remove records where enrollment started before search date
n <- n %>%
  filter(enrollment_begin >= search_date)


#Add date information re: academic terms 
a <- read.xlsx("data for code/Codebook.xlsx",
               sheet = "Academic Terms - By Day")

a <- clean_names(a)

a$date_by_day <- mdy(a$date_by_day)

colnames(a) <- c("date","term_nsc")

#Add date info to dataframe
n <- merge(n,a,
           by.x = "enrollment_begin",
           by.y = "date",
           all.x = TRUE)


a <- read.xlsx("data for code/Codebook.xlsx",
               sheet = "Academic Terms")

a <- clean_names(a)

n <- merge(n,a,
           by.x = "term_nsc",
           by.y = "course_term_desc",
           all.x = TRUE)

n <- n %>%
  rename(nsc_term_sequence = term_sequence)

rm(a)

#Remove rows with missing schools
#(This should be very rare.)
n <- n %>%
  filter(is.na(college_code_branch) == FALSE)


#######################################################################
###Calculate fall-to-fall retention
#######################################################################

#Create "enrollment status" hierarchy
#This will be important later
n$enroll_hierarchy <- ifelse(n$enrollment_status == "W",1, #Withdraw from institution
                             ifelse(n$enrollment_status == "F", 2, #Full-Time
                                    ifelse(n$enrollment_status == "Q", 3, #3/4 time
                                           ifelse(n$enrollment_status == "H", 4, #1/2 time
                                                  ifelse(n$enrollment_status == "L", 5, #Less than 1/2 time
                                                         ifelse(n$enrollment_status == "A", 6, #Approved leave of absence
                                                                ifelse(n$enrollment_status == "D", 7, #Deceased
                                                                       ifelse(n$enrollment_status == "",8, #No status
                                                                              9))))))))

#If student has 2+ records in term with DIFFERENT ENROLLMENT STATUS from SAME school, 
#only keep one row, according to "enrollment status" hierarchy.
n <- n %>%
  group_by(requester_return_field, #E.g. Jane Doe, Fall 2015, Worcester Polytechnic Institute
           nsc_term_sequence,
           college_code_branch) %>%
  mutate(top_status = min(enroll_hierarchy)) %>% #Find most important enrollment status for each student
  filter(enroll_hierarchy == top_status) %>% #Keep only row(s) with most important enrollment status for each student
  ungroup()

#Resolve instances where school reports same enrollment status
#multiple times for single student
n <- n %>%
  group_by(requester_return_field, #E.g. Jane Doe, Fall 2015, Worcester Polytechnic Institute
           nsc_term_sequence,
           college_code_branch) %>%
  filter(row_number() == 1) #keep only first row of each student

#Resolve instances where student attended more than one school in 1st term
#Let's keep only the earliest college attended in that term

#Get students' first college terms on record
n <- n %>%
  group_by(requester_return_field) %>%
  mutate(nsc_start_term = min(nsc_term_sequence, na.rm = TRUE)) %>%
  ungroup()

n1 <- n %>%
  filter(nsc_term_sequence == nsc_start_term) %>%
  group_by(requester_return_field) %>%
  arrange(enrollment_begin) %>%
  filter(row_number() == 1)

n1 <- n1 %>%
  select(requester_return_field,
         nsc_first_college_attended = college_code_branch)

#Add students' first college to main data
n <- merge(n, n1,
           by = "requester_return_field",
           all.x = TRUE)


#Indicate whether student attended college during specific term indicating fall-to-fall retention
n$fall_to_fall <- ifelse(n$nsc_term_sequence == n$nsc_start_term + 3 & #If term is next fall...
                           n$college_code_branch == n$nsc_first_college_attended & #...and they're still at same school...
                           n$enrollment_status != "W", #...and they're not recorded as Withdraw, then count them as retained.
                         1, 0)

#Indicate whether student persisted in college, but at DIFFERENT college than first one
n$fall_to_fall_other_school <- ifelse(n$nsc_term_sequence == n$nsc_start_term + 3 & #If term is next fall...
                                        n$college_code_branch != n$nsc_first_college_attended & #...and they're NOT at same school...
                                        n$enrollment_status != "W", #...and they're not recorded as Withdraw, then count them as retained.
                                      1, 0)

#Create summary dataframe with one row per student
nn <- n %>%
  group_by(requester_return_field) %>%
  summarise(nsc_fall_to_fall = max(fall_to_fall),
            nsc_fall_to_fall_other_school = max(fall_to_fall_other_school),
            #nsc_first_college_attended = nsc_first_college_attended) %>%
            nsc_first_college_attended = nsc_first_college_attended) %>%
  unique()

###############################################################
#Add university/college data (from College Scorecard)
###############################################################
#Read College Scorecard
c <- read.xlsx("data for code/college scorecard.xlsx")

#Rename column
colnames(c)[4] = "name"

#Select columns
c <- c %>%
  select(ope8_id, name, main_campus,
         location.lat, location.lon,
         carnegie_basic, carnegie_undergrad, carnegie_size_setting,
         minority_serving.historically_black, minority_serving.predominantly_black, minority_serving.hispanic,
         men_only, women_only, religious_affiliation,
         sat_scores.midpoint.math, sat_scores.midpoint.critical_reading,
         admission_rate.overall,
         demographics.race_ethnicity.white:demographics.race_ethnicity.non_resident_alien,
         part_time_share,
         tuition.in_state, tuition.out_of_state,
         instructional_expenditure_per_fte, faculty_salary,
         ft_faculty_rate,
         pell_grant_rate,
         share_firstgeneration,
         demographics.female_share,
         demographics.over_23_at_entry,
         demographics.median_family_income,
         demographics.share_white.home_ZIP,
         demographics.share_bachelors_degree_age25.home_ZIP)

#Change "NULL" to missing/na
c[c == "NULL"] <- NA 

#plot_missing(c)

# cc <- c %>%
#   group_by(ope8_id) %>% tally()

# Add a new column "na" to `c` containing the count of missing values per row
c <- c %>%
  mutate(na = rowSums(is.na(.)))

# Create dataframe with the minimum number of missing values ("fewest_nas") and the number of rows ("n") per institution (some ope8_ids have multiple institutions)
cc <- c %>%
  group_by(ope8_id) %>%
  summarise(fewest_nas = min(na),
            n = n()) 
#Merge data
c <- merge(c, cc,
           by = "ope8_id",
           all.x = TRUE)

#Keep rows with the minimum number of missing values
c <- c %>%
  filter(na == fewest_nas)

# Specify columns to be converted to factors and numeric
columns_to_factor <- c("main_campus", "carnegie_basic", "carnegie_size_setting", "carnegie_undergrad")

columns_to_numeric <- c("location.lat", "location.lon", "sat_scores.midpoint.critical_reading", "sat_scores.midpoint.math", "admission_rate.overall", 
                        "demographics.race_ethnicity.white", "demographics.race_ethnicity.black", "demographics.race_ethnicity.hispanic", 
                        "demographics.race_ethnicity.asian", "demographics.race_ethnicity.non_resident_alien", "part_time_share", "tuition.in_state", 
                        "tuition.out_of_state", "instructional_expenditure_per_fte", "faculty_salary", "ft_faculty_rate", "pell_grant_rate", 
                        "share_firstgeneration", "demographics.female_share", "demographics.median_family_income", "demographics.over_23_at_entry", 
                        "demographics.share_bachelors_degree_age25.home_ZIP", "demographics.share_white.home_ZIP")

# Convert specified columns to factors and numeric
c <- c %>% 
  mutate(across(columns_to_factor, as.factor),
         across(columns_to_numeric, as.numeric))

# Merge with main data
nn <- merge(nn, c,
            by.x = "nsc_first_college_attended",
            by.y = "ope8_id",
            all.x = TRUE)

# Export data
write.xlsx(nn, "results/nsc and college scorecard.xlsx")

