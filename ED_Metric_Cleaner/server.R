#
# This is the server logic of a Shiny app that will upload a Ed metrics file and clean it and then write to a separate google sheet to aggregate full TC ED numbers.
#

library (shiny)
library (readxl)
library (dplyr)
library (tibble)
library (tidyr)
library (stringr)
library (readr)
library (lubridate)
library (tidyverse)
library (googlesheets4)
library (gargle)
library (googledrive)

# Constants ----------------------------------

#Change this if Fabienne is running the program
email_user <- "t.belanger@totalcare.us"

gs4_auth(
  cache = ".secrets",
  email = email_user
)

drive_auth(
  cache = ".secrets",
  email = email_user
)

drive_names <- gs4_find()

# Define Inputs/Outputs --------------------------------------

epd_sheet_id <- drive_names$id[which(drive_names$name == 'EPD Data')]
old_epd_data <- read_sheet(epd_sheet_id, .name_repair = "universal")

fac_list_id <- drive_names$id[which(drive_names$name == 'TC Facility List')]
fac_list <- read_sheet(fac_list_id, .name_repair = "universal")

# Functions ------------------------------------------------------

#This function will process the uploaded data. It will not require the initial data read from the database.

download_cleaner <- function(temp_download) {
  
  #This part joins the facilities list to the data
  
  temp_download <- temp_download %>%
    rename(FACNO = FacNo) %>%
    mutate(FACNO = as.numeric(FACNO)) %>%
    left_join(fac_list) %>%
    select(-FACNO)
  
  # Rename Columns  ---------------------------------------------------------
  
  temp_download <- temp_download %>%
    rename(Gender = GEN) %>%
    rename(Diagnosis = FINAL.IMPRESSION) %>%
    rename(Insurance.Name = Pri.Name) %>%
    rename(STO.End = STO...End) %>%
    rename(STO.Start = STO...Start)
  
  
  
  
  # COVID Detection column ---------------------------------------------------------
  
  temp_download <- temp_download %>%
    mutate(COVID = 
             ifelse(is.na(CHIEF.COMPLAINT), FALSE, str_detect(CHIEF.COMPLAINT, "COVID|ovid")) 
           | ifelse(is.na(DISCHARGE.INSTRUCTIONS), FALSE, str_detect(DISCHARGE.INSTRUCTIONS, "COVID")) 
           | ifelse(is.na(Diagnosis), FALSE, str_detect(Diagnosis, "COVID|CORONA"))) %>%
    select(-DISCHARGE.INSTRUCTIONS)
  
  # Create Column for STO ---------------------------------------------------
  
  #Next we will create a column for STO
  temp_download <- mutate(temp_download, STO = !is.na(STO.Start) | !is.na(STO.End))
  
  # Time Column -------------------------------------------------------------
  
  #Now we will start to clean the times
  temp_download <- temp_download %>%
    mutate(DOS = mdy(DOS)) %>%
    mutate(ARR.Year = year(DOS)) %>%
    select(-DOS)
  
  #The arrival and depart times do not include the year so we will have to add this in since some of the arrivals/departs may cross days, months, or years
  
  #For each column that needs cleaning, we will separate it into a date and a time and then join the date to the year then rejoin the time to create a datetime. If the arrival time occurs AFTER any of the other times, we will add a year to the datetime.
  
  temp_download <- temp_download %>%
    rowwise() %>%
    mutate(ARR.STR = str_c(str_split_fixed(ARRIVAL.DT.TM, " ", n = 2)[1], '/', as.character(ARR.Year), " ", str_split_fixed(ARRIVAL.DT.TM, " ", n = 2)[2])) %>%
    mutate(ARRIVE = mdy_hm(ARR.STR)) %>%
    mutate(DEP.STR = str_c(str_split_fixed(DISCHARGE.DT.TM, " ", n = 2)[1], '/', as.character(ARR.Year), " ", str_split_fixed(DISCHARGE.DT.TM, " ", n = 2)[2])) %>%
    mutate(DEPART = mdy_hm(DEP.STR)) %>%
    mutate(STO.S.STR = str_c(str_split_fixed(STO.Start, " ", n = 2)[1], '/', as.character(ARR.Year), " ", str_split_fixed(STO.Start, " ", n = 2)[2])) %>%
    mutate(STO.Start = mdy_hm(STO.S.STR)) %>%
    mutate(STO.E.STR = str_c(str_split_fixed(STO.End, " ", n = 2)[1], '/', as.character(ARR.Year), " ", str_split_fixed(STO.End, " ", n = 2)[2])) %>%
    mutate(STO.End = mdy_hm(STO.E.STR))
  
  
  temp_download <- temp_download %>%
    select(-DEP.STR) %>%
    select(-DISCHARGE.DT.TM) %>%
    select(-ARR.STR) %>%
    select(-ARRIVAL.DT.TM) %>%
    select(-STO.S.STR) %>%
    select(-STO.E.STR) %>%
    select(-ARR.Year) %>%
    select(-Dischar..To)
  
  
  for (i in 1:nrow(temp_download)) {
    if(!is.na(temp_download[[i,'ARRIVE']]) & !is.na(temp_download[[i,'DEPART']])) {
      if(temp_download[[i,'ARRIVE']] > temp_download[[i,'DEPART']]) {
        temp_download[[i,'DEPART']] <- temp_download[[i,'DEPART']] + years(1)
      }
    }
    if(!is.na(temp_download[[i,'ARRIVE']]) & !is.na(temp_download[[i,'STO.Start']])) {
      if(temp_download[[i,'ARRIVE']] > temp_download[[i,'STO.Start']]) {
        temp_download[[i,'STO.Start']] <- temp_download[[i,'STO.Start']] + years(1)
      }
    }
    if(!is.na(temp_download[[i,'ARRIVE']]) & !is.na(temp_download[[i,'STO.End']])) {
      if(temp_download[[i,'ARRIVE']] > temp_download[[i,'STO.End']]) {
        temp_download[[i,'STO.End']] <- temp_download[[i,'STO.End']] + years(1)
      }
    }
  }
  
  
  # Age Column --------------------------------------------------------------
  
  #Now we clean the age
  
  age_vec <- vector()
  
  for (i in 1:nrow(temp_download)) {
    temp <- str_split_fixed(temp_download[[i, 'AGE']], " ", n = 2)
    if(temp[2] == 'Days') {
      temp_age <- as.numeric(temp[1])/365
    } else {
      temp_age <- as.numeric(temp[1])
    }
    age_vec <- c(age_vec, temp_age)
  }
  
  temp_download <- cbind(temp_download, age_vec)
  
  temp_download <- rename(temp_download, Patient.Age = age_vec)
  
  temp_download <- select(temp_download, -AGE)
  
  # New Column LOS ----------------------------------------------------------
  
  #Now we compute LOS
  temp_download <- temp_download %>%
    mutate(LOS = as.numeric(DEPART - ARRIVE))
  
  # New Full Address Column  ------------------------------------------------
  
  #Now we create a full address so that Google can recognize this
  temp_download <- mutate(temp_download, Full.Address = str_c(ADDRESS, City.ST, sep = ", "))
  temp_download <- select(temp_download, -ADDRESS)
  temp_download <- select(temp_download, -City.ST)
  
  
  # Create New Insured Column -----------------------------------------------
  temp_download <- temp_download %>%
    mutate(Insured = ifelse(!is.na(Insurance.Name), 
                            ifelse((str_detect(toupper(Insurance.Name), 'SELF'))|(str_detect(toupper(Insurance.Name), 'SELP')), FALSE, TRUE), 
                            FALSE))
  
  return(temp_download)
}


#This following function will now use old data and the new data to obtain a few important data points.

new_and_old_data_joiner <- function(old_data = tibble(), new_data) {
  
  #The first question is if the new data set brings old data to the set. If it does, then this will require checking the more recent data, as well. For a totally new dataset, we only need to review the added data.
  
  total_new <- new_data
  
  if(nrow(old_data) > 0) {
    
    max_old_date <- max(old_data$ARRIVE)
    
    new_data <- filter(new_data, !(HAN %in% old_data$HAN))
    
    if(min(new_data$ARRIVE) < max_old_date) {
      
      total_new <- filter(bind_rows(old_data, new_data), ARRIVE >= min(new_data$ARRIVE))
      total_new <- total_new %>%
        select(-First.Visit) %>%
        select(-Rpt.From.Same.Address) %>%
        select(-Rpt.Visit.Time)
      
      unadjusted_data <- filter(old_data, ARRIVE < min(new_data$ARRIVE))
      
    }
    
  } else {
    
    unadjusted_data <- old_data
    total_new <- filter(total_new, !(HAN %in% unadjusted_data$HAN))
    
  }
  
  
  #Now we will need to call functions on this new dataset to determine new features. The following features will be determined: if this is the patient's first visit, if this is the patient's first visit BUT there has been a previous visit from a same address on a different day, and, if this is a repeat visit, how long ago their last visit was.
  
  total_new <- new_visit_finder(total_new = total_new, older_visits = unadjusted_data)
  total_new <- same_address_different_day(total_new = total_new, older_visits = unadjusted_data)
  total_new <- time_between_visits(total_new = total_new, older_data = unadjusted_data)
  total_new$Rpt.Visit.Time <- as.numeric(total_new$Rpt.Visit.Time)
  
  full_data <- bind_rows(unadjusted_data, total_new)
  full_data <- arrange(full_data, ARRIVE)
  
  return(full_data)
  
}

#First, to find new visits, we will reduce the dataset to only those visits that use a non-unique MRN. All other visits are first visits by default.
#Next, we will pick out only the first visits for each of these people with duplicate visits - all other visits will be flagged as repeats.

new_visit_finder <- function(total_new, older_visits) {
  
  if(nrow(older_visits) == 0) {
    older_visits <- tibble(MRN = NA)
  }
  
  min_visits <- total_new %>%
    group_by(MRN) %>%
    summarize(Min.ARRIVE = min(ARRIVE)) %>%
    filter(!(MRN %in% older_visits$MRN)) %>%
    mutate(First.Visit = TRUE) %>%
    rename(ARRIVE = Min.ARRIVE)
  
  total_new <- total_new %>%
    left_join(min_visits) %>%
    mutate(First.Visit = ifelse(is.na(First.Visit), FALSE, TRUE))
  
  return(total_new)
  
}


#We will use a similar technique to determine if there has been a previous visit from the same address on a different date.
#First, if the address is unique OR if it is the first time that this address has been used, this patient does not meet the criteria.

same_address_different_day <- function(total_new, older_visits = tibble()) {
  
  if(nrow(older_visits) == 0) {
    
    min_visits <- total_new %>%
      group_by(Full.Address) %>%
      summarize(Min.ARRIVE = min(ARRIVE)) %>%
      mutate(Rpt.From.Same.Address = FALSE) %>%
      rename(ARRIVE = Min.ARRIVE)
    
    total_new <- left_join(total_new, min_visits)
    
    rpt_addresses <- filter(total_new, Full.Address %in% filter(total_new, is.na(Rpt.From.Same.Address))$Full.Address)
    rpt_addresses <- arrange(rpt_addresses, ARRIVE)
    
    for (i in 1:nrow(rpt_addresses)) {
      if(is.na(rpt_addresses[[i,'Full.Address']])) {
        total_new[[which(total_new$HAN == rpt_addresses[[i,'HAN']]),'Rpt.From.Same.Address']] <- FALSE
      } else {
        if(is.na(rpt_addresses[[i,'Rpt.From.Same.Address']])) {
          temp_frame <- filter(rpt_addresses, Full.Address == rpt_addresses[[i, 'Full.Address']])
          temp_frame <- arrange(temp_frame, ARRIVE)
          if(which(temp_frame$HAN == rpt_addresses[[i,'HAN']]) == 1) {
            total_new[[which(total_new$HAN == rpt_addresses[[i, 'HAN']]), 'Rpt.From.Same.Address']] <- FALSE
          } else if((temp_frame[[which(temp_frame$HAN == rpt_addresses[[i,'HAN']]),'ARRIVE']] - temp_frame[[(which(temp_frame$HAN == rpt_addresses[[i,'HAN']])-1),'ARRIVE']]) > hours(2)) {
            total_new[[which(total_new$HAN == rpt_addresses[[i, 'HAN']]), 'Rpt.From.Same.Address']] <- TRUE
          } else {
            total_new[[which(total_new$HAN == rpt_addresses[[i, 'HAN']]), 'Rpt.From.Same.Address']] <- FALSE
          }
        }
      }
      
      
    }
  } else {
    
    min_visits <- total_new %>%
      group_by(Full.Address) %>%
      summarize(Min.ARRIVE = min(ARRIVE)) %>%
      mutate(Rpt.From.Same.Address = ifelse(Full.Address %in% older_visits$Full.Address, TRUE, FALSE)) %>%
      rename(ARRIVE = Min.ARRIVE)
    
    total_new <- left_join(total_new, min_visits)
    total_full <- bind_rows(filter(older_visits, !(HAN %in% total_new$HAN)), total_new)
    
    rpt_addresses <- filter(total_full, Full.Address %in% filter(total_new, is.na(Rpt.From.Same.Address))$Full.Address)
    rpt_addresses <- arrange(rpt_addresses, ARRIVE)
    
    #This loop takes any NA to rpt.from.same.address and checks to see if this visit is >3h from the previous visit from this address. If so, then this value becomes true.
    
    for (i in 1:nrow(total_new)) {
      if(is.na(total_new[[i,'Rpt.From.Same.Address']])) {
        if(is.na(total_new[[i,'Full.Address']])) {
          total_new[[i,'Rpt.From.Same.Address']] <- FALSE
        } else {
          temp_frame <- filter(rpt_addresses, Full.Address == total_new[[i, 'Full.Address']])
          temp_frame <- arrange(temp_frame, ARRIVE)
          
          if(which(temp_frame$HAN == total_new[[i,'HAN']]) == 1) {
            total_new[[i, 'Rpt.From.Same.Address']] <- FALSE
          } else if((temp_frame[[which(temp_frame$HAN == total_new[[i,'HAN']]),'ARRIVE']] - temp_frame[[(which(temp_frame$HAN == total_new[[i,'HAN']])-1),'ARRIVE']]) > hours(2)) {
            total_new[[i, 'Rpt.From.Same.Address']] <- TRUE
          } else {
            total_new[[i, 'Rpt.From.Same.Address']] <- FALSE
          }
          
        }
      }
    }
    
  }
  
  return(total_new)
  
}


#This function will look only at repeat visits. For these, it will determine how long ago their last visit was.

time_between_visits <- function(total_new, older_data = tibble()) {
  
  if(nrow(older_data) == 0) {
    
    full_data <- total_new
    
  } else {
    
    older_data <- filter(older_data, MRN %in% filter(total_new, !First.Visit)$MRN)
    full_data <- bind_rows(older_data, total_new)
    
  }
  
  total_new <- mutate(total_new, Rpt.Visit.Time = 0)
  
  for (i in 1:nrow(total_new)) {
    if(total_new[[i,'First.Visit']]) {
      total_new[[i,'Rpt.Visit.Time']] <- NA
    } else {
      temp_frame <- filter(full_data, MRN == total_new[[i, 'MRN']])
      temp_frame <- arrange(temp_frame, ARRIVE)
      if(which(temp_frame$HAN == total_new[[i,'HAN']]) == 1) {
        total_new[[i, 'Rpt.Visit.Time']] <- NA
      } else {
        total_new[[i, 'Rpt.Visit.Time']] <- as.numeric(temp_frame[[which(temp_frame$HAN == total_new[[i,'HAN']]),'ARRIVE']] - temp_frame[[(which(temp_frame$HAN == total_new[[i,'HAN']])-1),'ARRIVE']])
      }
    }
  }
  
  return(total_new)
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  v <- reactiveValues()
  
  v$new_count <- 0
  v$total_records <- nrow(old_epd_data)
  
  #This part will operate EPD upload
  
  observeEvent(input$epd_upload, {
    new_data <- read_xlsx(input$epd_upload$datapath, .name_repair = "universal")
    
    new_data <- filter(new_data, !(HAN %in% old_epd_data$HAN))
    
    new_data <- download_cleaner(new_data)
    
    Data <- new_and_old_data_joiner(new_data = new_data, old_data = old_epd_data)
    
    #The data is now joined and cleaned - the rest is communicating this to the user.
    
    range_clear(ss = epd_sheet_id)
    range_write(ss = epd_sheet_id, data = Data)
    
    v$new_count <- nrow(new_data)
    v$total_records <- nrow(Data)
    
    shinyjs::hide(id = "data_input_panel", anim = TRUE)
    shinyjs::show(id = "new_rows_number", anim = TRUE)
    
  })
  
  output$new_row_output <- renderText({
    str_c("Number of new records added: ", as.character((v$new_count)))
  })
  
  output$initial_count <- renderText({
    str_c("Total records logged: ", as.character(v$total_records))
  })

})
