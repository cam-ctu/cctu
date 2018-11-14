#### data import and manipulate
PROGNAME <- "data_import.R"
#### Author: Simon Bond, 
#### Study: RIVASE
#### Input: paths and filenames of input data
#### Output: data sets - 
#### DMC report Sep 2017
#### Date created: 26JUL017
#### Notes:




options(stringsAsFactors = FALSE)

#Hardish coding, the metadata, value for Primary Table cell B312, and LUID cell B276 was edited to
# Neutropenia (ANC <1500&#0181;l)
# this is how to get a greek mu symbol into the output correctly. 


meta <- read_excel(paste0(DATA,DATADICTIONARY),
               sheet="Primary table") %>% clean_names %>% remove_blank_rows_cols
factor_labels <- read_excel(paste0(DATA,DATADICTIONARY),
                            sheet="LUIDMaster") %>% clean_names %>% remove_blank_rows_cols

#hard coding to remove duplicate congo 

factor_labels <- subset(factor_labels, !( duplicated(label) & label=="Congo"))


meta %<>% within(
  {
    variable %<>% tolower
 
  }
)

var_names <- meta$variable


sheet_names <- excel_sheets(paste0(DATA, DATAFILE))

for( sheet in sheet_names){

  temp_data <- read_excel( paste0(DATA,DATAFILE),
                           sheet=sheet
  ) %>% clean_names %>% remove_blank_rows_cols(cols=TRUE) %>% as.data.frame
  
 
  
  for( var_name in names(temp_data)){
    format <- subset(meta, variable==var_name)$data_format
    variable <- temp_data[,var_name]
    
    #creates factors with the right labels
    if(length(format)>0 && grepl("LUID",format)){
      luid_name <- subset(meta, variable==var_name)$format_lookup
      luid <- subset(factor_labels, luid==luid_name)
      variable %<>% as.character %>% factor( levels=luid$value, labels=luid$label)
    } 
    
    #deals with booleans
    if(length(format)>0 && grepl("boolean",format)){
      variable %<>% is.na %>% ifelse(.,0,1) %>% factor(levels=0:1, labels=c("No","Yes")) 
    }
    
    #creates dates
    if( length(format)>0 && grepl("date", format)){
      date_format <- subset(meta, variable==var_name)$format_lookup
      # we have fuzzy dates with day/month=00 if not know - currently set to NA
      if(date_format!="yyyy" & !grepl("fuzzy", date_format)){
        variable %<>% strptime( format="%Y-%m-%d") %>% as.POSIXct
        
      }
    }
    
    temp_data[,var_name] <- variable
    
    
    
  }
  #only needed this time
  #names(temp_data) <- var_names$new_name[match(names(temp_data), var_names$variable)]
  
  df_names <- recode(sheet,
                     Enrolment="baseline", 
                     consentForms="consent", 
                     aes="ae",
                     rivas_co_morbidities="comorb",
                     rivas_encounters="encounters",
                     rivas_medication="medication"
                                         )
  assign(df_names, temp_data)
  
}


#-----------------------------------------------------------------#
# population data
#-----------------------------------------------------------------#

# read in table of tables csv file
meta_table <- read.csv(file = "Data\\TableOfTablesV0.1.csv", stringsAsFactors = FALSE,
                          colClass=c("Number"="character")
                          #This is needed to have values like '3.10'
                          )

#manipulate the duplication of rows within the meta_table for all the subgroups



pops <- c("Male","Female","MPA","GPA","MPA-Male","MPA-Female","GPA-Male","GPA-Female")
new_prefix <- paste0("1.",1:length(pops),".")

for( i in 1:length(pops)){
  new_rows <- subset(meta_table, Number %in% paste0("1.0.0",1:6))
  new_rows$Number %<>%gsub("1\\.0\\.",new_prefix[i], .)
  new_rows$Population <- pops[i]
  meta_table <- rbind(meta_table,new_rows)
}


index <- order_dewey(as.character(meta_table$Number))

meta_table <- meta_table[index,]

rm(pops, new_prefix, index, new_rows,i)





# import a data.frame that defines the populations by patient
# population subjid, safety, analysis, ....  most are logical variables.
#population <- read.csv(file = "Data\\population.csv", stringsAsFactors = FALSE)

# just including everyone in everything to start off with
# Check with protocol/SAP in due course


#remove the patients with extra diagnoses
# relies on the first 2 values of the diagnose lbalesbeing the key ones we want

population <- subset( baseline, select=c(patient_id, diagcode, gender))

population %<>% within(
  {
    safety <- diagcode %>% as.numeric <=2
   # full <- safety
    male <- gender=="Male"
    female <- gender=="Female"
    mpa <- (diagcode %>% as.numeric ==1)
    gpa <- (diagcode %>% as.numeric ==2)
    mpa_male <- (diagcode %>% as.numeric ==1) & gender=="Male"
    mpa_female <- (diagcode %>% as.numeric ==1) & gender=="Female"
    gpa_male <-  (diagcode %>% as.numeric ==2) & gender=="Male"
    gpa_female <- (diagcode %>% as.numeric ==2) & gender=="Female"
  }
)

population  %<>% select(-c(diagcode,gender))
meta_table %<>% within(
  Population %<>% as.character %>% tolower() %>% gsub("\\-","_",.)
)

N <- sapply(population %>% select(-patient_id), sum)

# create a mapping between column names and full names of populations
# pop_name is the column name in population data.frame
population_title <- data.frame(
  pop_name         = c("safety", #"full",
                       "male","female","mpa","gpa","mpa_male","mpa_female","gpa_male","gpa_female"),
  title            = c("Safety", #"Full Analysis",
                       "Male","Female","MPA","GPA","MPA-Male","MPA-Female","GPA-Male","GPA-Female"),
  stringsAsFactors = FALSE
)

index <- match(names(N), population_title$pop_name)

population_title %<>% within(
  {
    label=paste0( title, " (N=", N[index],")")
  }
)

rm(N, index)


#define the grouping as it will alays be needed

group <- subset( medication, issue=="Baseline", select=c(patient_id, rituxi))
group %<>% within( cohort <- (rituxi=="Yes") %>% factor( levels=c(TRUE, FALSE), 
                                                         labels=c("Rituximab","Control"))
)

create_popn_envir(c("ae","baseline","bvas_wg", "complications",
                    "consent","diagnoses","comorb",
                    "encounters","medication","tests_and_observations",
                    "vdi", "group"),
                  population,
                  subjid_string = "patient_id"
                  )                  

#-----------------------------------------------------------------#
# remove anything no longer required
#-----------------------------------------------------------------#

rm(factor_labels, luid, temp_data,date_format, df_names, format, luid_name,
   sheet_names,sheet,var_name, variable, var_names,PROGNAME, rituximabInfusions
   )

RESERVED <- c("RESERVED", ls())

