#-----------------------------------------------#
# Risk of bias in prediction studies
# 1. Loading and preparing of Google Sheets data
# Roemer J. Janse - 2022/11/04
#-----------------------------------------------#

# 0. Set-up ----
# Load packages
pacman::p_load("dplyr",          # Data manipulation
               "readxl",         # Read Excel sheets
               "writexl",        # Write Excel sheets
               "labelled"        # Add labels to data frame
)

# Set working directionary
setwd("C:/Users/rjjanse/Onedrive - LUMC/Research/Collaborations/Liselotte Langenhuijsen/")

# 1. Main data ----
# Load data
dat.main <- read_excel("sheets/PROBAST DEF.xlsx")

# Give data new names 
colnames(dat.main) <- c("id", "author", "year", "title", "doi", "link", "topic", "extr_data", "reason_no_extr_data", "outcome", 
                        "population", "type", "dom", "sq", "notes_paper", "extr_level", "n_dev_dom", "n_val_dom", "n_dev_sq", "n_val_sq", "rob", "appl",
                        "notes_data", "corr_author_email", "dt_email", "notes_email", "reaction", "added_level", "available_level")

# Give data labels based on row 2 of the data
dat.main <- set_variable_labels(dat.main, .labels = filter(dat.main, row_number() == 2) %>% t())

# Remove first two rows and drop unnecessary columns
dat.main <- dat.main %>%
    # Drop first two rows
    filter(row_number() > 2) %>%
    # Drop link
    dplyr::select(-link)

# Clean variables
dat.main <- dat.main %>%
    # Remove rows without studies
    filter(!is.na(id) & !is.na(author) & !is.na(title)) %>%
    # Start manipulating variables
    mutate(id = as.integer(id),                                                                                        # Change ID to integer
           year = as.numeric(year),                                                                                    # Change year to numeric
           extr_data = ifelse(extr_data %in% c("Yes", "yes"), 1,  0),                                                  # Change to logical
           type_diag = ifelse(type %in% c("Both diagnostic and prognostic", "Diagnostic"), 1, 0),                      # Create new logical
           type_prog = ifelse(type %in% c("Both diagnostic and prognostic", "Prognostic"), 1, 0),                      # Create new logical
           type = ifelse(type == "Diagnostic", 1, ifelse(type == "Prognostic", 2, 3)),                                 # Change to numeric category
           dom = ifelse(dom %in% c("no", "No"), 0, 1),                                                                 # Change to logical
           sq = ifelse(sq %in% c("no", "No"), 0, 1),                                                                   # Change to logical
           extr_level = ifelse(extr_level == "Domain", 2, 1),                                                          # Change to numeric category
           n_dev_dom = as.numeric(n_dev_dom),                                                                          # Change to numeric
           n_dev_sq = as.numeric(n_dev_sq),                                                                            # Change to numeric
           n_val_dom = as.numeric(n_val_dom),                                                                          # Change to numeric
           n_val_sq = as.numeric(n_val_sq),                                                                            # Change to numeric
           rob = ifelse(rob %in% c("Yes", "yes"), 1, 0),                                                               # Change to logical
           appl = ifelse(appl %in% c("Yes", "yes"), 1, 0),                                                             # Change to logical
           sent_email = ifelse(dt_email == "x" | is.na(dt_email), 0, 1),                                               # Change to logical
           dt_email = as.numeric(dt_email),                                                                            # Prepare for date transformation
           dt_email = as.Date(ifelse(sent_email == 1, dt_email, NA), origin = "1899-12-30"),                           # Change to date
           react = ifelse(sent_email == 1 & !is.na(reaction), 1, ifelse(sent_email == 1 & is.na(reaction), 0, NA)),    # Change to logical
           react_type = ifelse(reaction == "n", 0, ifelse(reaction %in% c("j", "J"), 1, NA)),                          # Change to logical
           available_level = ifelse(available_level == "Domain", 2, 1)) %>%                                            # Change to numeric category
    # Put type_diag and type_prog before type
    relocate(type_diag:type_prog, .before = type) %>%
    # Put react and react_type after sent_email
    relocate(react:react_type, .after = sent_email) %>%
    # Drop reaction column
    dplyr::select(-reaction)
           
# Add column: how it was processed based on email (colour coded in Excel). 2 = domain, 1 = SQ
dat.main[["added_level"]] <- c(2, 2, 2, 2, NA, 2, 2, 1, 1, 1, 2, 2, NA, 1, 1, 1, 1, NA, 2, 1, 2, 1, NA, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, NA, 2, 1, 2, 1, 2, 2, NA, 2, NA, 1, NA, NA,
                               1, 2, 1, 2, 2, 2, NA, 1, NA, NA, 2, NA, 2, 1, NA, 1, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, NA, 2, 1, 2, 2, 2, NA, 1, 2, 2, NA, 1, 2, 1, 2, 2, 2, 1, NA, 1, 1, 2, 1, 1, 2, 1, 2, 2,
                               1, NA, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 1, NA, 2, 2, 1, 2, NA, NA, 2, 1, 1, 2, 2, NA, 1, 2, NA, 1, 1, 1, 2, 2, NA, NA, NA, 2, NA, 2, 1, 2)

# Save and remove data
save(dat.main, file = "codes/dataframes/dat_main.Rdata"); rm(dat.main)

# 2. Signalling question data ----
# Load data
dat.sq <- read_excel("sheets/PROBAST DEF.xlsx", sheet = "Individual SQ")

# Give data new names 
colnames(dat.sq) <- c("ph1", "id", "author", "year", "title", "doi", "ph2", "wrs_author", "wrs_year", "wrs_doi", "wrs_pmid", "ph3", 
                      "dt_pmid", "dt_doi", "dt_man", "dt_study", "ph4", "type", "cstat", "notes_paper", "notes_cstat", 
                      "sq11", "sq12", "sq21", "sq22", "sq23", "sq31", "sq32", "sq33", "sq34", "sq35", "sq36", "sq41", "sq42",
                      "sq43", "sq44", "sq45", "sq46", "sq47", "sq48", "sq49", "d1_low", "d1_unclear", "d1_high", "d1",
                      "d2_low", "d2_unclear", "d2_high", "d2", "d3_low", "d3_unclear", "d3_high", "d3", "d4_low",
                      "d4_unclear", "d4_high", "d4")

# Give data labels based on row 4 of the data
dat.sq <- set_variable_labels(dat.sq, .labels = filter(dat.sq, row_number() == 4) %>% t())

# Remove first two rows and drop unnecessary columns
dat.sq <- dat.sq %>%
    # Drop first two rows
    filter(row_number() > 4) %>%
    # Drop link
    dplyr::select(-c(ph1, ph2, ph3, ph4))

# Clean variables
dat.sq <- dat.sq %>%
    # Start manipulating variables
    mutate(id = as.integer(id),                                                                                     # Change ID to integer
           year = as.numeric(year),                                                                                 # Change year to numeric
           wrs_year = ifelse(nchar(wrs_year) == 4, as.numeric(wrs_year),                                            # Change to numeric year
                             as.numeric(format(as.Date(as.numeric(wrs_year), origin = "1899-12-30"), "%Y"))),               
           dt_pmid = as.Date(as.numeric(dt_pmid), origin = "1899-12-30"),                                           # Change to date                     
           dt_doi = as.Date(as.numeric(dt_doi), origin = "1899-12-30"),                                             # Change to date
           dt_man = as.Date(as.numeric(dt_man), origin = "1899-12-30"),                                             # Change to date
           dt_study = as.Date(as.numeric(dt_study), origin = "1899-12-30"),                                         # Change to date    
           type = ifelse(type == "Development", 1, 2),                                                              # Change to numeric category
           cstat = as.numeric(cstat),                                                                               # Change to numeric
           sq11 = as.numeric(sq11),                                                                                 # Change to numeric
           sq12 = as.numeric(sq12),                                                                                 # Change to numeric
           sq21 = as.numeric(sq21),                                                                                 # Change to numeric
           sq22 = as.numeric(sq22),                                                                                 # Change to numeric
           sq23 = as.numeric(sq23),                                                                                 # Change to numeric
           sq31 = as.numeric(sq31),                                                                                 # Change to numeric
           sq32 = as.numeric(sq32),                                                                                 # Change to numeric
           sq33 = as.numeric(sq33),                                                                                 # Change to numeric
           sq34 = as.numeric(sq34),                                                                                 # Change to numeric
           sq35 = as.numeric(sq35),                                                                                 # Change to numeric
           sq36 = as.numeric(sq36),                                                                                 # Change to numeric
           sq41 = as.numeric(sq41),                                                                                 # Change to numeric
           sq42 = as.numeric(sq42),                                                                                 # Change to numeric
           sq43 = as.numeric(sq43),                                                                                 # Change to numeric
           sq44 = as.numeric(sq44),                                                                                 # Change to numeric
           sq45 = as.numeric(sq45),                                                                                 # Change to numeric
           sq46 = as.numeric(sq46),                                                                                 # Change to numeric
           sq47 = as.numeric(sq47),                                                                                 # Change to numeric
           sq48 = as.numeric(sq48),                                                                                 # Change to numeric
           sq49 = as.numeric(sq49),                                                                                 # Change to numeric
           d1_low = as.numeric(d1_low),                                                                             # Change to numeric    
           d1_unclear = as.numeric(d1_unclear),                                                                     # Change to numeric            
           d1_high = as.numeric(d1_high),                                                                           # Change to numeric      
           d1 = as.numeric(d1),                                                                                     # Change to numeric        
           d2_low = as.numeric(d2_low),                                                                             # Change to numeric    
           d2_unclear = as.numeric(d2_unclear),                                                                     # Change to numeric            
           d2_high = as.numeric(d2_high),                                                                           # Change to numeric      
           d2 = as.numeric(d2),                                                                                     # Change to numeric        
           d3_low = as.numeric(d3_low),                                                                             # Change to numeric    
           d3_unclear = as.numeric(d3_unclear),                                                                     # Change to numeric            
           d3_high = as.numeric(d3_high),                                                                           # Change to numeric      
           d3 = as.numeric(d3),                                                                                     # Change to numeric        
           d4_low = as.numeric(d4_low),                                                                             # Change to numeric    
           d4_unclear = as.numeric(d4_unclear),                                                                     # Change to numeric            
           d4_high = as.numeric(d4_high),                                                                           # Change to numeric      
           d4 = as.numeric(d4))                                                                                     # Change to numeric 

# Give each row a unique ID (i.e., each model a unique ID)
dat.sq <- dat.sq %>%
    # Create ID
    mutate(model_id = paste0("sq.", row_number()))

# Save and remove data
save(dat.sq, file = "codes/dataframes/dat_sq.Rdata"); rm(dat.sq)

# 3. Domain data ----
# Load data
dat.dom <- read_excel("sheets/PROBAST DEF.xlsx", sheet = "Domain")

# Give data new names 
colnames(dat.dom) <- c("ph1", "id", "author", "year", "title", "doi", "ph2", "wrs_author", "wrs_year", "ph3", "wrs_doi", "ph4", "wrs_pmid", 
                      "dt_doi", "dt_pmid", "dt_man", "dt_study", "ph5", "type", "cstat", "notes_paper", "notes_cstat", 
                      "d1", "d2", "d3", "d4", "ph6")

# Give data labels based on row 3 of the data
dat.dom <- set_variable_labels(dat.dom, .labels = filter(dat.dom, row_number() == 3) %>% t())

# Remove first two rows and drop unnecessary columns
dat.dom <- dat.dom %>%
    # Drop first two rows
    filter(row_number() > 3) %>%
    # Drop link
    dplyr::select(-c(ph1, ph2, ph3, ph4, ph5, ph6))
    
# Clean variables
dat.dom <- dat.dom %>%
    # Start manipulating variables
    mutate(id = as.integer(id),                                                  # Change ID to integer
           year = as.numeric(year),                                              # Change year to numeric
           wrs_year = as.numeric(wrs_year),                                      # Change year to numeric
           dt_pmid = as.Date(as.numeric(dt_pmid), origin = "1899-12-30"),        # Change to date
           dt_doi = as.Date(as.numeric(dt_doi), origin = "1899-12-30"),          # Change to date
           dt_man = as.Date(as.numeric(dt_man), origin = "1899-12-30"),          # Change to date
           dt_study = as.Date(as.numeric(dt_study), origin = "1899-12-30"),      # Change to date
           type = ifelse(type == "Development", 1, 2),                           # Change to numeric category
           cstat = as.numeric(cstat),                                            # Change to numeric
           d1 = as.numeric(d1),                                                  # Change to numeric
           d2 = as.numeric(d2),                                                  # Change to numeric
           d3 = as.numeric(d3),                                                  # Change to numeric
           d4 = as.numeric(d4))                                                  # Change to numeric

# Give each row a unique ID (i.e., each model a unique ID)
dat.dom <- dat.dom %>%
    # Create ID
    mutate(model_id = paste0("sq.", row_number()))

# Save and remove data
save(dat.dom, file = "codes/dataframes/dat_dom.Rdata"); rm(dat.dom)
