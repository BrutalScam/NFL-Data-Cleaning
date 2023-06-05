# NFL-Data-Cleaning
Data cleaning of NFL elo dataset


# Dataset Name (2 sheets):
nfl_elo_latest.xlsx

# Packages: 
1. Readxl – Data Importation
2. Tidyverse – Data Handling and Data Cleaning
3. Funmodeling – Data Overview and Data Structure
4. Lubridate – Date Formatting
5. Mice – Missing Data Imputation

# Sections:
1.	Packages: library ()
2.	Data Importation and Data Overview: read_excel (), df_status ()
3.	Date Variable Cleaning: as.Date ()
4.	Initial Data Cleaning on Logic: select ()
5.	Categorical Variables Cleaning: '%!in%' <- function(x,y)!('%in%'(x,y)), is.na(), which() 
6.	Numerical Variables Cleaning: df_status(), as.numeric(), colnames(), select(), mice(), complete()
7.	Combining Data Sets: cbind(), df_status(), as.numeric(), which(), is.na(), mean(), round(), rbind(), write.csv()
8.	Visualizing Data: subset(), gather(), ggplot()
