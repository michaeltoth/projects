library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

Download and extract data from Lending Club
if (!file.exists("LoanStats3b.csv")) {
    fileUrl <- "https://resources.lendingclub.com/LoanStats3b.csv.zip"
    download.file(fileUrl, destfile = "LoanStats3b.csv.zip", method="curl")
    dateDownloaded <- date()
    unzip("LoanStats3b.csv.zip")
}

# Read in Lending Club Data
full_dataset <- read.csv(file="LoanStats3b.csv", header=TRUE, skip = 1)

# Select variables to keep
variables <- c("id", "loan_amnt", "term", "int_rate", "installment", "grade", 
               "sub_grade", "emp_length", "home_ownership", "annual_inc", 
               "is_inc_v", "loan_status", "desc", "purpose", 
               "addr_state", "dti", "delinq_2yrs", "earliest_cr_line", 
               "inq_last_6mths", "mths_since_last_delinq", 
               "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", 
               "revol_util", "total_acc", "initial_list_status", 
               "collections_12_mths_ex_med", "mths_since_last_major_derog")

# Subset the data using only the selected variables
train <- full_dataset[variables]

# Reduce loan status to binary "Performing" and "NonPerforming" Measures:
train$new_status <- factor(ifelse(train$loan_status %in% c("Current", "Fully Paid"), 
                                  "Performing", "NonPerforming"))

# Convert interest rate numbers to numeric
train$int_rate <- as.numeric(sub("%", "", train$int_rate))
train$revol_util <- as.numeric(sub("%", "", train$revol_util))

    
# Investigate performance by home ownership.  In general, Mortgage > Own > Rent.  Significant?
home_status <- table(train$new_status,train$home_ownership)
prop.table(home_status, 2)

# Investigate performance by loan purpose
purpose <- table(train$new_status,train$purpose)
prop.table(purpose, 2)

fit <- rpart(new_status ~ loan_amnt + term + int_rate + installment + grade + 
                 sub_grade + emp_length + home_ownership + annual_inc + 
                 is_inc_v + purpose + addr_state + dti + delinq_2yrs + 
                 earliest_cr_line + inq_last_6mths + mths_since_last_delinq +
                 mths_since_last_record + open_acc + pub_rec + revol_bal +
                 revol_util + total_acc + initial_list_status + 
                 collections_12_mths_ex_med + mths_since_last_major_derog,
             data = train, method = "class")

simple_fit <- glm(new_status ~ grade + home_ownership, data=train,family=binomial())
