# Download and extract data from Lending Club
if (!file.exists("LoanStats3b.csv")) {
    fileUrl <- "https://resources.lendingclub.com/LoanStats3b.csv.zip"
    download.file(fileUrl, destfile = "LoanStats3b.csv.zip", method="curl")
    dateDownloaded <- date()
    unzip("LoanStats3b.csv.zip")
}

# Read in Lending Club Data
if (!exists("full_dataset")) {
  full_dataset <- read.csv(file="LoanStats3b.csv", header=TRUE, skip = 1)
}

# Select variables to keep and subset the data
variables <- c("id", "loan_amnt", "term", "int_rate", "installment", "grade", 
               "sub_grade", "emp_length", "home_ownership", "annual_inc", 
               "is_inc_v", "loan_status", "purpose", "addr_state", "dti", 
               "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", 
               "mths_since_last_delinq", "mths_since_last_record", "open_acc", 
               "pub_rec", "revol_bal", "revol_util", "total_acc", 
               "initial_list_status", "collections_12_mths_ex_med", 
               "mths_since_last_major_derog")
train <- full_dataset[variables]

# Reduce loan status to binary "Performing" and "NonPerforming" Measures:
train$new_status <- factor(ifelse(train$loan_status %in% c("Current", "Fully Paid"), 
                                  "Performing", "NonPerforming"))

# Convert a subset of the numeric variables to factors
train$delinq_2yrs <- factor(train$delinq_2yrs)
train$inq_last_6mths <- factor(train$inq_last_6mths)
train$open_acc <- factor(train$open_acc)
train$pub_rec <- factor(train$pub_rec)
train$total_acc <- factor(train$total_acc)

# Convert interest rate numbers to numeric (strip percent signs)
train$int_rate <- as.numeric(sub("%", "", train$int_rate))
train$revol_util <- as.numeric(sub("%", "", train$revol_util))

# Graph default rates by grade and subgrade and store images in png files:
png(filename = "by_grade.png", width = 660, height = 480)
by_grade <- table(train$new_status, train$grade, exclude="")
prop_grade <- prop.table(by_grade,2)
barplot(prop_grade, main = "Loan Performance by Grade", xlab = "Grade", 
        col=c("darkblue","red"), legend = rownames(prop_grade))
dev.off()

png(filename = "by_subgrade.png", width = 660, height = 480)
by_subgrade <- table(train$new_status, train$sub_grade, exclude="")
prop_subgrade <- prop.table(by_subgrade,2)
barplot(prop_subgrade, main = "Loan Performance by Sub Grade", xlab = "SubGrade",
        col=c("darkblue","red"),legend = rownames(prop_subgrade))
dev.off()



### Explore the relationships between default rates and factor levels
### I take a few different approaches, but the key idea is the same


# Ownership status (exclude status "OTHER" and "NONE" because of few data points)
home_ownership <- table(train$new_status,train$home_ownership,
                     exclude=c("OTHER","NONE",""))
prop_home_ownership <- round(prop.table(home_ownership, 2) * 100, 2)

# Test for significance of the difference in proportions for home ownership factors
# Calculate the counts of mortgage, owners, and renters:
count_m <- sum(train$home_ownership == "MORTGAGE")
count_o <- sum(train$home_ownership == "OWN")
count_r <- sum(train$home_ownership == "RENT")

# Calculate the counts of default for mortgages, owners, and renters:
dflt_m <- sum(train$home_ownership == "MORTGAGE" & train$new_status == "NonPerforming")
dflt_o <- sum(train$home_ownership == "OWN" & train$new_status == "NonPerforming")
dflt_r <- sum(train$home_ownership == "RENT" & train$new_status == "NonPerforming")

# 1-sided proportion test for mortgage vs owners
prop.test(c(dflt_m,dflt_o), c(count_m,count_o), alternative = "less")
# 1-sided proportion test for owners vs renters
prop.test(c(dflt_o,dflt_r), c(count_o,count_r), alternative = "less")


# Employment Length (combine factor levels for better comparison)
levels(train$emp_length) <- c("None", "< 10 years", "< 10 years", "10+ years",
                              rep("< 10 years", 8), "None")
emp_length <- table(train$new_status, train$emp_length)
prop_emp_length <- round(prop.table(emp_length, 2) * 100, 2)


# Verified income status
is_inc_v <- table(train$new_status, train$is_inc_v, exclude = "")
prop_is_inc_v <- round(prop.table(is_inc_v, 2) * 100, 2)


# Delinquencies in the past 2 Years (combine factors levels for any > 3)
levels(train$delinq_2yrs) <- c("0", "1", "2", rep("3+", 17))
delinq_2yrs <- table(train$new_status, train$delinq_2yrs)
prop_delinq_2yrs <- round(prop.table(delinq_2yrs, 2) * 100, 2)


# Inquiries in the last 6 months (combine factor levels for any > 4)
levels(train$inq_last_6mths) <- c("0", "1", "2", "3", rep("4+", 5))
inq_last_6mths <- table(train$new_status, train$inq_last_6mths)
prop_inq_last_6mths <- round(prop.table(inq_last_6mths, 2) * 100, 2)


# Months since last delinquency (break factor levels in increments of 10)
train$mths_since_last_delinq <- cut(train$mths_since_last_delinq, 
                                   breaks = c(0, 10, 20, 30, 40, 50, 60, 156))
mths_since_last_delinq <- table(train$new_status, train$mths_since_last_delinq)
prop_mths_since_last_delinq <- round(prop.table(mths_since_last_delinq, 2) * 100, 2)

# Months Since Last Record (compare blank vs. non-blank)
na_last_record <- sum(is.na(train$mths_since_last_record))
not_na_last_record <- sum(!is.na(train$mths_since_last_record))
na_last_rec_dflt <- sum(is.na(train$mths_since_last_record) & train$new_status == "NonPerforming")
not_na_last_rec_dflt <- sum(!is.na(train$mths_since_last_record) & train$new_status == "NonPerforming")

not_na_last_rec_pct_dflt <- not_na_last_rec_dflt / not_na_last_record
na_last_rec_pct_dflt <- na_last_rec_dflt/na_last_record


# Number of Open Accounts (combine factor levels into groups of 5)
levels(train$open_acc) <- c(rep("<= 5", 6), rep("6 - 10", 5), 
                                  rep("11 - 15", 5), rep("16+", 38))
open_acc <- table(train$new_status, train$open_acc)
prop_open_acc <- round(prop.table(open_acc, 2) * 100, 2)


# Number of Public Revords (break factor levels into 0, 1, 2+)
levels(train$pub_rec) <- c("0", "1", rep("2+", 12))
pub_rec <- table(train$new_status, train$pub_rec)
prop_pub_rec <- round(prop.table(pub_rec, 2) * 100, 2)


# Number of total accounts (combine factor levels into groups of 5, then 23+)
levels(train$total_acc) <- c(rep("<= 7", 5), rep("8 - 12", 5), 
                            rep("13 - 17", 5), rep("18 - 22", 5), 
                            rep("23+", 68))
total_acc <- table(train$new_status, train$total_acc)
prop_total_acc <- round(prop.table(total_acc, 2) * 100, 2)


# Collections last 12 months
collections <- table(train$new_status, train$collections_12_mths_ex_med)
prop_collections <- round(prop.table(collections, 2) * 100, 2)


# Loan Purpose (exclude renewable energy because so few data points)
purpose <- table(train$new_status,train$purpose, exclude = c("renewable_energy",""))
prop_purpose <- prop.table(purpose, 2)


# Loan Amount (break into < 15k, 15k - 30k, 30k - 35k)
train$new_loan_amnt <- cut(train$loan_amnt,c(0, 15000, 30000, 35000))
loan_amnt <- table(train$new_status, train$new_loan_amnt)
prop_loan_amnt <- round(prop.table(loan_amnt, 2) * 100, 2)


# Annual Income (factor into quantiles of 20%)
train$new_annual_inc <- cut(train$annual_inc,
                            quantile(train$annual_inc, na.rm = TRUE,
                                     probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))
annual_inc <- table(train$new_status, train$new_annual_inc)
prop_annual_inc <- round(prop.table(annual_inc, 2) * 100, 2)

# Debt to Income Ratio (break into factors at 5% levels)
train$new_dti <- cut(train$dti, breaks = c(0, 5, 10, 15, 20, 25, 30, 35))
dti <- table(train$new_status, train$new_dti)
prop_dti <- round(prop.table(dti, 2) * 100, 2)


# Revolving Utilization (break into 0 - 20, then factors of 10, then 80+)
train$new_revol_util <- cut(train$revol_util, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 141))
revol_util <- table(train$new_status, train$new_revol_util)
prop_revol_util <- round(prop.table(revol_util, 2) * 100, 2)