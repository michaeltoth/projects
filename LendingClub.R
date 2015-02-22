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

# Convert delinquencies, inquiries, open accounts, public records variables to factors
train$delinq_2yrs <- factor(train$delinq_2yrs)
train$inq_last_6mths <- factor(train$inq_last_6mths)
train$open_acc <- factor(train$open_acc)
train$pub_rec <- factor(train$pub_rec)
train$total_acc <- factor(train$total_acc)



# Convert interest rate numbers to numeric
train$int_rate <- as.numeric(sub("%", "", train$int_rate))
train$revol_util <- as.numeric(sub("%", "", train$revol_util))

    
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

# Exclude ownership of "OTHER" and "NONE" because of so few data points
ownership_status <- table(train$new_status,train$home_ownership,
                     exclude=c("OTHER","NONE",""))

prop_ownership <- round(prop.table(ownership_status, 2) * 100, 2)

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


### Categorical

# Employment Length

# Combining factors
levels(train$emp_length) <- c("None", "< 10 years", "< 10 years", "10+ years",
                              rep("< 10 years", 8), "None")

emp_length <- table(train$new_status, train$emp_length)
prop_emp_length <- round(prop.table(emp_length, 2) * 100, 2)

# Verified Income

verified <- table(train$new_status, train$is_inc_v, exclude = "")
prop_verified <- round(prop.table(verified, 2) * 100, 2)

# Delinquencies past 2 Years

levels(train$delinq_2yrs) <- c("0", "1", "2", rep("3+", 17))
delinquencies <- table(train$new_status, train$delinq_2yrs)
prop_delinq <- round(prop.table(delinquencies, 2) * 100, 2)

# Inquiries 6 mo

levels(train$inq_last_6mths) <- c("0", "1", "2", "3", rep("4+", 5))
inquiries <- table(train$new_status, train$inq_last_6mths)
prop_inquiries <- round(prop.table(inquiries, 2) * 100, 2)

# mths_since_last_delinq

train$new_mths_since_last_delinq <- cut(train$mths_since_last_delinq, 
                                   breaks = c(0, 10, 20, 30, 40, 50, 60, 156))
last_delinq <- table(train$new_status, train$new_mths_since_last_delinq)
prop_last_delinq <- round(prop.table(last_delinq, 2) * 100, 2)

# mths_since_last_record

na_list <- sum(is.na(train$mths_since_last_record))
not_na_list <- sum(!is.na(train$mths_since_last_record))
na_list_dflt <- sum(is.na(train$mths_since_last_record) & train$new_status == "NonPerforming")
not_na_list_dflt <- sum(!is.na(train$mths_since_last_record) & train$new_status == "NonPerforming")
# Differences here are significant, opposite from expected direction.  Need to create new factor for N/A vs not N/A to check
not_na_list_dflt / not_na_list
na_list_dflt/na_list

# mths_since_last_major_derog, do similar to the above

# open_acc

levels(train$open_acc) <- c(rep("<= 5", 6), rep("6 - 10", 5), 
                                  rep("11 - 15", 5), rep("16+", 38))
accounts <- table(train$new_status, train$open_acc)
prop_accounts <- round(prop.table(accounts, 2) * 100, 2)

# pub_rec

levels(train$pub_rec) <- c("0", "1", rep("2+", 12))
pub_rec <- table(train$new_status, train$pub_rec)
prop_pub_rec <- round(prop.table(pub_rec, 2) * 100, 2)


# total_acc

levels(train$total_acc) <- c(rep("<= 7", 5), rep("8 - 12", 5), 
                            rep("13 - 17", 5), rep("18 - 22", 5), 
                            rep("23+", 68))
total_acc <- table(train$new_status, train$total_acc)
prop_total_acc <- round(prop.table(total_acc, 2) * 100, 2)

# collections_12_mths_ex_med

collections <- table(train$new_status, train$collections_12_mths_ex_med)
prop_collections <- round(prop.table(collections, 2) * 100, 2)

# Investigate performance by loan purpose
purpose <- table(train$new_status,train$purpose)
prop.table(purpose, 2)

# Earliest Line will be annoying with date elements

### Numerical

# Loan Amount, seems like no significant differences

# Annual Income
plot(train$new_status, log(train$annual_inc))

# DTI looks significant

# revol_util might be significant















#fit <- rpart(new_status ~ loan_amnt + term + int_rate + installment + grade + 
#                 sub_grade + emp_length + home_ownership + annual_inc + 
#                 is_inc_v + purpose + addr_state + dti + delinq_2yrs + 
#                 earliest_cr_line + inq_last_6mths + mths_since_last_delinq +
#                 mths_since_last_record + open_acc + pub_rec + revol_bal +
#                 revol_util + total_acc + initial_list_status + 
#                 collections_12_mths_ex_med + mths_since_last_major_derog,
#             data = train, method = "class")

simple_fit <- glm(new_status ~ grade + home_ownership, data=train,family=binomial())
