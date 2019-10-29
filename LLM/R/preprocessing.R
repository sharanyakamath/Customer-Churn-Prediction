require(dplyr)

#read data
data <- read.csv("TelcoCustomerChurn.csv")

#converts rows to numerical
data <- data %>%
      mutate(gender = ifelse(gender == "Female",0,1))

data <- data %>%
      mutate(Partner = ifelse(Partner == "No",0,1))

data <- data %>%
      mutate(Dependents = ifelse(Dependents == "No",0,1))

data <- data %>%
      mutate(PhoneService = ifelse(PhoneService == "No",0,1))

data <- data %>%
      mutate(MultipleLines = ifelse(MultipleLines == "Yes",1,0))

data <- data %>%
      mutate(InternetService = ifelse(InternetService == "No",0,ifelse(InternetService == "DSL",1,2)))

data <- data %>%
      mutate(OnlineSecurity = ifelse(OnlineSecurity == "Yes",1,0))

data <- data %>%
      mutate(OnlineBackup = ifelse(OnlineBackup == "Yes",1,0))

data <- data %>%
      mutate(DeviceProtection = ifelse(DeviceProtection == "Yes",1,0))

data <- data %>%
      mutate(TechSupport = ifelse(TechSupport == "Yes",1,0))

data <- data %>%
      mutate(StreamingTV = ifelse(StreamingTV == "Yes",1,0))

data <- data %>%
      mutate(StreamingMovies = ifelse(StreamingMovies == "Yes",1,0))

data <- data %>%
      mutate(Contract = ifelse(Contract == "Month-to-Month",1,ifelse(Contract == "One year",2,3)))

data <- data %>%
      mutate(PaperlessBilling = ifelse(PaperlessBilling == "Yes",1,0))

data <- data %>%
      mutate(TotalCharges = ifelse(is.na(TotalCharges),0,TotalCharges))

data <- data %>%
      mutate(PaymentMethod = ifelse(PaymentMethod == "Electronic check",1,
      	ifelse(PaymentMethod == "Mailed check",2,
      		ifelse(PaymentMethod == "Bank transfer (automatic)",3,4))))

# data <- data %>%
#       mutate(Churn = ifelse(Churn == "Yes",1,0))

#remove column id
cols.dont.want <- "customerID"
data <- data[, ! names(data) %in% cols.dont.want, drop = F]

#write to csv file
write.csv(data,"preprocessed.csv",row.names = FALSE)