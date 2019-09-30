## Packages

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(tidyr)

library(cowplot)
library(caret)
library(rpart)
library(ROCR)
library(rpart.plot)
library(ggplot2)

## ggplot theme
theme <- theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.position="none" 
)

## Load Data
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

data <- data %>%
mutate(
    # column was int but best to have it as logical
    SeniorCitizen = as.logical(SeniorCitizen)
)
str(data)

data %>%
    summarise_all(
        funs(sum(is.na(.)))
    ) %>%
gather(ColumnTitle, NAs, customerID:Churn)

data %>%
select(
    customerID, tenure, TotalCharges
) %>%
filter(
    is.na(TotalCharges)
)

options(repr.plot.width = 4, repr.plot.height = 3)

data %>%
group_by(Churn) %>%
    summarize(
        n = n()
    ) %>%
    mutate(
        percentage = round(n / sum(n), 3),
        n = NULL
    ) %>%
ggplot(aes(x = Churn, y = percentage)) + geom_col(aes(fill = Churn)) +
theme +
geom_text(
        aes(x = Churn, y = percentage, label = paste(percentage*100, "%", sep = ""))
    )


# Decrease graph size from standard
options(repr.plot.width = 4, repr.plot.height = 4)

# Function to generate graphs for factor variables and churn

## Extract columns to be analyzed
function_columns <- data %>%
    select(
        "gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines", 
        "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
        "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "Churn"
          )

## Function, goes through each column selected
for (i in 1:ncol(function_columns))
{
    # Get column names so dplyr group by works
    cname <- colnames(function_columns[c(i,17)])
    # Subset data frame by variable name selected
    a <- subset(
        function_columns, !is.na(function_columns[,i]) & function_columns[,i] != "",
                select = cname
    ) %>%
    # Create percentage statistics per variable
    group_by_at(vars(cname)) %>%
    summarize(
        n = n()
    ) %>%
    mutate(
        Percentage = round(n / sum(n), 2)
    )
    
    # Save plot in a variable so plots can be displayed sequentialy
    p <- ggplot(
        data = a, aes_string(
            x = colnames(a[1]), y = colnames(a[4]), fill = colnames(a[1])
        )
    ) +
    # Split each graph per Churn to see influence of variable
    facet_wrap("Churn") + 
    geom_bar(stat = "identity") +
    # Make graph a bit cleaner
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position="none"
    ) +
    geom_text(
        aes(y = Percentage, label = paste0(Percentage * 100,"%"))
    ) +
    labs(
        x = colnames(a[1]), y = "Churn", title = paste("Churn and", colnames(a[1]))
    )
    
    # Display graphs
    print(p)
    # Cleanup
    rm(cname, a, p)
}


# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
data %>%
    filter(Churn == "Yes") %>%
    group_by(tenure) %>%
    summarize(
        n = n()
    ) %>%
    mutate(
        Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
        aes(x = tenure, y = Percentage, color = tenure)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
        x = "Tenure", y = "Churn (%)"
    ),

ggplot(
    data = data,
    aes(y = tenure, x = Churn, color = Churn)
    ) +
    theme +
    geom_boxplot()
, align = "h")

# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
data %>%
    filter(Churn == "Yes") %>%
    group_by(MonthlyCharges) %>%
    summarize(
        n = n()
    ) %>%
    mutate(
        Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
        aes(x = MonthlyCharges, y = Percentage, color = MonthlyCharges)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
        x = "Monthly Charges", y = "Churn (%)"
    ),

ggplot(
    data = data,
    aes(y = MonthlyCharges, x = Churn, color = Churn)
    ) +
    theme +
    geom_boxplot()
, align = "h")

# Remove columns we didn't see correlation from above
data.model <- data %>%
    select(
        -customerID, -gender,-PhoneService, -MultipleLines, -MonthlyCharges, -TotalCharges 
    )
