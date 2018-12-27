
install.packages("purrr", dependencies = T)
install.packages("nnet", dependencies = T)
install.packages("randomForest", dependencies = T)
install.packages("ROSE", dependencies = T)
install.packages("ggplot2", dependencies = T)
library(ROSE)
library(purrr)
library(dplyr)
library(nnet)
library(randomForest)
library(ggplot2)

####Change the project path to your local directory
prj_path <- "/Users/NINI"
data_path <- "CablecoData.csv"
data <- read.csv(paste0(prj_path, data_path))
dim(data)
head(data)

#####Independent Variables that are used for prediction
var_names <-
  c(
    # "Case.ID",
    #"Date.Opened",
    #"Date.Closed",
    "Escalation",
    "Fix.Type",
    #"Service.Identifier",
    "Product.Group.Product.Family",
    "Product.Technology",
    "Product.1",
    #"SCID",
    "Originator.Work.Group.Technology",
    "Closure.Level.2",
    "Closure.Level.3",
    "Closure.Level.4",
    #"Closure.Comments",
    "Owner.Work.Group",
    "Owner.WorkGroup.Technology",
    #"A.LOC.NAME",
    #"Z.LOC.NAME",
    #"Case.Subtype" ,
    "Case.Type"
  )

######Subset the data######
data_subset <- data %>%
  dplyr::select_(.dots = var_names)


#####Generate Data Sample using random sampling method
data_subset$Case.Type <- as.character(data_subset$Case.Type)

generate_case_data <- function(sample_size) {
  sample_data <- purrr::map(
    .x = var_names ,
    .f = function(x) {
      sample_n(data_subset[x], sample_size)
    }
  ) %>%
    purrr::reduce(cbind) %>%
    data.frame()
  return(sample_data)
}

#####Sample Size = n can be specified where n can take any values
random_sample <- generate_case_data(sample_size = 100)
write.csv(random_sample,
          paste0(prj_path, 'RANDOM_DATA.csv'),
          row.names = F)

#####Generate sample data using smote sampling method

case_type_unique <- as.vector(unique(data_subset$Case.Type))
x <- case_type_unique[1]
case_type_unique

smote_sample_overall <- data.frame()
#####Sample Size = n can be specified where n can take any values
sample_size <- 20

for (name in case_type_unique) {
  data_subset_sample <- data_subset
  #print(unique(data_subset_sample$Case.Type))
  data_subset_sample$Case.Type <-
    ifelse(data_subset_sample$Case.Type == name,
           name,
           paste0("Non - ", name))
  
  #data_subset_sample$Case.Type <- as.factor(data_subset_sample$Case.Type)
  smote_sample <-
    ROSE::ovun.sample(Case.Type ~ .,
                      data = data_subset_sample,
                      N = sample_size,
                      seed = 123)$data
  smote_sample <- smote_sample %>%
    dplyr::filter(Case.Type == name)
  smote_sample_overall <- rbind(smote_sample_overall, smote_sample)
}

dim(smote_sample_overall)
head(smote_sample_overall)
table(smote_sample_overall$Case.Type)

write.csv(smote_sample_overall,
          paste0(prj_path, 'SAMPLE_DATA.csv'),
          row.names = F)

#####Multionmial Logistic Regression TO assess relation between independent and dependent variable

####Case.Type followed by of the independent variables can be used
#####In the below code we are using Product.Group.Product.Family and product.1
model_fit <- nnet::multinom(Case.Type ~ Product.Group.Product.Family
                            + Product.1, data = data_subset)
model_summary <- summary(model_fit)

#####Calculating significance of independent variables

p_values <-
  (1 - pnorm(
    abs(model_summary$coefficients / model_summary$standard.errors),
    0,
    1
  )) * 2

df1 <- p_values %>% as.data.frame()
write.csv(p_values, paste0(prj_path, 'pval_ex.csv'))

rownames(p_values)
str(p_values)

######Siginicance of independent variables for each dependent variables are captured in the below code

sig_vars <- purrr::map(
  .x = rownames(p_values),
  .f = function(x) {
    paste0(colnames(p_values)[p_values[x, ] <= 0.05], collapse = ", ")
  }
) %>% unlist

sig_summary <- data.frame(Dep_var = rownames(p_values),
                          sig_vars = sig_vars)

write.csv(sig_summary,
          paste0(prj_path, 'significance_var_summary.csv'),
          row.names = F)
colnames(data_subset)
unique(data_subset$Product.1)

#####Random Forest

data_subset$Case.Type <- as.factor(data_subset$Case.Type)
rF_Model_Fit <-
  randomForest(Case.Type ~ .,   data = data_subset, mtry = 3)
summary(rF_Model_Fit)

#### Important variable split is calculated below
importanceVar_RF <- importance(rF_Model_Fit) %>% as.data.frame()
write.csv(importanceVar_RF, paste0(prj_path, 'importanceVar_RF.csv'))

#### Calculating the accuracy of the model
accuracy_table <-
  data.frame(
    Dep_Var = rownames(rF_Model_Fit$confusion),
    Accuracy = 1 - rF_Model_Fit$confusion[, "class.error"]
  )

####Plotting the same
ggplot(data = accuracy_table, aes(x = Dep_Var,
                                  y = Accuracy)) +
  geom_bar(stat = "identity")

write.csv(accuracy_table,
          paste0(prj_path, 'accuracy_table_RF.csv'),
          row.names = F)

#######Predict

####Random Sample#####
head(random_sample)
random_sample$Case.Type <- NULL

random_sample$Case.Type <-
  predict(rF_Model_Fit, random_sample) %>% as.data.frame() %>% unlist

write.csv(random_sample,
          paste0(prj_path, 'random_sample_predict.csv'),
          row.names = F)


