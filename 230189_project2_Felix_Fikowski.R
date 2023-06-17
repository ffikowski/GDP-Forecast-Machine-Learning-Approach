# Case Studies - Report 1 (Felix Fikowski, 280189)

################################################################################
#Laden benötigter Pakete
{
  library(ggplot2)
  library(rpart)
  library(rpart.plot)
  library(ranger)
  library(randomForest)
}
################################################################################

#Setzen des Dateipfades

#Import of the data
{
  setwd("C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies\\project 2")
  
  df <- read.csv("2022-02.csv")
  df <- as.data.frame(df)
  head(df)
  dim(df)
  #[1] 256 247
  
  #remove uninformative rows:
  df <- df[-c(1:2, 255:256), ] 
  
  colnames(df)
  df <- df[, c("sasdate","GDPC1","CUMFNS","UNRATESTx","CPIAUCSL",
               "FEDFUNDS","M1REAL","S.P.500")]
  
  head(df)
  
  dim(df)
  #[1] 256   7
}

#xtable(con_table)


################################################################################
#Transform the data as mentioned in the project description
{
  matrix <- data.matrix(df, rownames.force = NA)
  
  #transform GDP (GDP1)
  GDP <- matrix[,2]
  GDP_per <- diff(GDP,lag=1, differences = 1)/GDP[1:251]*100
  
  #transform manufacturing industry capacity utilization (CUMFNS)
  CUMFNS <- matrix[2:252,3]
  
  #transform unemploymentrate (UNRATESTx )
  unemp <- matrix[2:252,4]
  head(unemp)
  
  #transform consumerprice index (CPIAUCSL  )
  CPIAUCSL <- matrix[,5]
  CPIAUCSL_per <- diff(CPIAUCSL,lag=1, differences = 1)/CPIAUCSL[1:251]*100
  head(CPIAUCSL_per)
  
  #transform Federal Funds rate (FEDFUNDS)
  FEDFUNDS <- matrix[2:252,6]
  head(FEDFUNDS)
  
  #transform M1 money stock (M1REAL)
  M1REAL <- matrix[,7]
  M1REAL_per <- diff(M1REAL,lag=1, differences = 1)/M1REAL[1:251]*100
  head(M1REAL_per)
  
  #transform S.P.500 (S.P.500)
  S.P.500 <- matrix[,8]
  S.P.500_per <- diff(S.P.500,lag=1, differences = 1)/S.P.500[1:251]*100
  head(S.P.500_per)
  
  
  dates <- as.Date(df[4:254,1], "%m/%d/%Y")
  
  per_matrix <- cbind(GDP_per, CUMFNS, unemp, CPIAUCSL_per, FEDFUNDS, M1REAL_per, S.P.500_per)
  head(per_matrix)
  
  dates <- as.Date(df[2:252,1], "%m/%d/%Y")
  
  data <- data.frame(dates,per_matrix[,1], per_matrix[,2], per_matrix[,3],
                     per_matrix[,4], per_matrix[,5], per_matrix[,6], per_matrix[,7])
  colnames(data) <- c("dates", "GDP_per", "CUMFNS", "unemp", 
                      "CPIAUCSL_per", "FEDFUNDS", "M1REAL_per", "S.P.500_per")
  #5-Number summary of the data:
  
  GDPC1<-summary(data$GDP_per)
  CUMFNS<-summary(data$CUMFNS)
  UNRATESTx<-summary(data$unemp)
  CPIAUCSL<-summary(data$CPIAUCSL_per)
  FEDFUNDS<-summary(data$FEDFUNDS)
  M1REAL<-summary(data$M1REAL_per)
  S.P.500<-summary(data$S.P.500_per)
  
  
  compare1<-cbind(GDPC1,CUMFNS, UNRATESTx, CPIAUCSL, FEDFUNDS, M1REAL, S.P.500)
  con_table <- round(compare1,2)
  con_table
}

#         GDPC1 CUMFNS UNRATESTx CPIAUCSL FEDFUNDS M1REAL S.P.500
#Min.    -8.94  63.76      2.81    -2.29     0.06  -3.82  -27.33
#1st Qu.  0.34  75.71      3.92     0.50     1.94  -0.54   -0.86
#Median   0.75  78.94      4.75     0.79     4.74   0.47    2.00
#Mean     0.74  79.28      4.86     0.91     4.83   1.49    1.96
#3rd Qu.  1.16  82.84      5.47     1.14     6.55   1.28    5.61
#Max.     7.55  91.57     12.25     3.95    17.78 207.56   20.12

#xtable(con_table)

################################################################################
#a)

#build the data set
{
  length(data$GDP_per)
  dates <- data$dates[11:length(data$dates)]
  response <- as.numeric(data$GDP_per[11:length(data$GDP_per)])
  
  typeof(response)
  GDP_growth_lag10 <- data$GDP_per[1:(length(data$GDP_per)-10)]
  GDP_growth_lag9 <- data$GDP_per[2:(length(data$GDP_per)-9)]
  GDP_growth_lag8 <- data$GDP_per[3:(length(data$GDP_per)-8)]
  GDP_growth_lag7 <- data$GDP_per[4:(length(data$GDP_per)-7)]
  GDP_growth_lag6 <- data$GDP_per[5:(length(data$GDP_per)-6)]
  GDP_growth_lag5 <- data$GDP_per[6:(length(data$GDP_per)-5)]
  GDP_growth_lag4 <- data$GDP_per[7:(length(data$GDP_per)-4)]
  GDP_growth_lag3 <- data$GDP_per[8:(length(data$GDP_per)-3)]
  GDP_growth_lag2 <- data$GDP_per[9:(length(data$GDP_per)-2)]
  GDP_growth_lag1 <- data$GDP_per[10:(length(data$GDP_per)-1)]
  
  data2 <- data.frame(dates,GDP_growth_lag10,GDP_growth_lag9,GDP_growth_lag8,GDP_growth_lag7,GDP_growth_lag6,GDP_growth_lag5,GDP_growth_lag4,GDP_growth_lag3,GDP_growth_lag2,GDP_growth_lag1, response)
  dim(data2)
}
#fit the regression tree
fit <- rpart(response ~ ., method = "anova", data = data2[,2:12])

#node36 <- c()
#for (i in 1:241) {
#  if (node[i]==8){
#    node36 <- c(node36, node[i])    
#    
#  }
#}
#node36


fittet_v<-predict(fit)

#plot the refression tree

pdf(file = "rt_small_plotoftree.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
## Run dev.off() to create the PDF file 
rpart.plot(fit,type=0, fallen.leaves=TRUE, 
           box.palette=c(),
           shadow.col = "grey", extra=101, digits = 4)
dev.off()

#get the observations in each node

node_rt_small <- fit$where
node_numrt_small <- data.frame(node_rt_small, fittet_v) 
rownames(node_numrt_small)<- data2$dates
node_numrt_small
################################################################################
#b)
#build the data set
{
  #CUMFNS
  CUMFNS_lag10 <- data$CUMFNS[1:(length(data$CUMFNS)-10)]
  CUMFNS_lag9 <- data$CUMFNS[2:(length(data$CUMFNS)-9)]
  CUMFNS_lag8 <- data$CUMFNS[3:(length(data$CUMFNS)-8)]
  CUMFNS_lag7 <- data$CUMFNS[4:(length(data$CUMFNS)-7)]
  CUMFNS_lag6 <- data$CUMFNS[5:(length(data$CUMFNS)-6)]
  CUMFNS_lag5 <- data$CUMFNS[6:(length(data$CUMFNS)-5)]
  CUMFNS_lag4 <- data$CUMFNS[7:(length(data$CUMFNS)-4)]
  CUMFNS_lag3 <- data$CUMFNS[8:(length(data$CUMFNS)-3)]
  CUMFNS_lag2 <- data$CUMFNS[9:(length(data$CUMFNS)-2)]
  CUMFNS_lag1 <- data$CUMFNS[10:(length(data$CUMFNS)-1)]
  
  #unemp
  UNRATESTx_lag10 <- data$unemp[1:(length(data$unemp)-10)]
  UNRATESTx_lag9 <- data$unemp[2:(length(data$unemp)-9)]
  UNRATESTx_lag8 <- data$unemp[3:(length(data$unemp)-8)]
  UNRATESTx_lag7 <- data$unemp[4:(length(data$unemp)-7)]
  UNRATESTx_lag6 <- data$unemp[5:(length(data$unemp)-6)]
  UNRATESTx_lag5 <- data$unemp[6:(length(data$unemp)-5)]
  UNRATESTx_lag4 <- data$unemp[7:(length(data$unemp)-4)]
  UNRATESTx_lag3 <- data$unemp[8:(length(data$unemp)-3)]
  UNRATESTx_lag2 <- data$unemp[9:(length(data$unemp)-2)]
  UNRATESTx_lag1 <- data$unemp[10:(length(data$unemp)-1)]
  
  #CPIAUCSL_per
  inflation_lag10 <- data$CPIAUCSL_per[1:(length(data$CPIAUCSL_per)-10)]
  inflation_lag9 <- data$CPIAUCSL_per[2:(length(data$CPIAUCSL_per)-9)]
  inflation_lag8 <- data$CPIAUCSL_per[3:(length(data$CPIAUCSL_per)-8)]
  inflation_lag7 <- data$CPIAUCSL_per[4:(length(data$CPIAUCSL_per)-7)]
  inflation_lag6 <- data$CPIAUCSL_per[5:(length(data$CPIAUCSL_per)-6)]
  inflation_lag5 <- data$CPIAUCSL_per[6:(length(data$CPIAUCSL_per)-5)]
  inflation_lag4 <- data$CPIAUCSL_per[7:(length(data$CPIAUCSL_per)-4)]
  inflation_lag3 <- data$CPIAUCSL_per[8:(length(data$CPIAUCSL_per)-3)]
  inflation_lag2 <- data$CPIAUCSL_per[9:(length(data$CPIAUCSL_per)-2)]
  inflation_lag1 <- data$CPIAUCSL_per[10:(length(data$CPIAUCSL_per)-1)]
  
  #FEDFUNDS
  FEDFUNDS_lag10 <- data$FEDFUNDS[1:(length(data$FEDFUNDS)-10)]
  FEDFUNDS_lag9 <- data$FEDFUNDS[2:(length(data$FEDFUNDS)-9)]
  FEDFUNDS_lag8 <- data$FEDFUNDS[3:(length(data$FEDFUNDS)-8)]
  FEDFUNDS_lag7 <- data$FEDFUNDS[4:(length(data$FEDFUNDS)-7)]
  FEDFUNDS_lag6 <- data$FEDFUNDS[5:(length(data$FEDFUNDS)-6)]
  FEDFUNDS_lag5 <- data$FEDFUNDS[6:(length(data$FEDFUNDS)-5)]
  FEDFUNDS_lag4 <- data$FEDFUNDS[7:(length(data$FEDFUNDS)-4)]
  FEDFUNDS_lag3 <- data$FEDFUNDS[8:(length(data$FEDFUNDS)-3)]
  FEDFUNDS_lag2 <- data$FEDFUNDS[9:(length(data$FEDFUNDS)-2)]
  FEDFUNDS_lag1 <- data$FEDFUNDS[10:(length(data$FEDFUNDS)-1)]
  
  #M1REAL_per
  M1REAL_growth_lag10 <- data$M1REAL_per[1:(length(data$M1REAL_per)-10)]
  M1REAL_growth_lag9 <- data$M1REAL_per[2:(length(data$M1REAL_per)-9)]
  M1REAL_growth_lag8 <- data$M1REAL_per[3:(length(data$M1REAL_per)-8)]
  M1REAL_growth_lag7 <- data$M1REAL_per[4:(length(data$M1REAL_per)-7)]
  M1REAL_growth_lag6 <- data$M1REAL_per[5:(length(data$M1REAL_per)-6)]
  M1REAL_growth_lag5 <- data$M1REAL_per[6:(length(data$M1REAL_per)-5)]
  M1REAL_growth_lag4 <- data$M1REAL_per[7:(length(data$M1REAL_per)-4)]
  M1REAL_growth_lag3 <- data$M1REAL_per[8:(length(data$M1REAL_per)-3)]
  M1REAL_growth_lag2 <- data$M1REAL_per[9:(length(data$M1REAL_per)-2)]
  M1REAL_growth_lag1 <- data$M1REAL_per[10:(length(data$M1REAL_per)-1)]

  #S.P.500_per
  SP500_growth_lag10 <- data$S.P.500_per[1:(length(data$S.P.500_per)-10)]
  SP500_growth_lag9 <- data$S.P.500_per[2:(length(data$S.P.500_per)-9)]
  SP500_growth_lag8 <- data$S.P.500_per[3:(length(data$S.P.500_per)-8)]
  SP500_growth_lag7 <- data$S.P.500_per[4:(length(data$S.P.500_per)-7)]
  SP500_growth_lag6 <- data$S.P.500_per[5:(length(data$S.P.500_per)-6)]
  SP500_growth_lag5 <- data$S.P.500_per[6:(length(data$S.P.500_per)-5)]
  SP500_growth_lag4 <- data$S.P.500_per[7:(length(data$S.P.500_per)-4)]
  SP500_growth_lag3 <- data$S.P.500_per[8:(length(data$S.P.500_per)-3)]
  SP500_growth_lag2 <- data$S.P.500_per[9:(length(data$S.P.500_per)-2)]
  SP500_growth_lag1 <- data$S.P.500_per[10:(length(data$S.P.500_per)-1)]
  
  data3 <- data.frame(data2, CUMFNS_lag10,CUMFNS_lag9,CUMFNS_lag8,CUMFNS_lag7,CUMFNS_lag6,
                      CUMFNS_lag5,CUMFNS_lag4,CUMFNS_lag3,CUMFNS_lag2,
                      CUMFNS_lag1,UNRATESTx_lag10,UNRATESTx_lag9,UNRATESTx_lag8,UNRATESTx_lag7,
                      UNRATESTx_lag6,UNRATESTx_lag5,UNRATESTx_lag4,UNRATESTx_lag3,UNRATESTx_lag2,
                      UNRATESTx_lag1,inflation_lag10,inflation_lag9,
                      inflation_lag8,inflation_lag7,inflation_lag6,
                      inflation_lag5,inflation_lag4,inflation_lag3,
                      inflation_lag2,inflation_lag1,FEDFUNDS_lag10,
                      FEDFUNDS_lag9,FEDFUNDS_lag8,FEDFUNDS_lag7,FEDFUNDS_lag6,
                      FEDFUNDS_lag5,FEDFUNDS_lag4,FEDFUNDS_lag3,FEDFUNDS_lag2,
                      FEDFUNDS_lag1,M1REAL_growth_lag10,M1REAL_growth_lag9,
                      M1REAL_growth_lag8,M1REAL_growth_lag7,M1REAL_growth_lag6,
                      M1REAL_growth_lag5,M1REAL_growth_lag4,M1REAL_growth_lag3,
                      M1REAL_growth_lag2,M1REAL_growth_lag1,SP500_growth_lag10,
                      SP500_growth_lag9, SP500_growth_lag8,SP500_growth_lag7,
                      SP500_growth_lag6,SP500_growth_lag5,SP500_growth_lag4,
                      SP500_growth_lag3,SP500_growth_lag2,SP500_growth_lag1)
  
  
}

#fit the regression tree

fit1 <- rpart(response ~ ., method = "anova", data = data3[,2:72])

fittet_v1 <- predict(fit1)

#plot the regression tree

pdf(file = "rt_big_plotoftree.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
## Run dev.off() to create the PDF file 
par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
rpart.plot(fit1, type=0, fallen.leaves=TRUE, 
           box.palette=c(),
           shadow.col = "grey", extra=101, digits = 4)
dev.off()

#get the observations in each node

node_rt_big <- fit1$where
node_num_rt_big <- data.frame(node_rt_big, round(fittet_v1, 3)) 
rownames(node_num_rt_big)<- data3$dates
node_num_rt_big

#node36 <- c()
#for (i in 1:241) {
#  if (node_rt_big[i]==5){
#    node36 <- c(node36, node_rt_big[i])    
#    
#  }
#}
#node36


################################################################################
#c)
#one-quarter-ahead forecasts of the two regression tree models
forecast_rt_big <- c()
for(i in 1:240) {
  tree<-rpart(response ~ ., method = "anova", data = data3[1:i,2:72])
  tree_forecast <- predict(tree,data3[1+i,2:72])
  forecast_rt_big <- c(forecast_rt_big, tree_forecast)    
}
length(forecast_rt_big)

forecast_rt_small <- c()
for(i in 1:240) {
  tree<-rpart(response ~ ., method = "anova", data = data3[1:i,2:12])
  tree_forecast <- predict(tree,data3[1+i,2:72])
  forecast_rt_small <- c(forecast_rt_small, tree_forecast)    
}
length(forecast_rt_small)

forecasts <- data.frame(data3$dates[2:241],data3$response[2:241], forecast_rt_small, forecast_rt_big)
colnames(forecasts) <- c("dates","GDP_per", "forecast_rt_small_1ahead", "forecast_rt_big_1ahead")

#calculate th RMSFEs of the regression trees and the var models

RMSE_RT_big_one_ahead <- sqrt(mean((forecasts$GDP_per - forecasts$forecast_rt_big_1ahead)^2))
RMSE_RT_big_one_ahead
#1.232489

RMSE_RT_small_one_ahead <- sqrt(mean((forecasts$GDP_per - forecasts$forecast_rt_small_1ahead)^2))
RMSE_RT_small_one_ahead
#1.258732

df_ar <- read.csv("data_forecast.csv")
RMSE_AR1 <- sqrt(mean((df_ar$GDP_per[9:248] - df_ar$forecast_ar[9:248])^2))
RMSE_AR1
#1.196388

df_var1 <- read.csv("data_var_forecast.csv")
RMSE_VAR1 <- sqrt(mean((df_var1$GDP_per[3:242] - df_var1$var_forecast[3:242])^2))
RMSE_VAR1
#1.974371

#RMSE random forest:
#1.214389

#plot the forecasts

rt_oneahead_plot <- ggplot(forecasts, aes(dates)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y = GDP_per, colour = "actual GDP growth"), linetype = "dashed") + 
  geom_line(aes(y = forecast_rt_small_1ahead, colour = "Based on GDP growth")) +
  geom_line(aes(y = forecast_rt_big_1ahead, colour = "Based on all variables")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("GDP growth in %") +
  theme(legend.position="bottom")
rt_oneahead_plot

ggsave("rt_oneahead_plot.pdf",plot = rt_oneahead_plot)



################################################################################
#d)

#calculate the variable importance measure through permutation

# ranger_forest_mspe <- ranger(response ~ ., data=data3[,2:72], importance = 'impurity',seed = 420)
ranger_forest_permutation <- ranger(response ~ ., data=data3[,2:72], importance = "permutation",seed = 420)

  # ranger_importance_mspe <- sort(ranger_forest_mspe$variable.importance, decreasing=TRUE)
ranger_importance_permutation <- 100*sort(ranger_forest_permutation$variable.importance, decreasing=TRUE)
  # ranger_importance_permutation2 <- sort(ranger::importance(ranger_forest_permutation), decreasing=TRUE)
#{
  # #plot for variable importance for mspe
  # ranger_importance_mspe_df1 <- data.frame(ranger_importance_mspe)
  # colnames(ranger_importance_mspe_df1) <- c("Variable_Importance")
  # ranger_importance_mspe_df <- data.frame(rownames(ranger_importance_mspe_df1),ranger_importance_mspe_df1$Variable_Importance)
  # colnames(ranger_importance_mspe_df) <- c("Variable","Variable_Importance")
  # ranger_importance_mspe_df$Variable <- factor(ranger_importance_mspe_df$Variable,levels = unique(ranger_importance_mspe_df$Variable),ordered = T)
  # 
  # ranger_importance_mspe_plot <- ggplot(ranger_importance_mspe_df[1:20,],aes(Variable, Variable_Importance))+
  #   geom_point(stat="identity") +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   xlab("Variable") + ylab("Variable Importance") +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #   theme(legend.position="bottom")
  # ranger_importance_mspe_plot
  # 
  # ggsave("ranger_importance_mspe_plot.pdf",plot = ranger_importance_mspe_plot)
#}

#plot the variable importance measures

#plot for variable importance for permutation
ranger_importance_permutation_df <- data.frame(ranger_importance_permutation)
colnames(ranger_importance_permutation_df) <- c("Variable_Importance")
ranger_importance_permutation_df <- data.frame(rownames(ranger_importance_permutation_df),ranger_importance_permutation_df$Variable_Importance)
colnames(ranger_importance_permutation_df) <- c("Variable","Variable_Importance")
ranger_importance_permutation_df$Variable <- factor(ranger_importance_permutation_df$Variable,levels = unique(ranger_importance_permutation_df$Variable),ordered = T)

# ranger_importance_permutation_df%>%
# #  mutate(Variable = fct_reorder(Variable, Variable_Importance)) %>%
#   ggplot( aes(y=Variable, x=Variable_Importance))+
#   geom_col()
# #  coord_flip()

ranger_importance_permutation_plot_points <- ggplot(ranger_importance_permutation_df[c(1:20),],aes(Variable_Importance, Variable))+
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("MSE increase in %") + ylab("Variable Importance") +
  theme(legend.position="bottom")
ranger_importance_permutation_plot_points

ggsave("ranger_importance_permutation_plot_points.pdf",plot = ranger_importance_permutation_plot_points)

# rF_forest <- randomForest(response ~ ., importance = TRUE,data=data3[,2:72],seed=420)
# 
# rF_forest_mspe <- sort(importance(rF_forest, type=2))
# rF_forest_permutation <- sort(importance(rF_forest, type=1))

################################################################################
#e)

#calculate the one-quarter-ahead forecast of the regression trees

#forest<-ranger(response ~ ., data = data3[1:2,2:72],seed = 420)
#tree_forecast <- predict(forest,data3[1+2,2:72])
#tree_forecast$predictions

forecast_ranger <- c()
for(i in 1:240) {
  forest<-ranger(response ~ ., data = data3[1:i,2:72],seed = 420)
  tree_forecast <- predict(forest,data3[1+i,2:72])
  forecast_ranger <- c(forecast_ranger, tree_forecast$predictions)    
}

#forecast_ranger[i+1]
forecasts2 <- data.frame(forecasts, forecast_ranger, df_ar$forecast_ar[9:248], df_var1$var_forecast[3:242])
colnames(forecasts2) <- c("dates","GDP_per", "forecast_rt_small_1ahead", "forecast_rt_big_1ahead", "forecast_rf_ranger","forecast_ar", "forecast_var")
write.csv(forecasts2,"C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies\\project 2\\rf_forecasts.csv", row.names = FALSE)


# forecast_rF <- c()
# set.seed(420)
# for(i in 1:240) {
#   forest<-randomForest(response ~ ., data = data3[1:i,2:72])
#   tree_forecast <- predict(forest,data3[1+i,2:72])
#   forecast_rF <- c(forecast_rF, tree_forecast)    
# }
# length(forecast_rF)
# 
# forecasts3 <- data.frame(forecasts2, forecast_rF)
# colnames(forecasts3) <- c("dates","GDP_per", "forecast_rt_small_1ahead", "forecast_rt_big_1ahead", "forecast_rt_small", "forecast_rt_big", "forecast_rf_ranger","forecast_rf_rF")


RMSE_rf_ranger_oneanhead <- sqrt(mean((forecasts2$GDP_per - forecasts2$forecast_rf_ranger)^2))
RMSE_rf_ranger_oneanhead
#1.214389
# RMSE_rf_rF_oneanhead <- sqrt(mean((forecasts3$GDP_per - forecasts3$forecast_rf_rF)^2))
# RMSE_rf_rF_oneanhead
# #1.249408

#plot the forecasts

rf_oneahead_plot <- ggplot(forecasts2, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth"), linetype = "dashed") + 
  geom_line(aes(y = forecast_rf_ranger, colour = "Random Forest all variables")) +
  geom_line(aes(y = forecast_rt_big_1ahead, colour = "Regression Tree all varibales")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("quarterly GDP growth in %") +
  theme(legend.position="bottom")
rf_oneahead_plot

ggsave("rf_oneahead_plot.pdf",plot = rf_oneahead_plot)

ar_and_var_plot <- ggplot(forecasts2, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth"), linetype = "dashed") + 
  geom_line(aes(y = forecast_ar, colour = "AR(1) process")) +
  geom_line(aes(y = forecast_var, colour = "VAR(1) process")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("quarterly GDP growth in %") +
  theme(legend.position="bottom")
ar_and_var_plot

ggsave("ar_and_var_plot.pdf",plot = ar_and_var_plot)

ar_plot <- ggplot(forecasts2, aes(dates)) + 
  geom_line(aes(y = GDP_per, colour = "actual GDP growth"), linetype = "dashed") + 
  geom_line(aes(y = forecast_ar, colour = "AR(1) process")) +
  geom_line(aes(y = forecast_rf_ranger, colour = "Radnom Forest all variables")) +
  geom_line(aes(y = forecast_rt_big, colour = "Regression Tree all variables")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("quarterly GDP growth in %") +
  theme(legend.position="bottom")
ar_plot

