#https://cran.r-project.org/web/packages/kerasR/vignettes/introduction.html

#Sys.setenv(PATH = paste("/home/key/software/anaconda3/envs/tf3.5/bin", Sys.getenv("PATH"), sep=":"))
Sys.getenv('PATH')
reticulate::use_condaenv("tf3.5", required = TRUE)
reticulate::py_available()
reticulate::py_config()
reticulate::py_module_available("keras") 
reticulate::import("keras.models")

library(kerasR)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


################################################################################################################
#                                             datasets                                                         #
################################################################################################################

traffic_df <- read_csv("internet-traffic-data-in-bits-fr.csv", col_names = c("hour", "bits"), skip = 1)

trend_train <- 11:110 + rnorm(100, sd = 4)
trend_test <- 111:130 + rnorm(20, sd =4)
#trend_test <- 31:50 + rnorm(20, sd =4)

seasonal_train <- rep(1:7, times = 13)
seasonal_test <- rep(1:7, times = 3)

ts_train <- trend_train
ts_test <- trend_test


################################################################################################################
#                                             configuration                                                    #
################################################################################################################


lstm_predict_sequences <- FALSE
lstm_num_predictions <- 5

# lstm_num_timesteps
lstm_num_timesteps <- 5
# lstm_num_features
lstm_num_features <- 1
# stateful?
lstm_stateful <- FALSE

batch_size <- 1
num_epochs <- 20
# dimensionality of the output space
num_neurons <- 4

# scale the dataset to values between scale_min and scale_max
scale <- FALSE
scale_min <- -1
scale_max <- 1

# number of consecutive (dependent) predictions
prediction_window <- 5


################################################################################################################
#                                             data prep                                                        #
################################################################################################################

# for testing
#ts_train <- c(111,222,333,444,555,666,777,888,999,1000)
#ts_test <- c(300,290,280,270,260,250)

X_train <- sapply(1:(length(ts_train) - lstm_num_timesteps), function(x) ts_train[x:(x + lstm_num_timesteps - 1)])
dim(X_train)
X_train <- t(X_train)
X_train
dim(X_train)

y_train <- sapply((lstm_num_timesteps + 1):(length(ts_train)), function(x) ts_train[x])
y_train

X_test <- sapply(1:(length(ts_test) - lstm_num_timesteps), function(x) ts_test[x:(x + lstm_num_timesteps - 1)])
dim(X_test)
X_test <- t(X_test)
X_test
dim(X_test)

y_test <- sapply((lstm_num_timesteps + 1):(length(ts_test)), function(x) ts_test[x])
y_test

# 3rd dim for LSTM
X_train <- expand_dims(X_train, axis = 2)
dim(X_train)

X_test <- expand_dims(X_test, axis = 2)
dim(X_test)


# LSTM input shape: (samples, time steps, features)
num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]
c(num_samples, num_steps, num_features)

################################################################################################################
#                                             model                                                            #
################################################################################################################

model <- Sequential()

model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$input_shape
model$output_shape
        
model$add(Dense(1))
model$input_shape
model$output_shape

keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = batch_size, epochs = 50, verbose = 1)


# model <- Sequential()
# 
# if (lstm_stateful) {
#   
#   print('stateful')
# 
#   if (lstm_predict_sequences) {
#     print(paste0('Using TimeDistributedDense to predict ', lstm_num_predictions, ' timesteps at once'))
#     model$add(LSTM(num_neurons,
#                    batch_input_shape=c(batch_size, X_train.shape[1], X_train.shape[2]),
#                    stateful = True,
#                    return_sequences = True))
#     model$add(TimeDistributed(Dense(1)))
#     model$add(Activation("linear"))  
#     model$compile(loss='mean_squared_error', optimizer='adam')
#     
#   }
#   else {
#     print('Using Dense to predict one timestep at a time')
#     model$add(LSTM(num_neurons,
#                    batch_input_shape=c(batch_size, dim(X_train,1),dim(X_train,2)),
#                    stateful = True))
#     model$add(Dense(1))
#     model$compile(loss='mean_squared_error', optimizer='adam')
#   }
# 
# } # stateful == False    
# else {
#   print('stateless')
#   
#   if (lstm_predict_sequences) {
#     print(paste0('Using TimeDistributedDense to predict ', lstm_num_predictions, ' timesteps at once'))
#     model$add(LSTM(num_neurons,
#                    #  input_dim = lstm_num_features,
#                    batch_input_shape=c(batch_size, dim(X_train,1),dim(X_train,2)),
#                    return_sequences = True))
#     model$add(TimeDistributed(Dense(1)))
#     model.compile(loss='mean_squared_error', optimizer='adam')
#   }
#     
#   else {
#     print('predict single')
#     model.add(LSTM(num_neurons, 
#                    input_shape=c(dim(X_train,1),dim(X_train,2))
#               ))
#   model.add(Dense(1))
#   model.compile(loss='mean_squared_error', optimizer='adam')
#   }
# }
  

################################################################################################################
#                                             predictions                                                      #
################################################################################################################

test_loss <- model$evaluate(X_test, y_test)
test_loss

pred_train <- keras_predict(model, X_train, batch_size = batch_size)
pred_train
pred_test <-keras_predict(model, X_test, batch_size = batch_size)
pred_test

dim(pred_train)
dim(pred_test)



comp_df <- data_frame(observed = c(ts_train, ts_test), predicted = c(rep(NA, lstm_num_timesteps), pred_train,
                                                                     rep(NA, lstm_num_timesteps), pred_test))
comp_df <- comp_df %>% mutate(id = as.numeric(row.names(comp_df)))
comp_df <- comp_df %>% gather(key = 'source', value = 'value', -id)

ggplot(comp_df, aes(x = id, y = value, color = source)) + geom_point()



