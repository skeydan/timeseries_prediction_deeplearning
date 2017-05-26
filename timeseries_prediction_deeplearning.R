
reticulate::use_condaenv("tf3.5", required = TRUE)
library(kerasR)
library(forecast)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(ggfortify)
library(tidyr)
library(lubridate)

# ========================================================
cola_df <- read_csv("monthly-sales-of-tasty-cola.csv", col_names = c("month", "sales"), skip = 1,
                    col_types = cols(month = col_date("%y-%m")))
ggplot(cola_df, aes(x = month, y = sales)) + geom_line() + ggtitle("Monthly sales of Tasty Cola")


# ========================================================
traffic_df <- read_csv("internet-traffic-data-in-bits-fr.csv", col_names = c("hour", "bits"), skip = 1)
ggplot(traffic_df, aes(x = hour, y = bits)) + geom_line() + ggtitle("Internet traffic")


# ========================================================
win_df <- read_csv("winning-times-for-the-mens-400-m.csv", col_names = c("year", "seconds"), skip = 1)
ggplot(win_df, aes(x = year, y = seconds)) + geom_line() + ggtitle("Men's 400m winning times")

# ========================================================
deaths_df <- read_csv("deaths-from-homicides-and-suicid.csv", col_names = c("year", "homicide", "suicide"), skip = 1)
deaths_df <- gather(deaths_df, key = 'type', value = 'deaths', homicide:suicide)
ggplot(deaths_df, aes(x = year, y = deaths, color = type)) + geom_line() + scale_colour_manual(values=c("green","blue")) + ggtitle("Australia: Homicides and suicides")

# ========================================================
data("lynx")
autoplot(lynx) + ggtitle("Lynx population over time")

# ========================================================
  
# ========================================================
lynx_df <- read_delim("lynxhare.csv", delim = ";") %>% select(year, hare, lynx) %>%   filter(between(year, 1890, 1945)) %>% mutate(hare = scale(hare), lynx = scale(lynx))
lynx_df <- gather(lynx_df, key = 'species', value = 'number', hare:lynx)
ggplot(lynx_df, aes(x = year, y = number, color = species)) + geom_line() + scale_colour_manual(values=c("green","red")) + ggtitle("Lynx and hare populations over time")


# ========================================================
set.seed(7777)
trend <- 1:100 + rnorm(100, sd = 5)
diff <- diff(trend)
df <- data_frame(time_id = 1:100,
                 trend = trend,
                 diff = c(NA, diff))
df <- df %>% gather(key = 'trend_diff', value = 'value', -time_id)
ggplot(df, aes(x = time_id, y = value, color = trend_diff)) + geom_line()

# ========================================================
s1 <- ts(rnorm(100))
ts1 <- autoplot(s1)
acf1 <- ggfortify:::autoplot.acf(acf(s1, plot = FALSE), conf.int.fill = '#00cccc', conf.int.value = 0.95)
do.call('grid.arrange', list('grobs' = list(ts1, acf1), 'ncol' = 2, top = "White noise"))

# ========================================================
s2 <- ts(1:100 + rnorm(100, 2, 4))
ts2 <- autoplot(s2)
acf2 <- ggfortify:::autoplot.acf(acf(s2, plot = FALSE), conf.int.fill = '#00cccc', conf.int.value = 0.95)
do.call('grid.arrange', list('grobs' = list(ts2, acf2), 'ncol' = 2, top = "Series with a trend"))

# ========================================================
s3 <- ts(rep(1:5,20) + rnorm(100, sd= 0.5))
ts3 <- autoplot(s3)
acf3 <- ggfortify:::autoplot.acf(acf(s3, plot = FALSE), conf.int.fill = '#00cccc', conf.int.value = 0.95)
do.call('grid.arrange', list('grobs' = list(ts3, acf3), 'ncol' = 2, top = "Series with seasonality"))


########################################################################################################
# Internet traffic part 1: ARIMA-like
########################################################################################################


#========================================================
ggplot(traffic_df, aes(x = hour, y = bits)) + geom_line() + ggtitle("Internet traffic")


# ========================================================
traffic_ts <- msts(traffic_df$bits,seasonal.periods = c(24, 24*7))
autoplot(stl(traffic_ts, s.window = 7 * 24))

# ========================================================
ggfortify:::autoplot.acf(acf(traffic_ts, plot = FALSE), conf.int.fill = '#00cccc', conf.int.value = 0.95)

# ========================================================
#arima_fit <- auto.arima(traffic_ts, stepwise = FALSE, max.order = 10, trace = TRUE)

# ========================================================
traffic_df_wd <- traffic_df %>% mutate(weekend = if_else(wday(hour) %in% c(7,1), 1, 0))
ggplot(traffic_df_wd, aes(x=hour, y=bits, color=factor(weekend))) + geom_point()
#arima_fit <- auto.arima(ts(traffic_df_wd$bits, frequency = 24 * 7), xreg = traffic_df_wd$weekend, 
#stepwise = FALSE, max.order = 10, trace = TRUE)

# ========================================================
tbats_fit <- tbats(traffic_ts)
plot(forecast(tbats_fit, h=7*24))


########################################################################################################
# trend (test set out-of-range)
########################################################################################################

# create linear trend out-of-range dataset
#========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)
df <- data_frame(time_id = 1:120,
train = c(trend_train, rep(NA, length(trend_test))),
test = c(rep(NA, length(trend_train)), trend_test))
df <- df %>% gather(key = 'train_test', value = 'value', -time_id)
ggplot(df, aes(x = time_id, y = value, color = train_test)) + geom_line()


# auto.arima on linear trend out-of-range dataset
# ========================================================

set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)
h <- 1
n <- length(trend_test) - h + 1
fit <- auto.arima(trend_train)

# re-estimate the model as new data arrives, as per https://robjhyndman.com/hyndsight/rolling-forecasts/ 
order <- arimaorder(fit)
predictions <- matrix(0, nrow=n, ncol=h)
lower <- matrix(0, nrow=n, ncol=h) # 95% prediction interval
upper <- matrix(0, nrow=n, ncol=h)
for(i in 1:n) {  
  x <- c(trend_train[(1+i):length(trend_train)], trend_test[1:i])
  refit <- Arima(x, order=order[1:3], seasonal=order[4:6])
  predictions[i,] <- forecast(refit, h=h)$mean
  lower[i,] <- unclass(forecast(refit, h=h)$lower)[,2]
  upper[i,] <- unclass(forecast(refit, h=h)$upper)[,2]
}

(test_rsme <- sqrt(sum((trend_test - predictions)^2))) 

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 fitted = c(fit$fitted, rep(NA, length(trend_test))),
                 preds = c(rep(NA, length(trend_train)), predictions),
                 lower = c(rep(NA, length(trend_train)), lower),
                 upper = c(rep(NA, length(trend_train)), upper))
df <- df %>% gather(key = 'type', value = 'value', train:preds)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)

# lstm on linear trend out-of-range dataset: NODIFF
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)
lstm_num_timesteps <- 5

X_train <- t(sapply(1:(length(trend_train) - lstm_num_timesteps), function(x) trend_train[x:(x + lstm_num_timesteps - 1)]))
dim(X_train)
X_train[1:5, ]
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train)), function(x) trend_train[x])
y_train[1:5]
X_test <- t(sapply(1:(length(trend_test) - lstm_num_timesteps), function(x) trend_test[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test)), function(x) trend_test[x])
# add 3rd dimension
dim(X_train)
X_train <- expand_dims(X_train, axis = 2)
dim(X_train)
X_test <- expand_dims(X_test, axis = 2)
dim(X_test)
# LSTM input shape: (samples, time steps, features)
num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]
c(num_samples, num_steps, num_features)

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')

keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_nodiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
df <- data_frame(time_id = 1:120,
train = c(trend_train, rep(NA, length(trend_test))),
test = c(rep(NA, length(trend_train)), trend_test),
pred_train = c(rep(NA, lstm_num_timesteps), pred_train, rep(NA, length(trend_test))),
pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps), pred_test))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps) - pred_test)^2))
test_rsme # 76.28


# lstm on linear trend out-of-range dataset: DIFF
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)
trend_test_diff <- diff(trend_test)

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_diff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
train = c(trend_train, rep(NA, length(trend_test))),
test = c(rep(NA, length(trend_train)), trend_test),
pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 7.06


# lstm on linear trend out-of-range dataset: REL DIFF
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)/trend_train[-length(trend_train)]
trend_test_diff <- diff(trend_test)/trend_test[-length(trend_test)]

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_reldiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train * trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)] + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test * trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)] + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 8.27


# lstm on linear trend out-of-range dataset: DIFF + NORMALIZE 
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 111:130 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)
trend_test_diff <- diff(trend_test)

minval <- min(trend_train_diff)
maxval <- max(trend_train_diff)

normalize <- function(vec, min, max) {
  (vec-min) / (max-min)
}
denormalize <- function(vec,min,max) {
  vec * (max - min) + min
}

trend_train_diff <- normalize(trend_train_diff, minval, maxval)
trend_test_diff <- normalize(trend_test_diff, minval, maxval)

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_diffnorm.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)

pred_train <- denormalize(pred_train, minval, maxval)
pred_test <- denormalize(pred_test, minval, maxval)

pred_train_undiff <- pred_train + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 6.84


########################################################################################################
# trend (test set in-range)
########################################################################################################


# create linear trend in-range dataset
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)
df <- data_frame(time_id = 1:120,
train = c(trend_train, rep(NA, length(trend_test))),
test = c(rep(NA, length(trend_train)), trend_test))
df <- df %>% gather(key = 'train_test', value = 'value', -time_id)
ggplot(df, aes(x = time_id, y = value, color = train_test)) + geom_line()


# auto.arima on linear trend in-range dataset
#========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)
fit <- auto.arima(trend_train)
order <- arimaorder(fit)
# fit on the test set
refit <- Arima(trend_test, order=order[1:3], seasonal=order[4:6])
predictions <- refit$fitted
(test_rsme <- sqrt(sum((trend_test - predictions)^2))) 

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 fitted = c(fit$fitted, rep(NA, length(trend_test))),
                 preds = c(rep(NA, length(trend_train)), predictions))
df <- df %>% gather(key = 'type', value = 'value', train:preds)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type)) 


# lstm on linear trend in-range dataset: NODIFF
#========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)

lstm_num_timesteps <- 5

X_train <- t(sapply(1:(length(trend_train) - lstm_num_timesteps), function(x) trend_train[x:(x + lstm_num_timesteps - 1)]))
dim(X_train)
X_train[1:5, ]
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train)), function(x) trend_train[x])
y_train[1:5]
X_test <- t(sapply(1:(length(trend_test) - lstm_num_timesteps), function(x) trend_test[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test)), function(x) trend_test[x])
# add 3rd dimension
dim(X_train)
X_train <- expand_dims(X_train, axis = 2)
dim(X_train)
X_test <- expand_dims(X_test, axis = 2)
dim(X_test)
# LSTM input shape: (samples, time steps, features)
num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]
c(num_samples, num_steps, num_features)

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')

keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_i_nodiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps), pred_train, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps), pred_test))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps) - pred_test)^2))
test_rsme # 9.18


# lstm on linear trend in-range dataset: DIFF
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)
trend_test_diff <- diff(trend_test)

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_i_diff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 10.03


# lstm on linear trend in-range dataset: REL DIFF
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)/trend_train[-length(trend_train)]
trend_test_diff <- diff(trend_test)/trend_test[-length(trend_test)]

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_i_reldiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train * trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)] + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test * trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)] + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 7.25


# lstm on linear trend in-range dataset: DIFF + NORMALIZE 
# ========================================================
set.seed(7777)
trend_train <- 11:110 + rnorm(100, sd = 2)
trend_test <- 31:50 + rnorm(20, sd =2)

trend_train_start <- trend_train[1]
trend_test_start <- trend_test[1]

trend_train_diff <- diff(trend_train)
trend_test_diff <- diff(trend_test)

minval <- min(trend_train_diff)
maxval <- max(trend_train_diff)

normalize <- function(vec, min, max) {
  (vec-min) / (max-min)
}
denormalize <- function(vec,min,max) {
  vec * (max - min) + min
}

trend_train_diff <- normalize(trend_train_diff, minval, maxval)
trend_test_diff <- normalize(trend_test_diff, minval, maxval)

#!!
lstm_num_timesteps <- 4
#diffinv(trend_train_diff, xi=trend_train_start)

X_train <- t(sapply(1:(length(trend_train_diff) - lstm_num_timesteps), function(x) trend_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(trend_train_diff)), function(x) trend_train_diff[x])
X_test <- t(sapply(1:(length(trend_test_diff) - lstm_num_timesteps), function(x) trend_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(trend_test_diff)), function(x) trend_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'trend_i_diffnorm.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)

pred_train <- denormalize(pred_train, minval, maxval)
pred_test <- denormalize(pred_test, minval, maxval)

pred_train_undiff <- pred_train + trend_train[(lstm_num_timesteps+1):(length(trend_train)-1)]
pred_test_undiff <- pred_test + trend_test[(lstm_num_timesteps+1):(length(trend_test)-1)]

df <- data_frame(time_id = 1:120,
                 train = c(trend_train, rep(NA, length(trend_test))),
                 test = c(rep(NA, length(trend_train)), trend_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(trend_test))),
                 pred_test = c(rep(NA, length(trend_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(trend_test,length(trend_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 6.51



########################################################################################################
# seasonal
########################################################################################################


# create seasonal dataset
# ========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)
df <- data_frame(time_id = 1:112,
train = c(seasonal_train, rep(NA, length(seasonal_test))),
test = c(rep(NA, length(seasonal_train)), seasonal_test))
df <- df %>% gather(key = 'train_test', value = 'value', -time_id)
ggplot(df, aes(x = time_id, y = value, color = train_test)) + geom_line()


# auto.arima on seasonal dataset
# ========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)
h <- 1
n <- length(seasonal_test) - h + 1
fit <- auto.arima(seasonal_train)
order <- arimaorder(fit)
refit <- Arima(seasonal_test, order=order[1:3], seasonal=order[4:6])
predictions <- refit$fitted
(test_rsme <- sqrt(sum((seasonal_test - predictions)^2))) 

df <- data_frame(time_id = 1:112,
                 train = c(seasonal_train, rep(NA, length(seasonal_test))),
                 test = c(rep(NA, length(seasonal_train)), seasonal_test),
                 fitted = c(fit$fitted, rep(NA, length(seasonal_test))),
                 preds = c(rep(NA, length(seasonal_train)), predictions))
df <- df %>% gather(key = 'type', value = 'value', train:preds)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type)) 


# lstm on seasonal dataset: NODIFF
#========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)

lstm_num_timesteps <- 7

X_train <- t(sapply(1:(length(seasonal_train) - lstm_num_timesteps), function(x) seasonal_train[x:(x + lstm_num_timesteps - 1)]))
dim(X_train)
X_train[1:5, ]
y_train <- sapply((lstm_num_timesteps + 1):(length(seasonal_train)), function(x) seasonal_train[x])
y_train[1:5]
X_test <- t(sapply(1:(length(seasonal_test) - lstm_num_timesteps), function(x) seasonal_test[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(seasonal_test)), function(x) trend_test[x])
# add 3rd dimension
dim(X_train)
X_train <- expand_dims(X_train, axis = 2)
dim(X_train)
X_test <- expand_dims(X_test, axis = 2)
dim(X_test)
# LSTM input shape: (samples, time steps, features)
num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]
c(num_samples, num_steps, num_features)

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')

keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'seasonal_nodiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
df <- data_frame(time_id = 1:112,
                 train = c(seasonal_train, rep(NA, length(seasonal_test))),
                 test = c(rep(NA, length(seasonal_train)), seasonal_test),
                 pred_train = c(rep(NA, lstm_num_timesteps), pred_train, rep(NA, length(seasonal_test))),
                 pred_test = c(rep(NA, length(seasonal_train)), rep(NA, lstm_num_timesteps), pred_test))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(seasonal_test,length(seasonal_test) - lstm_num_timesteps) - pred_test)^2))
test_rsme # 0.84


# lstm on seasonal dataset: DIFF
# ========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)

seasonal_train_start <- seasonal_train[1]
seasonal_test_start <- seasonal_test[1]

seasonal_train_diff <- diff(seasonal_train)
seasonal_test_diff <- diff(seasonal_test)

#!!
lstm_num_timesteps <- 7
#diffinv(seasonal_train_diff, xi=seasonal_train_start)

X_train <- t(sapply(1:(length(seasonal_train_diff) - lstm_num_timesteps), function(x) seasonal_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(seasonal_train_diff)), function(x) seasonal_train_diff[x])
X_test <- t(sapply(1:(length(seasonal_test_diff) - lstm_num_timesteps), function(x) seasonal_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(seasonal_test_diff)), function(x) seasonal_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'seasonal_diff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train + seasonal_train[(lstm_num_timesteps+1):(length(seasonal_train)-1)]
pred_test_undiff <- pred_test + seasonal_test[(lstm_num_timesteps+1):(length(seasonal_test)-1)]

df <- data_frame(time_id = 1:112,
                 train = c(seasonal_train, rep(NA, length(seasonal_test))),
                 test = c(rep(NA, length(seasonal_train)), seasonal_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(seasonal_test))),
                 pred_test = c(rep(NA, length(seasonal_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(seasonal_test,length(seasonal_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 1.59


# lstm on seasonal in-range dataset: REL DIFF
# ========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)

seasonal_train_start <- seasonal_train[1]
seasonal_test_start <- seasonal_test[1]

seasonal_train_diff <- diff(seasonal_train)/seasonal_train[-length(seasonal_train)]
seasonal_test_diff <- diff(seasonal_test)/seasonal_test[-length(seasonal_test)]

#!!
lstm_num_timesteps <- 7
#diffinv(seasonal_train_diff, xi=seasonal_train_start)

X_train <- t(sapply(1:(length(seasonal_train_diff) - lstm_num_timesteps), function(x) seasonal_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(seasonal_train_diff)), function(x) seasonal_train_diff[x])
X_test <- t(sapply(1:(length(seasonal_test_diff) - lstm_num_timesteps), function(x) seasonal_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(seasonal_test_diff)), function(x) seasonal_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'seasonal_reldiff.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)
pred_train_undiff <- pred_train * seasonal_train[(lstm_num_timesteps+1):(length(seasonal_train)-1)] + seasonal_train[(lstm_num_timesteps+1):(length(seasonal_train)-1)]
pred_test_undiff <- pred_test * seasonal_test[(lstm_num_timesteps+1):(length(seasonal_test)-1)] + seasonal_test[(lstm_num_timesteps+1):(length(seasonal_test)-1)]

df <- data_frame(time_id = 1:112,
                 train = c(seasonal_train, rep(NA, length(seasonal_test))),
                 test = c(rep(NA, length(seasonal_train)), seasonal_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(seasonal_test))),
                 pred_test = c(rep(NA, length(seasonal_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(seasonal_test,length(seasonal_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 0.99


# lstm on seasonal in-range dataset: DIFF + NORMALIZE 
# ========================================================
set.seed(7777)
seasonal_train <- rep(1:7, times = 13) + rnorm(91, sd=0.2)
seasonal_test <- rep(1:7, times = 3) + rnorm(21, sd=0.2)

seasonal_train_start <- seasonal_train[1]
seasonal_test_start <- seasonal_test[1]

seasonal_train_diff <- diff(seasonal_train)
seasonal_test_diff <- diff(seasonal_test)

minval <- min(seasonal_train_diff)
maxval <- max(seasonal_train_diff)

normalize <- function(vec, min, max) {
  (vec-min) / (max-min)
}
denormalize <- function(vec,min,max) {
  vec * (max - min) + min
}

seasonal_train_diff <- normalize(seasonal_train_diff, minval, maxval)
seasonal_test_diff <- normalize(seasonal_test_diff, minval, maxval)

#!!
lstm_num_timesteps <- 7
#diffinv(seasonal_train_diff, xi=seasonal_train_start)

X_train <- t(sapply(1:(length(seasonal_train_diff) - lstm_num_timesteps), function(x) seasonal_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(seasonal_train_diff)), function(x) seasonal_train_diff[x])
X_test <- t(sapply(1:(length(seasonal_test_diff) - lstm_num_timesteps), function(x) seasonal_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(seasonal_test_diff)), function(x) seasonal_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'seasonal_diffnorm.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)

pred_train <- denormalize(pred_train, minval, maxval)
pred_test <- denormalize(pred_test, minval, maxval)

pred_train_undiff <- pred_train + seasonal_train[(lstm_num_timesteps+1):(length(seasonal_train)-1)]
pred_test_undiff <- pred_test + seasonal_test[(lstm_num_timesteps+1):(length(seasonal_test)-1)]

df <- data_frame(time_id = 1:112,
                 train = c(seasonal_train, rep(NA, length(seasonal_test))),
                 test = c(rep(NA, length(seasonal_train)), seasonal_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(seasonal_test))),
                 pred_test = c(rep(NA, length(seasonal_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(seasonal_test,length(seasonal_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme # 1.00


########################################################################################################
# Internet traffic part 2: LSTM
########################################################################################################


#========================================================
ggplot(traffic_df, aes(x = hour, y = bits)) + geom_line() + ggtitle("Internet traffic")


# ========================================================
traffic_train <- traffic_df$bits[1:800]
traffic_test <- traffic_df$bits[801:nrow(traffic_df)]

traffic_train_start <- traffic_train[1]
traffic_test_start <- traffic_test[1]

traffic_train_diff <- diff(traffic_train)
traffic_test_diff <- diff(traffic_test)

minval <- min(traffic_train_diff)
maxval <- max(traffic_train_diff)

normalize <- function(vec, min, max) {
  (vec-min) / (max-min)
}
denormalize <- function(vec,min,max) {
  vec * (max - min) + min
}

traffic_train_diff <- normalize(traffic_train_diff, minval, maxval)
traffic_test_diff <- normalize(traffic_test_diff, minval, maxval)

lstm_num_timesteps <- 7*24

X_train <- t(sapply(1:(length(traffic_train_diff) - lstm_num_timesteps), function(x) traffic_train_diff[x:(x + lstm_num_timesteps - 1)]))
y_train <- sapply((lstm_num_timesteps + 1):(length(traffic_train_diff)), function(x) traffic_train_diff[x])
X_test <- t(sapply(1:(length(traffic_test_diff) - lstm_num_timesteps), function(x) traffic_test_diff[x:(x + lstm_num_timesteps - 1)]))
y_test <- sapply((lstm_num_timesteps + 1):(length(traffic_test_diff)), function(x) traffic_test_diff[x])

X_train <- expand_dims(X_train, axis = 2)
X_test <- expand_dims(X_test, axis = 2)

num_samples <- dim(X_train)[1]
num_steps <- dim(X_train)[2]
num_features <- dim(X_train)[3]

model <- Sequential()
model$add(LSTM(units = 4, input_shape=c(num_steps, num_features)))
model$add(Dense(1))
keras_compile(model, loss='mean_squared_error', optimizer='adam')
keras_fit(model, X_train, y_train, batch_size = 1, epochs = 500, verbose = 1)
keras_save(model, 'traffic_diffnorm.h5')

pred_train <- keras_predict(model, X_train, batch_size = 1)
pred_test <-keras_predict(model, X_test, batch_size = 1)

pred_train <- denormalize(pred_train, minval, maxval)
pred_test <- denormalize(pred_test, minval, maxval)

pred_train_undiff <- pred_train + traffic_train[(lstm_num_timesteps+1):(length(traffic_train)-1)]
pred_test_undiff <- pred_test + traffic_test[(lstm_num_timesteps+1):(length(traffic_test)-1)]

df <- data_frame(time_id = 1:1231,
                 train = c(traffic_train, rep(NA, length(traffic_test))),
                 test = c(rep(NA, length(traffic_train)), traffic_test),
                 pred_train = c(rep(NA, lstm_num_timesteps+1), pred_train_undiff, rep(NA, length(traffic_test))),
                 pred_test = c(rep(NA, length(traffic_train)), rep(NA, lstm_num_timesteps+1), pred_test_undiff))
df <- df %>% gather(key = 'type', value = 'value', train:pred_test)
ggplot(df, aes(x = time_id, y = value)) + geom_line(aes(color = type))

test_rsme <- sqrt(sum((tail(traffic_test,length(traffic_test) - lstm_num_timesteps - 1) - pred_test_undiff)^2))
test_rsme 
