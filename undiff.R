lstm_num_timesteps <-3 
(test <- c(1,3,5,7,9,11,13))

(test_diff <- diff(test))
(test_undiff <- diffinv(test_diff, xi=test[1]))

(X_test <- t(sapply(1:(length(test_diff) - lstm_num_timesteps), function(x) test_diff[x:(x + lstm_num_timesteps - 1)])))
(y_test <- sapply((lstm_num_timesteps + 1):(length(test_diff)), function(x) test_diff[x]))

preds <- c(2.1,2.1,2.1)

(pred_test_undiff <- preds + test[(lstm_num_timesteps+1):(length(test)-1)])

# falsch!! ----- diffinv(preds, xi = test[(lstm_num_timesteps+1)])


#####

(test <- c(1,3,5,7,9,11,13))

(test_rel_diff <- diff(test)/test[-length(test)])

(test_rel_undiff <- diffinv(test_rel_diff * test[-length(test)], xi=test[1]))

(X_test <- t(sapply(1:(length(test_diff) - lstm_num_timesteps), function(x) test_rel_diff[x:(x + lstm_num_timesteps - 1)])))
(y_test <- sapply((lstm_num_timesteps + 1):(length(test_diff)), function(x) test_rel_diff[x]))

preds <- c(0.2857143, 0.2222222, 0.1818182)
(pred_test_rel_undiff <- preds * test[(lstm_num_timesteps+1):(length(test)-1)] + test[(lstm_num_timesteps+1):(length(test)-1)])


