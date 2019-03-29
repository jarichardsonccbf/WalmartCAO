#Function to calculate interesting metrics for each dataset----
CalculateErrors <- function(df) {
  Percent.Perfect.Match = df %>% 
    filter(Case.Difference == 0) %>% 
    count() / nrow(df) * 100
  
  Percent.Perfect.Match <- rename(Percent.Perfect.Match, Percent.Perfect.Match = n)
  
  ME = mean(df$Case.Difference)
  
  RMSE = sqrt(mean((df$Weekly.AM.Order.Cases - df$Weekly.GRS.Order.Cases)^2))
  
  RAE = rae(df$Weekly.AM.Order.Cases, df$Weekly.GRS.Order.Cases)
  
  RSE = rse(df$Weekly.AM.Order.Cases, df$Weekly.GRS.Order.Cases)
  
  MAE = mean(abs(df$Case.Difference))
  
  CoD = summary(lm(df$Weekly.GRS.Order.Cases ~ df$Weekly.AM.Order.Cases))$r.squared
  
  vec = cbind(Percent.Perfect.Match, ME, MAE, RMSE, RAE, RSE, CoD)
  
  return(vec)
}
