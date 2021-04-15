

colnames(Default["fa"])
colnames(Default$default)
substitute(table(Default$default))


bi_variat_log <- function(data_frame, class_var, desc_var, threshold){
  glm_model <- glm(class_var ~ desc_var, data = data_frame, family = binomial)
  x <- seq(min(desc_var), max(desc_var))
  plot(desc_var, class_var, xlab = str_interp("${deparse(substitute(desc_var))}"), ylab = str_interp("${deparse(substitute(class_var))}"))
  hat.beta <- coef(glm_model)
  lines(x,1/(1+exp(-hat.beta[1]-hat.beta[2]*x)), col = "blue")
  probs <- predict(glm_model, newdata = data_frame, type = "response")
  class.pred <- 1*(probs>threshold)
  truth_table <- table(class_var,class.pred)
  print(truth_table)
  error <- (truth_table[1,2]+truth_table[2,1])/sum(truth_table)
  print(str_interp("prediction-error: ${error}"))
  print(str_interp("prediction-accuracy: ${1-error}"))
}

#Default <- read.csv("Coursework/Week\ 15\ Logistic\ Regression-20210413/Default.csv", stringsAsFactors = T)
bi_variat_log(Default, Default$num_default, Default$balance, 0.5)


tri_variat_log <- function(data_frame, class_var, desc_var01, desc_var02, threshold){
  colors <- c("#00AFBB", "#E7B800")
  colors <- colors[as.numeric(as.factor(class_var))]
  shapes <- c(3, 17)
  shapes <- shapes[as.numeric(as.factor(class_var))]
  effective_th <- 1/threshold2-1
  glm_model <- glm(class_var ~ desc_var01 + desc_var02, data = data_frame, family = binomial)
  plot(desc_var01, desc_var02, col = colors, pch = shapes, xlab = str_interp("${deparse(substitute(desc_var01))}"), ylab = str_interp("${deparse(substitute(desc_var02))}"))
  hat.beta <- coef(glm_model)
  abline(a=(-hat.beta[1]-log(effective_th))/hat.beta[3], b=-hat.beta[2]/hat.beta[3], col="blue")
  probs <- predict(glm_model, newdata = data_frame, type = "response")
  class.pred <- 1*(probs>threshold)
  truth_table <- table(class_var,class.pred)
  print(truth_table)
  error <- (truth_table[1,2]+truth_table[2,1])/sum(truth_table)
  print(str_interp("prediction-error: ${error}"))
  print(str_interp("prediction-accuracy: ${1-error}"))
}

tri_variat_log(Default, Default$num_default, Default$balance, Default$income,0.1)




