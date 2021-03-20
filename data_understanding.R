#

germanCS <- read.csv("GermanCreditScoring.csv", stringsAsFactors = TRUE)
View(germanCS)
summary(germanCS)

plot(germanCS)
plot(germanCS, col = germanCS$credit_risk)

#getting an graphical overview of all conditional probabilities for a specific variable
plot_all_ccs <- function(dataframe, base_col, ploting = "cc_barplot", breaks = 5){
  int <- 2
  real_var_num <- 0
  plot_theme <- theme(axis.text=element_text(size=7),axis.title=element_text(size=7,face="bold"),strip.text = element_text(size = 7))
  df_col_num <- ncol(dataframe)
  plot_list <- list()
  
  while (df_col_num%%int != 0){int <- int + 1}
  for (name in colnames(dataframe)){
    name_class <- class(dataframe[[as.character(name)]])
    if (name_class == "numerc" || name_class == "integer"){
      real_var_num <- real_var_num + 1
    }
  }
  
  if (real_var_num >= 4){
    while (real_var_num%%int != 0){int <- int + 1}
  }else if (real_var_num != 0){
    int <- 1
  }
  par(mfrow = c(real_var_num/int,int))
  
    for (name in colnames(dataframe)){
      name_class <- class(dataframe[[as.character(name)]])
      if (as.character(name) != as.character(base_col)){
        if (ploting == "cc_barplot"){
          if (name_class != "numeric" && name_class != "integer"){
            plot_list[[length(plot_list)+1]] <- cc_barplot(Data = dataframe, x = as.character(name), y = as.character(base_col), freq = "condprob")+plot_theme
          }
        }else if (ploting == "cc_boxplot"){
          if (name_class == "numeric" || name_class == "integer"){
            plot_list[[length(plot_list)+1]] <- cc_boxplot(Data = dataframe, x = as.character(name), y = as.character(base_col))+plot_theme
          }
        }else if (ploting == "cc_hist"){
          if (name_class == "numeric" || name_class == "integer"){
            cc_hist(Data = dataframe, x = as.character(name), y = as.character(base_col), breaks = breaks)
          }
        }else if (ploting == "densityplot"){
          if (name_class == "numeric" || name_class == "integer"){
            plot_list[[length(plot_list)+1]] <- densityplot(~ dataframe[[as.character(name)]], data = dataframe, groups = dataframe[[as.character(base_col)]], auto.key = TRUE)
          }
        }
      }
    }    
  View(plot_list)
  if(ploting != "cc_hist"){do.call(grid.arrange, plot_list)}
}

plot_all_ccs(germanCS,"credit_risk","cc_barplot")
plot_all_ccs(germanCS, "credit_risk", "cc_boxplot")
plot_all_ccs(germanCS, "credit_risk", "cc_hist", 200)
plot_all_ccs(germanCS, "credit_risk", "densityplot")


#specific variable analysis
#Variable: credit_risk
barp(germanCS, "credit_risk")
barp(germanCS, "credit_risk", freq = "relfreq", )

#Variable: duration
hist(germanCS$duration,breaks = c(1:76), xaxp = c(0,80,40), ylim = c(0, 200))
cc_hist(germanCS, "duration", "credit_risk", breaks = 80, xaxp = x(0,80,40), ylim = c(0,200))


#Variable: amount
densityplot(~ amount, data = germanCS, groups = credit_risk, auto.key = TRUE)
