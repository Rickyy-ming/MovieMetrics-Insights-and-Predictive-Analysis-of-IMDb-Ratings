library(dplyr)
#install.packages("glmnet") # if installation needed
library(glmnet)
library(boot)
library(ggpubr)
library(stargazer)
library(ggplot2)
require(methods)
require(psych)
require(lmtest)
require(plm)
library(car)
require(car)
library(splines)

###Loading Dataset
movies = read.csv("IMDB_data_Fall_2024.csv")
attach(movies) 
par(mfrow=c(1,1))
## Step 2 Explore variables individually

summary(movies)
### imdb_score
boxplot(imdb_score, col="blue", main="Boxplot of IMDb Score", 
        ylab="IMDb Score")
hist(imdb_score, col="blue", breaks=40, main="Histogram of IMDb Score", 
     xlab="IMDb Score", ylab="Frequency")
#Density plot for imdb score
plot(density(imdb_score), col="blue", lwd=2, 
     main="Density Plot of IMDb Score", 
     xlab="IMDb Score", ylab="Density")

### nb_faces 
boxplot(nb_faces, col="#39BEB1", main="Boxplot of Number of Faces", 
        ylab="Number of Faces")
hist(nb_faces, col="#39BEB1", breaks=100, main="Histogram of Number of Faces", 
     xlab="Number of Faces", ylab="Frequency")
movies$movie_meter_IMDBpro <- log(movies$movie_meter_IMDBpro)


### duration
boxplot(duration, col="darkgreen", main="Boxplot of Movie Duration", 
        ylab="Duration (minutes)")
hist(duration, col="darkgreen", breaks=60, main="Histogram of Movie Duration", 
     xlab="Duration (minutes)", ylab="Frequency")

# Scatter plot of duration vs IMDb score with trend line
plot(duration, imdb_score, col="darkgreen", pch=19,
     main="Scatter Plot of Duration vs IMDb Score",
     xlab="Duration (in minutes)", ylab="IMDb Score")

# Add a trend line (linear model)
abline(lm(imdb_score ~ duration), col="red", lwd=2)


### movie_budget
boxplot(movie_budget, col="purple", main="Boxplot of Movie Budget", 
        ylab="Movie Budget (Scaled)")
hist(movies$movie_budget, col="purple", breaks=40, main="Histogram of Movie Budget", 
     xlab="Movie Budget (Scaled)", ylab="Frequency")
#movies$movie_budget <- log(movies$movie_budget)

#hist(movies$movie_budget, col="purple", breaks=40, main="Histogram of Log-Transformed Movie Budget", 
#     xlab="Log-Transformed Movie Budget", ylab="Frequency")

# Scatter plot for movie_budget and imdb_score
plot(movies$movie_budget, imdb_score, col="purple", pch=19,
     main="Scatter Plot of Movie Budget vs IMDb Score",
     xlab="Movie Budget (Log-Transformed)", ylab="IMDb Score")

# Add a trend line (linear model)
abline(lm(imdb_score ~ movies$movie_budget), col="red", lwd=2)


### nb_news_articles
boxplot(movies$nb_news_articles, col="darkblue", main="Boxplot of Number of News Articles", 
        ylab="Number of News Articles (Log-Transformed)")
hist(movies$nb_news_articles, col="darkblue", breaks=100, main="Histogram of Number of News Articles", 
     xlab="Number of News Articles (Log-Transformed)", ylab="Frequency")
movies$nb_news_articles <- log(movies$nb_news_articles)
movies$nb_news_articles[movies$nb_news_articles == -Inf] <- 0

hist(movies$nb_news_articles, col="darkblue", breaks=100, main="Histogram of Log-Transformed Number of News Articles", 
     xlab="Log-Transformed Number of News Articles", ylab="Frequency")

# Scatter plot of number of news articles vs IMDb score with trend line
plot(movies$nb_news_articles, imdb_score, col="darkblue", pch=19,
     main="Scatter Plot of Number of News Articles vs IMDb Score",
     xlab="Number of News Articles (Log-Transformed)", ylab="IMDb Score")

# Add trend line (linear model)
abline(lm(imdb_score ~ movies$nb_news_articles), col="red", lwd=2)


### Log transformation needed for actor1_star_meter
boxplot(actor1_star_meter, col="red", main="Boxplot of Actor 1 Star Meter", 
        ylab="Actor 1 Star Meter")
hist(actor1_star_meter, col="red", breaks=30, main="Histogram of Actor 1 Star Meter", 
     xlab="Actor 1 Star Meter", ylab="Frequency")
movies$actor1_star_meter <- log(movies$actor1_star_meter)

#after
hist(movies$actor1_star_meter, col="red", breaks=70, main="Histogram of Log-Transformed Actor 1 Star Meter", 
     xlab="Log-Transformed Actor 1 Star Meter", ylab="Frequency")


### Log transformation needed for actor2_star_meter
boxplot(actor2_star_meter, col="darkorange", main="Boxplot of Actor 2 Star Meter", 
        ylab="Actor 2 Star Meter")
hist(actor2_star_meter, col="darkorange", breaks=60, main="Histogram of Actor 2 Star Meter", 
     xlab="Actor 2 Star Meter", ylab="Frequency")
movies$actor2_star_meter <- log(movies$actor2_star_meter)

hist(movies$actor2_star_meter, col="darkorange", breaks=70, main="Histogram of Actor 2 Star Meter (Log-Transformed)", 
     xlab="Actor 2 Star Meter (Log-Transformed)", ylab="Frequency")

### Log transformation needed for actor3_star_meter
boxplot(actor3_star_meter, col="darkred", main="Boxplot of Actor 3 Star Meter", 
        ylab="Actor 3 Star Meter")
hist(actor3_star_meter, col="darkred", breaks=60, main="Histogram of Actor 3 Star Meter", 
     xlab="Actor 3 Star Meter", ylab="Frequency")
movies$actor3_star_meter <- log(movies$actor3_star_meter)

hist(movies$actor3_star_meter, col="darkred", breaks=60, main="Histogram of Actor 3 Star Meter (Log-Transformed)", 
     xlab="Actor 3 Star Meter (Log-Transformed)", ylab="Frequency")


### Create a list of actor star meter variables and corresponding plot titles
actor_vars <- list("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")
plot_titles <- c("Actor 1 Star Meter vs IMDb Score", "Actor 2 Star Meter vs IMDb Score", "Actor 3 Star Meter vs IMDb Score")
colors <- c("red", "darkorange", "darkred")

# Loop through each actor star meter variable and plot
for (i in 1:length(actor_vars)) {
  actor_var <- actor_vars[[i]]
  plot_title <- plot_titles[i]
  color <- colors[i]
  
  # Scatter plot with trend line
  plot(movies[[actor_var]], imdb_score, col=color, pch=19,
       main=paste("Scatter Plot of", plot_title),
       xlab=paste(actor_var, "(Log-Transformed)"), ylab="IMDb Score")
  
  # Add trend line (linear model)
  abline(lm(imdb_score ~ movies[[actor_var]]), col="red", lwd=2)
}

### Log transformation needed for movie_meter_IMDBpro
boxplot(movie_meter_IMDBpro, col="pink", main="Boxplot of Movie Meter (IMDBpro)", 
        ylab="Movie Meter (IMDBpro)")
hist(movie_meter_IMDBpro, col="pink", breaks=60, main="Histogram of Movie Meter (IMDBpro)", 
     xlab="Movie Meter (IMDBpro)", ylab="Frequency")
movies$movie_meter_IMDBpro <- log(movies$movie_meter_IMDBpro)

hist(movies$movie_meter_IMDBpro, col="pink", breaks=60, main="Histogram of Movie Meter (Log-Transformed)", 
     xlab="Movie Meter (Log-Transformed)", ylab="Frequency")

# Scatter plot of movie_meter_IMDBpro vs IMDb score with trend line
plot(movies$movie_meter_IMDBpro, imdb_score, col="darkblue", pch=19,
     main="Scatter Plot of Movie Meter (IMDBpro) vs IMDb Score",
     xlab="Movie Meter (Log-Transformed IMDBpro)", ylab="IMDb Score")

# Add a trend line (linear model)
abline(lm(imdb_score ~ movies$movie_meter_IMDBpro), col="red", lwd=2)

attach(movies) 

#### Variable Processing 
month_to_number = function(month_names) {
  months_numeric = match(month_names, month.abb)
  return(months_numeric)
}
movies$release_month = month_to_number(movies$release_month)

#obtain a list of top production companies, transform variable into a binary variable
## Source: https://www.imdb.com/list/ls567506715/

Top_production = c("Castle Rock Entertainment", "Paramount Pictures", "Warner Bros.", 
                   "Universal Pictures", "New Line Cinema", "Miramax", "Produzioni Europee Associate (PEA)", 
                   "Lucasfilm", "Toho Company", "Dreamworks Pictures", "Studio Ghibli", "CJ Entertainment", 
                   "Walt Disney Pictures", "PolyGram Filmed Entertainment", "Touchstone Pictures", 
                   "Charles Chaplin Productions", "Shochiku", "Alfred J. Hitchcock Productions", 
                   "Pixar Animation Studios", "Marvel Studios", "Twentieth Century Fox", 
                   "Stanley Kubrick Productions", "The Ladd Company", "Metro-Goldwyn-Mayer (MGM)", 
                   "Kurosawa Production Co.", "Horizon Pictures (II)", "Zanuck/Brown Productions", 
                   "Columbia Pictures", "New Regency Productions", "Aamir Khan Productions", 
                   "Malpaso Productions", "Paramount Vantage", "Python (Monty) Pictures", 
                   "Chartoff-Winkler Productions", "Selznick International Pictures", 
                   "Summit Entertainment", "Buster Keaton Productions", "Svensk Filmindustri (SF)")
production = function(top_production) {
  whether_top = ifelse (production_company %in% Top_production, 1, 0) 
  return (whether_top)
}
movies$production_company = production(movies$production_company)

#obtain a list of top directors companies, transform variable into a binary variable
## Source: https://www.imdb.com/list/ls052380992/
Top_director = c("Steven Spielberg", "Martin Scorsese", "Alfred Hitchcock", "Stanley Kubrick", 
                 "Francis Ford Coppola", "Woody Allen", "Billy Wilder", "John Huston", "Peter Jackson", 
                 "Milos Forman", "Clint Eastwood", "David Lean", "Ingmar Bergman", "Joel Coen", "John Ford", 
                 "James Cameron", "Sidney Lumet", "Charles Chaplin", "Tim Burton", "Roman Polanski", 
                 "Quentin Tarantino", "Danny Boyle", "Ridley Scott", "David Fincher", "Christopher Nolan")

length(Top_director)
directors = function(top_director) {
  whether_top_director = ifelse (director %in% Top_director, 1, 0) 
  return (whether_top_director)
}
movies$director_top = directors(movies$director)

#transform country variable, if it is USA, the value is 1, otherwise, the value is 0
movies$country = ifelse(movies$country == "USA", 1, 0)

#Given that only a small portion of the movies are not in English, remove the movies that are not
movies$movies_english <- ifelse(movies$language == "English", 1,0 )

#transform colour film variable, if it is a Color movie, the value is 1, otherwise, the value is 0
movies$colour_film = ifelse(movies$colour_film == "Color", 1, 0)

#transform maturity rating variable. Upon examing the datasset, we noticed that over 3/4th of the movies have 
#a rating of PG-13 ro R. Thus, If the movie has a rating of PG-13 or R, it's value is 1, else 0
movies$maturity_rating <- ifelse(movies$maturity_rating %in% c("PG-13", "R"), 1, 0)

#Calculate the num_of_year release
movies$release_length <-  2024 -  movies$release_year 

# Count the number of genres for each movie
movies$nb_genres <- rowSums(movies[, c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                                       "western", "sport", "horror", "drama", "war", "animation", "crime")])
movies
# Example: Average IMDb score of movies directed by the same director
library(dplyr)
director_avg_score <- movies %>%
  group_by(director) %>%
  summarise(avg_director_score = mean(imdb_score, na.rm = TRUE))
# Merge back into the original data
movies <- merge(movies, director_avg_score, by = "director", all.x = TRUE)

# Combine actor star meter rankings into one score
movies$actor_avg_star_meter <- rowMeans(cbind(movies$actor1_star_meter, movies$actor2_star_meter, movies$actor3_star_meter), na.rm = TRUE)


movies$release_season <- cut(movies$release_month,
                             breaks = c(0, 3, 6, 9, 12),
                             labels = c(1,2,3,4),
                             include.lowest = TRUE)

attach(movies) 
####Non-Linearity Issue

# Assuming 'imdb_score' is the response variable and 'movies' is your dataset

# Split the data
Y <- imdb_score
X_numeric <- cbind.data.frame(movie_budget, duration, aspect_ratio, nb_news_articles, 
                              actor1_star_meter, actor2_star_meter, actor3_star_meter, 
                              nb_faces, movie_meter_IMDBpro)

# Explore the linearity between features and response variable
for (i in 1:length(colnames(X_numeric))) {
  # Fit the linear model
  model <- lm(Y ~ X_numeric[, i])
  residualPlot(
    model,
    xlab = paste("Fitted values for", colnames(X_numeric)[i]), 
    ylab = "Residuals",                                        
    main = paste("Residual Plot for", colnames(X_numeric)[i]), 
    col = "blue",                                             
    pch = 19                                                  
  )
  abline(h = 0, col = "red", lwd = 2)
  grid()
}

# Reset plot layout
par(mfrow = c(1, 1))



####Heteroskedasticity

# List to store predictors with heteroskedasticity
heteroskedastic_predictors <- list()

for (i in 1:ncol(X_numeric)){   # Use ncol to loop over columns of X_numeric
  predictor_name <- colnames(X_numeric)[i]
  model <- lm(Y ~ X_numeric[, i])   # Linear regression model with Y as the dependent variable and X_numeric[,i] as the predictor
  
  # Run the NCV test (Breusch-Pagan test for heteroskedasticity)
  ncv_test <- ncvTest(model)
  
  # Check if heteroskedasticity exists (p-value < 0.05)
  if (ncv_test$p < 0.05) {
    # Store the predictor name and its p-value
    heteroskedastic_predictors[[predictor_name]] <- ncv_test$p
    
    # Print the result for this predictor
    print(paste("Heteroskedasticity detected in", predictor_name, "with p-value:", ncv_test$p))
  } else {
    # Print that no heteroskedasticity was detected
    print(paste("No heteroskedasticity in", predictor_name, "with p-value:", ncv_test$p))
  }
}

print("Predictors with heteroskedasticity:")
print(heteroskedastic_predictors)


####Outliers
# Loop over each numeric predictor
for (i in 1:length(colnames(X_numeric))) {
  
  # Build a linear model for the i-th predictor
  model <- lm(Y ~ X_numeric[, i], data = movies)
  
  # Perform the outlier test
  outlier_test <- outlierTest(model)
  
  # Print the outlier test results
  print(outlier_test)
  
  
}
#movies <- movies[-c(989, 395, 191, 1806, 492, 1581), ]
#X_numeric <- X_numeric[-c(989, 395, 191, 1806, 492, 1581), ]
X_numeric <- X_numeric[-c(153, 739, 741, 1136, 1535, 1864), ]
movies <- movies[-c(153, 739, 741, 1136, 1535, 1864), ]
attach(movies) 


#Colinearity
  # For diagnostic tools like VIF

# Prepare data for VIF calculation
Y <- imdb_score
X_numeric <- cbind.data.frame(movie_budget, duration, aspect_ratio, nb_news_articles,
                              actor1_star_meter, actor2_star_meter, actor3_star_meter,
                              nb_faces, movie_meter_IMDBpro)
X_numeric <- X_numeric[-c(989, 395, 191, 1806, 492, 1581), ]
Y <- Y[-c(989, 395, 191, 1806, 492, 1581)]
# Calculate VIF for each predictor
vif_values <- vif(lm(Y ~ ., data = X_numeric))
print(vif_values)

# Explore which variables have high VIF (common threshold is VIF > 5 or VIF > 10)
high_vif <- names(vif_values[vif_values > 5])  # You can adjust the threshold as needed

# Output the variables with high VIF
print(high_vif)


library(corrplot)
cor_matrix <- cor(X_numeric)
corrplot(cor_matrix, 
         method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.srt = 45,          
         tl.cex = 0.8,         
         number.cex = 0.7,    
         type = "upper",       
         addCoef.col = "black" 
)



#### Linear regression 

numeric_columns <- select_if(movies, is.numeric)
numeric_columns
numeric_columns <- numeric_columns[, !colnames(numeric_columns) %in% c("imdb_score", "movie_id")]
significant_predictors <- list()
for (col in colnames(numeric_columns)) {
  # Dynamically create the formula for each numeric column
  formula <- as.formula(paste("imdb_score ~", col))
  
  # Run the linear regression
  model <- lm(formula, data = movies)
  
  p_value <- summary(model)$coefficients[2, 4]  # The p-value is in the 4th column of the coefficients
  
  # Check if p-value is less than 0.05
  if (p_value < 0.001) {
    # Add the predictor to the list of significant predictors
    significant_predictors[[col]] <- p_value
    
    # Print the summary of the model
    cat("Regression Summary for:", col, "\n")
    print(summary(model))
    cat("\n------------------------------------\n")
  }
  
 
}

significant_predictors

# Assuming significant_predictors has been populated from the previous loop
# Extract the names of the significant predictors
significant_predictor_names <- names(significant_predictors)

# Dynamically create the formula for the multiple regression
# Combine all predictor names into a formula
predictors_formula <- paste(significant_predictor_names, collapse = " + ")
full_formula <- as.formula(paste("imdb_score ~", predictors_formula))

# Run the multiple regression with the significant predictors
mreg1 <- lm(full_formula, data = movies)

# Print the summary of the multiple regression model
summary(mreg1)


### multiple regressions
mreg2=lm(imdb_score~avg_director_score+movie_budget,data = movies)
summary(mreg2) #movie_budget coefficient too small remove

mreg3=lm(imdb_score~avg_director_score+duration,data = movies)
summary(mreg3)
mreg4=lm(imdb_score~avg_director_score+duration+release_year,data = movies)
summary(mreg4)
mreg5=lm(imdb_score~avg_director_score+duration+release_year+drama,data = movies)
summary(mreg5)
mreg6=lm(imdb_score~avg_director_score+duration+release_year+drama+horror,data = movies)
summary(mreg6) 

mreg7=lm(imdb_score~avg_director_score+movie_budget+horror+duration+release_year+movies_english+drama+movie_meter_IMDBpro, data = movies)
summary(mreg7)

residualPlots(mreg7)
ncvTest(mreg7) 
coeftest(mreg7, vcov=vcovHC(mreg7, type="HC1")) 

outlierTest(mreg7)
#movies2 = movies[-c(1135,157,154,731,379,515,155), ]
movies = movies[-c(155,156,381,541,728,1103,1406), ]

movies$movie_meter_IMDBpro <- scale(movies$movie_meter_IMDBpro)
movies$avg_director_score <- scale(movies$avg_director_score)
movies$duration <- scale(movies$duration)
movies$movie_budget <- scale(movies$movie_budget)
movies$release_year <- scale(movies$release_year )

attach(movies)

###Anova
#Here I ran an ANOVA table for each individual regressor with the dependent 
#variable to find the degree that best describes the data. 
#I set the limit to the 3rd degree to limit the degrees of freedom lost.

###Polynomial ANOVA
poly1m = lm(imdb_score~poly(movie_budget,1)) #Best
poly2m = lm(imdb_score~poly(movie_budget,2))
poly3m = lm(imdb_score~poly(movie_budget,3))
anova(poly1m, poly2m, poly3m)

poly1d = lm(imdb_score~poly(duration,1))
poly2d = lm(imdb_score~poly(duration,2)) #BEST
poly3d = lm(imdb_score~poly(duration,3)) 
anova(poly1d, poly2d, poly3d)


poly1a1 = lm(imdb_score~poly(avg_director_score,1)) #best
poly2a1 = lm(imdb_score~poly(avg_director_score,2))
poly3a1 = lm(imdb_score~poly(avg_director_score,3)) 
anova(poly1a1, poly2a1, poly3a1)

poly1y = lm(imdb_score~poly(release_year,1))
poly2y = lm(imdb_score~poly(release_year,2))
poly3y = lm(imdb_score~poly(release_year,3)) #best
anova(poly1y, poly2y, poly3y)

poly1p = lm(imdb_score~poly(movie_meter_IMDBpro,1))
poly2p = lm(imdb_score~poly(movie_meter_IMDBpro,2))
poly3p = lm(imdb_score~poly(movie_meter_IMDBpro,3)) #best
anova(poly1p, poly2p, poly3p)
anova(poly1p, poly3p)




splineduration <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)
splinebudget <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)
splinedirector <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)
splineyear <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)
splinemeter <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)

splineenglish <- data.frame(
  MSE =c(100),
  LowQuant = c(1),
  HighQuant = c(1),
  Degree = c(1)
)

quant1 = c(0.01,0.05,0.10,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5)
quant2 = c(0.99,0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5)

#duration
for (i in quant1){
  for (f in quant2){
    for (a in 1:3){
      fit=glm(imdb_score ~ bs(duration, degree = a, knots = c(quantile(movies$duration,i), quantile(movies$duration,f))), data = movies)
      splineduration[nrow(splineduration) + 1,] = list(cv.glm(movies, fit, K = 20)$delta[1],i,f,a)
    }
  }
}
splinedurationavg <- splineduration %>%
  group_by(LowQuant, HighQuant, Degree) %>%
  summarize(mean_MSE = mean(MSE))
splinedurationMSE = splinedurationavg[which.min(splinedurationavg$mean_MSE), ]
splinedurationMSE #0.05-0.85, a=1, cuts outside data.

#Duration
chart=ggplot(movies, aes(y=imdb_score, x=duration))
scatter= geom_point(color="grey")
nospline= geom_smooth(method = "lm", formula = y~bs(x, degree=2))
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(quantile(movies$duration,0.05), quantile(movies$duration,0.85)), degree=2))
chart1 = chart + scatter + spline_1 + geom_vline(xintercept=c(quantile(movies$duration,0.05), quantile(movies$duration,0.85)),linetype='dashed')
chart2 = chart + scatter + nospline
plots <- list(chart1, chart2)
ggarrange(plotlist = plots, ncol = 2, nrow = 1)


#movie budget
for (i in quant1){
  for (f in quant2){
    for (a in 1:3){
      fit=glm(imdb_score ~ bs(movie_budget, degree = a, knots = c(quantile(movies$movie_budget,i), quantile(movies$movie_budget,f))), data = movies)
      splinebudget[nrow(splinebudget) + 1,] = list(cv.glm(movies, fit, K = 20)$delta[1],i,f,a)
    }
  }
}
splinebudgetavg <- splinebudget %>%
  group_by(LowQuant, HighQuant, Degree) %>%
  summarize(mean_MSE = mean(MSE))
splinebudgetMSE = splinebudgetavg[which.min(splinebudgetavg$mean_MSE), ]
splinebudgetMSE #0.5-0.65 a=1, one spline at 0.5.

#budget
chart=ggplot(movies, aes(y=imdb_score, x=movie_budget))
scatter= geom_point(color="grey")
nospline= geom_smooth(method = "lm", formula = y~bs(x, degree=1))
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knot=c(quantile(movies$movie_budget,0.5),quantile(movies$movie_budget,0.65)), degree=1))
chart1 = chart + scatter + spline_1 + geom_vline(xintercept=c(quantile(movies$movie_budget,0.5),quantile(movies$movie_budget,0.65)),linetype='dashed')
chart2 = chart + scatter + nospline
plots <- list(chart1, chart2)
ggarrange(plotlist = plots, ncol = 2, nrow = 1)

#avg_director_score
for (i in quant1){
  for (f in quant2){
    for (a in 1:3){
      fit=glm(imdb_score ~ bs(avg_director_score, degree = a, knots = c(quantile(movies$avg_director_score,i), quantile(movies$avg_director_score,f))), data = movies)
      splinedirector[nrow(splinedirector) + 1,] = list(cv.glm(movies, fit, K = 20)$delta[1],i,f,a)
    }
  }
}
splinedirectoravg <- splinedirector %>%
  group_by(LowQuant, HighQuant, Degree) %>%
  summarize(mean_MSE = mean(MSE))
splinedirectorMSE = splinedirectoravg[which.min(splinedirectoravg$mean_MSE), ]
splinedirectorMSE #0.01-0.99 a=1, cuts off tail data.

#director
chart=ggplot(movies, aes(y=imdb_score, x=avg_director_score))
scatter= geom_point(color="grey")
nospline= geom_smooth(method = "lm", formula = y~bs(x, degree=1))
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knot=c(quantile(movies$avg_director_score,0.01),quantile(movies$avg_director_score,0.99)), degree=1))
chart1 = chart + scatter + spline_1 + geom_vline(xintercept=c(quantile(movies$avg_director_score,0.01),quantile(movies$avg_director_score,0.99)),linetype='dashed')
chart2 = chart + scatter + nospline
plots <- list(chart1, chart2)
ggarrange(plotlist = plots, ncol = 2, nrow = 1)
#No obvious visual splines, cubic fits data well.

#release year
for (i in quant1){
  for (f in quant2){
    for (a in 1:3){
      fit=glm(imdb_score ~ bs(release_year, degree = a, knots = c(quantile(movies$release_year,i), quantile(movies$release_year,f))), data = movies)
      splineyear[nrow(splineyear) + 1,] = list(cv.glm(movies, fit, K = 20)$delta[1],i,f,a)
    }
  }
}
splineyearavg <- splineyear %>%
  group_by(LowQuant, HighQuant, Degree) %>%
  summarize(mean_MSE = mean(MSE))
splineyearMSE = splineyearavg[which.min(splineyearavg$mean_MSE), ]
splineyearMSE ##lb = 0.2, ub = 0.65, degree = 3

#Year
chart=ggplot(movies, aes(y=imdb_score, x=release_year))
scatter= geom_point(color="grey")
nospline= geom_smooth(method = "lm", formula = y~bs(x, degree=3))
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(quantile(movies$release_year,0.2),quantile(movies$release_year,0.65)), degree=1))
chart1 = chart + scatter + spline_1 + geom_vline(xintercept=c(quantile(movies$release_year,0.2),quantile(movies$release_year,0.65)),linetype='dashed')
chart2 = chart + scatter + nospline
plots <- list(chart1, chart2)
ggarrange(plotlist = plots, ncol = 2, nrow = 1)


#movie_meter_IMDBpro
for (i in quant1){
  for (f in quant2){
    for (a in 1:3){
      fit=glm(imdb_score ~ bs(movie_meter_IMDBpro, degree = a, knots = c(quantile(movies$movie_meter_IMDBpro,i), quantile(movies$movie_meter_IMDBpro,f))), data = movies)
      splinemeter[nrow(splinemeter) + 1,] = list(cv.glm(movies, fit, K = 20)$delta[1],i,f,a)
    }
  }
}
splinemeteravg <- splinemeter %>%
  group_by(LowQuant, HighQuant, Degree) %>%
  summarize(mean_MSE = mean(MSE))
splinemeterMSE = splinemeteravg[which.min(splinemeteravg$mean_MSE), ]
splinemeterMSE #0.01-0.95 a=1

#Movie Meter
chart=ggplot(movies, aes(y=imdb_score, x=movie_meter_IMDBpro))
scatter= geom_point(color="grey")
nospline= geom_smooth(method = "lm", formula = y~bs(x, degree=3))
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(quantile(movies$movie_meter_IMDBpro,0.01), quantile(movies$movie_meter_IMDBpro,0.95)), degree=2))
chart1 = chart + scatter + spline_1 + geom_vline(xintercept=c(quantile(movies$movie_meter_IMDBpro,0.01),quantile(movies$movie_meter_IMDBpro,0.95)),linetype='dashed')
chart2 = chart + scatter + nospline
plots <- list(chart1, chart2)
ggarrange(plotlist = plots, ncol = 2, nrow = 1)



optimal_mse = Inf
optimal_a = 0
optimal_b = 0
optimal_c = 0
optimal_d = 0


for (a in 1:5) {
  for (b in 1:5) {
    for (c in 1:5) {
      for (d in 1:5) {
        #            mse=rep(NA, 20)  
        
        # Fit the model using specified degrees and knots
        fit = glm(imdb_score ~ 
                    bs(avg_director_score, knots = c(quantile(avg_director_score, 0.1), quantile(avg_director_score, 0.99)), degree = a) +
                    bs(movie_meter_IMDBpro, knots = c(quantile(movie_meter_IMDBpro, 0.01), quantile(movie_meter_IMDBpro, 0.95)), degree = b) +
                    bs(duration, knots = c(quantile(duration, 0.05), quantile(duration, 0.85)), degree = c) +
                    bs(movie_budget, knots = c(quantile(movie_budget, 0.5), quantile(movie_budget, 0.65)), degree = d) +
                    drama + horror + movies_english, data = movies)
        
        
        # Perform K-fold cross-validation (K=20)
        mse =  cv.glm(movies, fit, K = 20)$delta[1]
        
        # Update optimal values if the current MSE is lower
        if (!is.na(mse) && mse < optimal_mse) {
          optimal_mse = mse
          optimal_a = a
          optimal_b = b
          optimal_c = c
          optimal_d = d
          
          
          
        }
      }
    }
  }
}

optimal_mse ## loop mse: 0.6747726 0.2265 0.2139661
optimal_a
optimal_b
optimal_c
optimal_d

############################release year
optimal_mse = Inf
optimal_a = 0
optimal_b = 0
optimal_c = 0
optimal_d = 0


for (a in 1:5) {
  for (b in 1:5) {
    for (c in 1:5) {
      for (d in 1:5) {
        #            mse=rep(NA, 20)  
        
        # Fit the model using specified degrees and knots
        fit = glm(imdb_score ~ 
                    bs(release_year, knots = c(quantile(release_year, 0.2), quantile(avg_director_score, 0.65)), degree = a) +
                    bs(movie_meter_IMDBpro, knots = c(quantile(movie_meter_IMDBpro, 0.01), quantile(movie_meter_IMDBpro, 0.95)), degree = b) +
                    bs(duration, knots = c(quantile(duration, 0.05), quantile(duration, 0.85)), degree = c) +
                    bs(movie_budget, knots = c(quantile(movie_budget, 0.5), quantile(movie_budget, 0.65)), degree = d) +
                    drama + horror + movies_english, data = movies)
        
        
        # Perform K-fold cross-validation (K=20)
        mse =  cv.glm(movies, fit, K = 20)$delta[1]
        
        # Update optimal values if the current MSE is lower
        if (!is.na(mse) && mse < optimal_mse) {
          optimal_mse = mse
          optimal_a = a
          optimal_b = b
          optimal_c = c
          optimal_d = d
          
          
          
        }
      }
    }
  }
}

optimal_mse ## loop mse: 0.6747726 0.2265 0.2139661
optimal_a
optimal_b
optimal_c
optimal_d
###########################

###Final Model  really sensitive to movie_meter_IMDB pro quantile and degree
fit = glm(imdb_score ~ 
            bs(avg_director_score, knots = c(quantile(avg_director_score, 0.1), quantile(avg_director_score, 0.99)), degree = 2) +
            bs(movie_meter_IMDBpro, knots = c(quantile(movie_meter_IMDBpro, 0.01), quantile(movie_meter_IMDBpro, 0.95)), degree = 1) +
            bs(duration, knots = c(quantile(duration, 0.05), quantile(duration, 0.85)), degree = 3) +
            bs(movie_budget, knots = c(quantile(movie_budget, 0.5), quantile(movie_budget, 0.65)), degree = 1) +
            drama + horror + movies_english, data = movies)
summary(fit)
mse =  cv.glm(movies, fit, K = 20)$delta[1]
mse # 0.2104777

#### lastly try on new dataset
test = read.csv("/Users/yuxingu/Desktop/Fall 2024/MGSC661/Midterm/test_data_IMDB_Fall_2024.csv")

# data process

test$movie_meter_IMDBpro <- log(test$movie_meter_IMDBpro)  # Log-transform the movie meter
test$movies_english <- ifelse(test$language == "English", 1, 0)  # Binary encoding for English language

# Calculate the avg_director_score in the test set
director_avg_score <- movies %>%
  group_by(director) %>%
  summarise(avg_director_score = mean(imdb_score, na.rm = TRUE))

# Merge the avg_director_score back into the test dataset
test <- merge(test, director_avg_score, by = "director", all.x = TRUE)
test$avg_director_score[is.na(test$avg_director_score)] <- mean(director_avg_score$avg_director_score, na.rm = TRUE)
test$movie_meter_IMDBpro <- scale(test$movie_meter_IMDBpro)
test$avg_director_score <- scale(test$avg_director_score)
test$duration <- scale(test$duration)
test$movie_budget <- scale(test$movie_budget)
attach(test)
predictions <- predict(fit, newdata = test) 
predictions
mean(predictions)


