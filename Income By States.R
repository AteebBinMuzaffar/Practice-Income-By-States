install.packages("dplyr")
library(dplyr) 
#Download the Dataset from https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv
df <- read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")

#Selecting Random N Rows
sample_n(df,5)

#Selecting Random 10% of Rows
sample_frac(df, 0.1)

#Removing Duplicate Rows, if any, based on all variables
var1 <- distinct(df)

#Removing Duplicate Rows, if any, based on a single variable
var2 <- distinct(df, Index, .keep_all = TRUE)

#Removing Duplicate Rows, if any, based on multiple variables
var2 <- distinct(df, Index, Y2010, .keep_all = TRUE)

#Selecting Variables (Columns)
#a ' : ' gives a range, while a comma selects that particular variable
df2 <- select(df, Index, State:Y2005)

#Dropping Variables
df2 <- select(df, -Index, -State)

#Selecting or Dropping Variables starting with different letters
df3 <- select(df, starts_with("Y"))
df3 <- select(df, -starts_with("Y"))
df4 <- select(df, contains("I"))
df4 <- select(df, -contains("I"))

#Reorder Variables starting with 'State'
df5 <- select(df, State, everything())

#Rename Variables
df6 <- rename(df, Old_State = State)

#Filter Rows
df7 <- filter(df, Index == "A")

#Filter Rows using multiple criteria
df7 <- filter(df, Index %in% c("A","D"))

#Using conditions in the criteria
#AND condition
df8 <- filter(df, Index %in% c("A","C") & Y2002 >= 1300000)
#OR condition
df9 <- filter(df, Index %in% c("A","D")| Y2002 >= 1300000)
#NOT condition
df10 <- filter(df, !Index %in% c("A","D")) 

#Contains condition
df10 <- filter(df, grepl("17", Y2002))

#Summarize selected variables
summarise(df, Mean_2015 <- mean(Y2015), Median_2015 <- median(Y2015))

#Summarize multiple variables
summarize_at(df, vars(Y2010,Y2015), funs(n(), mean, median))

#Summarize with Custom Functions
summarize_at(df, vars(Y2010,Y2015), funs(n(), missing <- sum(is.na(.)), 
mean(., na.rm = TRUE), median(.,na.rm = TRUE)))

#Applying non standard functions
set.seed(222)
randSample <- data.frame(X1 = sample(1:100,100), X2= runif(100))
summarise_at(randSample, vars(X1,X2), function(x) var(x - mean(x)))

#Summarize all numeric variables
summarise_if(df, is.numeric,funs(n(),mean,median))

#Alternate Method
numData <- df[sapply(df,is.numeric)]
summarise_all(numData, funs(n(),mean,median))

#Summarize factor variable
summarise_all(df["Index"], funs(nlevels(.), nmiss = sum(is.na(.))))

#Sort Data by multiple variables
arrange(df, Index, Y2002)
arrange(df, desc(Index), Y2002)

#Selecting 10 random obs. of two variables
dt <- sample_n(select(df,Index,State), 10)

#Alternate Variable
dt <- df %>% select(Index,State) %>% sample_n(10)

#Summarize Data by Categorical Variable
t = summarise_at(group_by(df, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE)))

#Alternate Method
t = df %>% group_by(Index) %>%
  summarise_at(vars(Y2012,Y2013), funs(n(), mean(., na.rm = TRUE)))

