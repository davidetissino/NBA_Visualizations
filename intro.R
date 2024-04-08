# first R script: WHEN RUNNING, IT ONLY RUNS CURSOR LINE

hello <- "hello world!"


# Vectors: objects of the same type, 'c' = combine()

vector_1 <- c("osama", "majinbu", "jesus", "...")

vector_2 <- c(1, 4, 64, 9183, 19276392)

    # to return xth element of the vector: vector[x]

    # to explore structure of vector: str(vector)



# Factors: categorical variables 

gender <- c("f", "m", "f", "f", "f", "m", "m")

gender <- factor(gender)      # vector transformed to factor 

str(gender)                   # examine the structure of gender 



# Lists: sequence of elements of ≠ types 

mylist <- list(vector_1 = vector_1 , vector_2 = vector_2, gender = gender)

mylist

str(mylist[[1]])              # to execute function on specific element 
str(mylist[['vector_1']])

mylist$vector_2               # to reference individual elements from a list 



# Data frames: tables of data, obtained combining two vectors 

franchise <- c('suns', 'heat', 'bucks', 'lakers')

city <- c('Phoenix', 'Miami', 'Milwaukee', 'Los Angeles')

teams <- data.frame(city, franchise)  # creates the dataframe  

teams

names(teams)     # names() identifies names of df variables (franchise, city)



# Functions: example w/ mean created rather than built-in 

x <- c(1, 3, 5, 11, 15)

our_mean_1 <- function(x){

    return (sum(x) / length(x))
  
}

our_mean_2 <- function(x){
  
  name <- sum(x)/length(x)
  
  name
  
}

our_mean_3 <- function(x){
  
  another <- sum(x)/length(x)
  
  print(another)
  
}

# follows a function providing summary on vector x 

our_summary <- function(x){
  mean = mean(x)
  median = median(x)
  standard_deviation = sd(x)
  
  miao <- cbind(mean, median, standard_deviation)
  return(miao)
  
}



# Getting data to/from R: 

read.delim('file location', header = TRUE, sep = '\t', quote = '\'', 
           dec = '.', fill = TRUE, comment.char = '')

dati <- read.csv('url', header = TRUE, na.strings = 'NA', 
                 stringsAsFactors = FALSE)  # stringasfactors to read strings as such and not as factors

str(dati)

head(dati)            # provides first 6 rows of the object 

write.csv(dati, 'path')



# Import a dataset 

steph_gamelog = read_csv('steph gamelog')

gamelog_clean <- head(steph_gamelog[, c(4:27)], 79)

points_first <- gamelog_clean[, c(1:4, 23, 5:22, 24)]

only_ws <- filter(gamelog_clean, WL == 'W')
  


# Data Manipulation: 

data(iris)                         # load specific dataset 
head(iris, 3)                      # preview of first 3 rows of dataset 
head(iris$Sepal.Length, 3)         # to access specific object of dataset 

with(iris, Sepal.Length / Sepal.Width)      # to access more than one variable 

iris$sepal_length_width_ratio <- with(iris, Sepal.Length / Sepal.Width)
head(iris)        # use of $ to assign new variable to dataset

iris$sepal_length_width_ratio <- round(iris$sepal_length_width_ratio, 2)
head(iris)        # provides a rounding of the values, number explicit

# code variables w/ ifelse, based on below/between/above 1st and 3rd quartile 

summary(iris$sepal_length_width_ratio)
iris$ratio_q <- with(iris,  
                     ifelse(sepal_length_width_ratio <= 1.550, 1, 
                     ifelse(sepal_length_width_ratio > 1.550 & sepal_length_width_ratio < 2.22, 2,
                     ifelse(sepal_length_width_ratio >= 2.228, 3, NA))))
  
  # 1, 2 and 3 are values assigned to new object ratio_q basing on ifelse statement

head(iris[, c(6:7)], 10)      #first 10 rows of element 6 to 7 of dataset (ratio, ratio_q)

head(iris[, c(1,3)])           # returns variables 1 and 3 of dataset 

unique(iris$Species)          # to identify the three unique species in the dataset 

sub_virginica <- subset(iris, Species == 'virginica')   # includes only virginica cases
head(sub_virginica)

ex_virginica <- subset(iris, Species != 'virginica')    # excludes virginica cases
head(ex_virginica, 100)

sub_virginica_2 <- subset(iris, Species != 'virginica' & sepal_length_width_ratio >= 2)
head(sub_virginica_2)                                   # adds a condition to subset 



# The dplyr package (lahman dataset installed, explore batting and players)

batting <- Lahman::Batting
player <- Lahman::People

AB_400 <- filter(batting, AB >= 400)      # filter adds condition 

AB_400_names <- left_join(AB_400, player, by = 'playerID')
  # left_join keeps the cases from the first table and matches data 
  # in the second, where there are common cases, according to key variable

AB_400_names$fullName <- with(AB_400_names, paste(nameFirst, nameLast)) 
  # adds object $'object_name' in dataset by pasting two variables 

AB_reduced <- select(AB_400_names, playerID, yearID, G, AB, HR, fullName) 
  # new dataset with only specific variables from previous dataset 

AB_indexed <- AB_reduced[, c(6, 2, 1, 3:5)]
  # indexed specifying order of specific variables 



# Pipe Function %>% ≈ then (first what on left, then what on right)

AB_lower70 <- filter(AB_400_names, height < 70) %>% 
              select(fullName, yearID, HR) %>%
              arrange(fullName)
  # first we filter a certain dataset, then select only parts of it 
  # and eventually arrange in ascending name 



# Summary Statistics 

summary(airquality)

summarize_each(airquality, funs(sd(., na.rm = TRUE)))
  # used to obtain a measure (sd) for every variable contained 

select(airquality, -Day) %>% 
  group_by(Month) %>%
  summarise_each(funs(sd(., na.rm = TRUE)))
  # use to obtain measure (sd) for each variable according to key (month)

with(iris, table(Species, Petal.Width)) %>%   # to run a crosstabulation 
  prop.table() %>%
  round(., 2)

with(iris, table(Species, Petal.Width)) %>%   # to run crosstabl 
  prop.table(margin = 1) %>%    # margin = 1 --> row frequency 
  round(., 2)                   # margin = 2 --> column frequency

cross_column <- with(iris, table(Species, Petal.Width)) %>%
  prop.table(margin = 2) %>%
  round(., 2) %>% 
  as.data.frame.matrix()

write.csv(cross_column, 'cross_column.csv')



# Basic Stats and Modeling

survey_data <- read.csv('https://raw.githubusercontent.com/BillPetti/R-Crash-Course/master/survey_sample_data.csv',
                        header = TRUE, 
                        stringsAsFactors = FALSE)


