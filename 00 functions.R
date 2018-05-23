require(tidyverse)


# Non Detects -------------------------------------------------------------

# Separate out qualifiers (<, >, =) that are embedded inside Result.Value
# Create 2 new columns:  
#      'qual' for qualifiers
#      'r' for numeric results without qualifiers
# Input is a dataframe - must have column named 'Result.Value'


non.detects <- function(df){
  
  df <- df %>%
    #pull qualifiers out of results
    mutate(qual = ifelse(str_sub(Result.Value, start = 1, end = 1) == "<" | str_sub(Result.Value, start = 1, end = 1) == ">" , 
                         str_sub(Result.Value, start = 1, end = 1), "=")) %>%
    #set data qualifiers
    mutate(r =ifelse(qual == "<" | qual == ">", str_sub(Result.Value, start = 2, end = length(Result.Value)), Result.Value )) %>%
    mutate(r = as.numeric(r))
  
  return(df)
}



# Geometric mean ----------------------------------------------------------

#Calculate geometric mean
#borrowed from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
#  "Here is a vectorized, zero- and NA-tolerant function for calculating geometric mean in R. 
#   The verbose mean calculation involving length(x) is necessary for the cases where x contains non-positive values"
#input is a vector


geo_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  
}




# Binomial Listing requirements -------------------------------------------


#create function for binomial listing requirements for toxix substances
#This is a hard coded version of the table on pg 15 of the
#Draft Methodology
# >200 needs to be fixed
#ALIANA COMMENT- R has function binom.test that will perform binomial test for us
#if the table is the method however, we will need to create function based on table
# and if sample size is greater than 200 perhaps use the function

funct_list <- function(value){
  if(value == 1){
    return(1)
  } else if(value >= 2 & value <= 18){
    return(2)
  } else if(value >= 19 & value <= 22){
    return(3)
  } else if(value >= 23 & value <= 35){
    return(4)
  } else if(value >= 36 & value <= 49){
    return (5)
  } else if(value >= 50 & value <= 63){
    return(6)
  } else if(value >= 64 & value <= 78){
    return(7)
  } else if(value >= 79 & value <= 92){
    return(8)
  } else if(value >= 93 & value <= 109){
    return(9)
  } else if(value >= 110 & value <= 125){
    return(10)
  } else if(value >= 126 & value <= 141){
    return(11)
  } else if(value >= 142 & value <= 158){
    return(12)
  } else if(value >= 159 & value <= 171){
    return(13)
  } else if(value >= 172 & value <= 191){
    return(14)
  } else if(value >= 192 & value <= 200){
    return(15)
  } else if(value >= 201 & value <= 250){
    return(16)
  }
}


#Inserting listing requirements into table
#This takes a dataframe and 
funct_list_size <- function(df){
  for(i in 1:nrow(df)){
    df$num_to_list[i] <- as.numeric(funct_list(df$count[[i]])) 
  } 
  return(df)
}


