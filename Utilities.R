library(here)
library(dplyr)
library(stringr)
# store data dictionary file path
DD_FILEPATH <- here::here( "data/rodeo/ltdb_data_dictionary.csv" )

# import data dictionary
dd <- read.csv( DD_FILEPATH, stringsAsFactors=F )

# ONE: Filter variables by theme or group. Write a function that takes as an argument one or more of the groups that you just created in the concordance file under the “category” field and returns all of the variables associated with that group.
aapi <- c (str_detect(dd$definition, regex("Asian", ignore_case = T)),
           str_detect(dd$definition, regex("pacific", ignore_case = T)),
           str_detect(dd$definition, regex("korean", ignore_case = T)),
           str_detect(dd$definition, regex("japanese", ignore_case = T)),
           str_detect(dd$definition, regex("vietnamese", ignore_case = T)))

asians.pacific.islanders <- cbind(dd,aapi) %>% 
  filter(aapi == T) 
View(asians.pacific.islanders)


# another example 

dd %>% filter(str_detect(definition, "poverty")) %>%
  slice()


# The function should return all of the rows of the dataframe that belong to the group.



# TWO: create a function that searches variable descriptions for a specific string and returns any 
# that match. 

# does not work

find.variables.for.string <- function(variable.string) {
  test.term <- str_detect(dd$definition, regex("variable.string", ignore_case = T))
    test.term.l <- cbind (dd,test.term) %>%
      filter(test.term == T)
  return(test.term.l)
}

find.variables.for.string(asian)

#function back work - does not work

asian.test <- str_detect(dd$definition, regex("Asian", ignore_case = T))
                         asian.test.2 <-cbind(dd,asian.test) %>%
                           filter(asian.test == T)
                         View(asian.test.2)

find.variables.for.string(asian)

#does not work

find.string <- function(string) {
  variable.string <- dd %>% filter(str_detect(definition, "string")) 
  return(variable.string)
}

find.string(poverty)





# Create a function to filter variables by time periods. 
#Specifically, the user will specify the time periods of interest for the study and the
#function will identify all variables that have measures for those periods.

#variable formation:
seventies <- c (grepl("[1-9]",dd$X1970.f),
                grepl("[1-9]",dd$X1970.s))

variables.1970s <- cbind (dd,seventies) %>%
  filter(seventies == T)



eighties <- c (grepl("[1-9]",dd$X1980.f),
               grepl("[1-9]",dd$X1980.s))

variables.1980s <- cbind (dd,eighties) %>%
  filter(eighties == T)




nineties <- c (grepl("[1-9]",dd$X1990.f),
               grepl("[1-9]",dd$X1990.s))

variables.1990s <- cbind (dd,nineties) %>%
  filter(nineties == T)




early.2000s <- c(grepl("[1-9]",dd$X2000.f),
                 grepl("[1-9]",dd$X2000.s))

variables.2000s <- cbind (dd,early.2000s) %>%
  filter(early.2000s == T)




later.2000s<- c(grepl("[1-9]",dd$X2010.f),
                grepl("[1-9]",dd$X2010.s))

variables.2010s <- cbind (dd,later.2000s) %>%
  filter(later.2000s == T)
# choose from time periods:
variables.1970s
variables.1980s
variables.1990s
variables.2000s
variables.2010s

# function
find.variables.for.time.period <- function(time.period) {
  variables <- time.period %>% 
    select(4)
  return(variables)
}


find.variables.for.time.period("your time period here")


#need to remove NAs and blanks from function



#BONUS: create a function that adds a column to the current LTDB dataset
# does not work don't fool yourselves. just playing around at 3am
povinfo <- slice(dd,57)
disinfo <- slice(dd,59)

dd <- dd %>% 
  mutate() %>%
  select(dd$X1970.f) %>%  
  mutate(disability.poverty.ratio=(povinfo/disinf0))
