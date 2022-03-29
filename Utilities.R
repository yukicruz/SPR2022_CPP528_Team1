library(here)
library(dplyr)
library(stringr)
# store data dictionary file path
DD_FILEPATH <- here::here( "data/rodeo/ltdb_data_dictionary.csv" )

# import data dictionary
dd <- read.csv( DD_FILEPATH, stringsAsFactors=F )

# create a function that searches variable descriptions for a specific string and returns any 
# that match. 
aapi <- c (str_detect(dd$definition, regex("Asian", ignore_case = T)),
           str_detect(dd$definition, regex("pacific", ignore_case = T)),
           str_detect(dd$definition, regex("korean", ignore_case = T)),
           str_detect(dd$definition, regex("japanese", ignore_case = T)),
           str_detect(dd$definition, regex("vietnamese", ignore_case = T)))

asians.pacific.islanders <- cbind(dd,aapi) %>% 
  filter(aapi == T) 
View(asians.pacific.islanders)


dd %>% filter(str_detect(definition, "poverty")) %>%
  slice()

# Create a function to filter variables by time periods. 
#Specifically, the user will specify the time periods of interest for the study and the
#function will identify all variables that have measures for those periods.

# choose from time periods:
variables.1970s
variables.1980s
variables.1990s
variables.2000s
variables.2010s


find.variables.for.time.period <- function(time.period) {
  variables <- time.period %>% 
    select(4)
  return(variables)
}



find.variables.for.time.period("your time period here")

#need to remove NAs and blanks from function

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

#BONUS: create a function that adds a column to the current LTDB dataset
# does not work don't fool yourselves. just playing around at 3am
povinfo <- slice(dd,57)
disinfo <- slice(dd,59)

dd <- dd %>% 
  mutate() %>%
  select(dd$X1970.f) %>%  
  mutate(disability.poverty.ratio=(povinfo/disinf0))
