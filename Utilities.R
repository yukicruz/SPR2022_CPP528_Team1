catsearch <- function( string, dd )
{
  vector <- dd$category
  these <- grepl( string, vector, ignore.case=T )
  dd.sub <- dd[ these, ]
  return( dd.sub )
}

varsearch <- function( string, dd )
{
  vector <- dd$definition
  these <- grepl( string, vector, ignore.case=T )
  dd.sub <- dd[ these, ]
  return( dd.sub )
}

find.variables.for.time.period <- function(time.period) {
  variables <- time.period %>% 
    select(4)
  return(variables)
}