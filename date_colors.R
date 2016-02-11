year_colors <- function(series){
  
  year.color = function(year) { 
    color = switch(year,
                   'blue', 'red', 'orange', 'purple', 'green'
    )
  }
  return(apply(as.data.frame(.indexyear(series)-110), 1, year.color))
  
}