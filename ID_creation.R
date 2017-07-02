mtcars$ID <- rep(NA, nrow(mtcars))
library(stringr)
for (i in 1:nrow(mtcars)){
  mtcars$ID[i] <- paste0(unlist(strsplit(rownames(mtcars)[i]," "))[1],as.integer(rnorm(1,150,150)))
}

cars_table <- mtcars %>% 
  mutate(performance = hp/mpg, price = performance *3000, year = as.integer(rnorm(nrow(mtcars), 2000,10)))%>%
  select(ID, performance, year)

cars_table <- cars_table %>% sample_frac(size = 0.5, replace = FALSE) 
