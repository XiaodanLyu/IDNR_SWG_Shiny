pub <- readxl::read_excel("./data/Public lands coordinates2.xlsx", sheet = 1)
colnames(pub)[1:2] <- c("x", "y")
pn <- unique(pub$NAME)
sp <- pub[1:2]
myplot <- function(){
  plot(pub$`UTM X`, pub$`UTM Y`, type = "n") 
  for (x in pn)
    polygon(sp[pub$NAME == x, ])
}
#myplot2 <- compiler::cmpfun(myplot)
#system.time(myplot2())
system.time(myplot())

library(tidyr)
library(dplyr)
pub_nested <- pub %>% select(x, y, NAME) %>% nest(x, y) 
plot(pub$x, pub$y, type = "n")
library(purrr)
res <- map(pub_nested$data, polygon)
library(ggplot2)
ggplot(data = pub, aes(x = x, y = y)) + geom_polygon(color = "black", fill = NA, aes(group = NAME))
