specie <- "KEWA"
prob <- "Psi"
prob <- "Gam"
add.new()

add.new <- function(){
  file <- file.path("./data", paste(specie, prob, ".txt", sep = ""))
  d <- read.table(file, header = T)
  dname <- paste0(specie, prob)
  names(d)[2:4] <- c("x", "y", dname)
  dd <- read.csv("./data/Predicted_values.csv", header = T)
  newd <- cbind(dd, d[, dname])
  names(newd) <- c(colnames(dd), dname)
  write.csv(newd, "./data/Predicted_values.csv", row.names  = F)
}

library(ggplot2)
ggplot(data = newd, aes(x = x, y = y, z = as.numeric(LAND), fill = NOBOPsi)) + geom_tile() +
  scale_fill_gradient2(limits = range(dd[, 5]), low = "skyblue2",
                       high = "red", mid = "yellow", midpoint = 0.5) +
    geom_contour(color = "black")

reg <- readxl::read_excel("./data/Coordinates with region.xlsx")
names(reg)[5:6] <- c("x", "y")
reg$y <- round(reg$y,3)
newd <- dplyr::left_join(d, reg[, 5:7], by = c("x", "y"))

require(ggvis)
d %>% ggvis(~x, ~y, fill=~get(dname)) %>% layer_points() %>%
  scale_numeric("fill", range = "category10")

Range <- index %>% filter(Abbr == "KEWA") %>% select(Range_Restrict) %>% as.character
Range <- unlist(strsplit(Range, ", "))
all(Range %in% levels(est$LAND))
newd[!(est$LAND %in% Range), dname] <- NaN
