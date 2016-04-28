specie <- "BAOR"
prob <- "Psi"
prob <- "Gam"
prob <- "Eps"

add.new()

add.new <- function(){
  file <- file.path("./data", paste(specie, prob, ".txt", sep = ""))
  d <- read.table(file, header = T)
  dname <- paste0(specie, prob)
  names(d)[2:4] <- c("x", "y", dname)
  dd <- read.csv("./data/Predicted_values.csv", header = T)
  newd <- cbind(dd, d[, dname])
  names(newd) <- c(colnames(dd), dname)
  
  Range <- index %>% filter(Abbr == specie) %>% select(Range_Restrict) %>% as.character
  Range <- unlist(strsplit(Range, ", "))
  print(all(Range %in% levels(est$LAND)))
  if (length(Range)>0) {
    newd[!(est$LAND %in% Range), dname] <- NA
  }
  
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

wood.w <- index %>% filter(Kind == "Bird" & Category == "Woodland") %>% select(c(Abbr, Psi, Gam))
wood_name <- outer(wood.w$Abbr, c("Psi", "Gam"), paste0)
wood.psi <- est %>% select(one_of(wood_name[, 1]))
wood.gam <- est %>% select(one_of(wood_name[, 2]))

grs.w <- index %>% filter(Kind == "Bird" & Category == "Grassland") %>% select(c(Abbr, Psi, Gam))
grs_name <- outer(grs.w$Abbr, c("Psi", "Gam"), paste0)
grs.psi <- est %>% select(one_of(grs_name[, 1]))
grs.gam <- est %>% select(one_of(grs_name[, 2]))

ss.w <- index %>% filter(Kind == "Bird" & Category == "Scrub-shrub") %>% select(c(Abbr, Psi, Gam))
ss_name <- outer(ss.w$Abbr, c("Psi", "Gam"), paste0)
ss.psi <- est %>% select(one_of(ss_name[, 1]))
ss.gam <- est %>% select(one_of(ss_name[-3, 2]))

data <- wood.gam
rp <- matrix(rep(wood.w$Gam, each = nrow(data)), nr = nrow(data))
rp[is.na(data)] <- NA
rp <- rp/rowSums(rp, na.rm = T)
data <-  rowSums(data*rp, na.rm = T)
data <- est %>% select(PNTID, x, y) %>% bind_cols(data.frame(data)) 
names(data) <- c("PNTID", "UTM_x", "UTM_y", "Gam")
write.table(data, file = "Woodland Birds_Gam.txt", quote = FALSE, row.names = F)
