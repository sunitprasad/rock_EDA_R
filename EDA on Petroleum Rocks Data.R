#------------------------------------------------------------------------
# Packages
#------------------------------------------------------------------------
# install if not preset
# install.packages(c("ggplot2))

options(scipen = 999) # disables the sci notation
library(ggplot2)
library(reshape2)

#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Data - Measurements on Petroleum Rock Samples
#------------------------------------------------------------------------
help(rock)
# Columns
# $ area : int  -  area of pores space, in pixels out of 256 by 256
# $ peri : num  -  perimeter in pixels
# $ shape: num  -  perimeter/sqrt(area)
# $ perm : num  -  permeability in milli-Darcies


data(rock)
rock <- rock

# Twelve core samples from petroleum reservoirs were sampled by 4 
# cross-sections. 
# Each core sample was measured for permeability, and each cross-section 
# has total area of pores, total perimeter of pores, and shape.
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Basic Explorations
#------------------------------------------------------------------------
str(rock)
summary(rock)
ncol(rock) # no of columns
nrow(rock) # no of rows

#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Statistical Indicators
#------------------------------------------------------------------------

#------------------------------------------------
# 0. Five number summary
#------------------------------------------------

# area
five_area <- round(quantile(rock$area),2)

# peri
five_peri <- round(quantile(rock$peri),2)

# shape
five_shape <- round(quantile(rock$shape),2)

# perm
five_perm <- round(quantile(rock$perm),2)

five_num <- data.frame(area=five_area,peri=five_peri,shape=five_shape,perm=five_perm)
five_num


#------------------------------------------------

#------------------------------------------------
# 1. Percentile Distribution of variables
# 0%  1%  5%  10%   20%  25%  50%   75%   90%  95%  99%  100%
#------------------------------------------------

# 1.1 area
quant_area <- round(quantile(rock$area, probs = c(0,0.01,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99,1)),2)

# 1.2 peri
quant_peri <- round(quantile(rock$peri, probs = c(0,0.01,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99,1)),2)

# 1.3 shape
quant_shape <- round(quantile(rock$shape, probs = c(0,0.01,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99,1)),2)

# 1.4 perm
quant_perm <- round(quantile(rock$perm, probs = c(0,0.01,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99,1)),2)

dist <- data.frame(quant_area,quant_peri,quant_shape,quant_perm)
#------------------------------------------------

#------------------------------------------------
# 2. Visulaizing distriutions - Whisker Plot
#------------------------------------------------


# 2.1 area
boxplot(rock$area)

# 2.2 peri
boxplot(rock$peri)

# 2.3 shape
boxplot(rock$shape)

# 2.4 perm
boxplot(rock$perm)


#------------------------------------------------

#------------------------------------------------
# 3. Central tendency
#------------------------------------------------

# 3.1 area
m1 <- mean(rock$area)
me1 <- median(rock$area)
mad1 <- mad(rock$area)

# 3.2 peri
m2 <- mean(rock$peri)
me2 <- median(rock$peri)
mad2 <- mad(rock$peri)

# 3.3 shape
m3 <- mean(rock$shape)
me3 <- median(rock$shape)
mad3 <- mad(rock$shape)

# 3.4 perm
m4 <- mean(rock$perm)
me4 <- median(rock$perm)
mad4 <- mad(rock$perm)

avg <- c(m1,m2,m3,m4)
median_val <- c(me1,me2,me3,me4)
mad_val <- c(mad1,mad2,mad3,mad4)

ct_and_disp <- data.frame(avg,median_val,mad_val)
rownames(ct_and_disp) <- c("area","peri","shape","perm")
#------------------------------------------------

#------------------------------------------------
# 4. Dispersion
#------------------------------------------------
# 4.1 area
var1 <- round(var(rock$area),2)
sd1 <- round(sd(rock$area),2)

# 4.2 peri
var2 <- round(var(rock$peri),2)
sd2 <- round(sd(rock$peri),2)

# 4.3 shape
var3 <- round(var(rock$shape),2)
sd3 <- round(sd(rock$shape),2)

# 4.4 perm
var4 <- round(var(rock$perm),2)
sd4 <- round(sd(rock$perm),2)

variance <- c(var1,var2,var3,var4)
std_dev <- c(sd1,sd2,sd3,sd4)
ct_and_disp <- cbind(ct_and_disp,variance,std_dev)

#------------------------------------------------

#------------------------------------------------
# 5. Probability Distributions (Histogram)
#------------------------------------------------

hist(rock$area,col="peachpuff",prob=T);lines(density(rock$area), col="chocolate")
hist(rock$peri,col="#F4A582",prob=T);lines(density(rock$peri), col="#2166AC")
hist(rock$shape,col="#FDDBC7",prob=T);lines(density(rock$shape), col="#2166AC")
hist(rock$perm,col="peachpuff",prob=T);lines(density(rock$perm), col="chocolate")

#------------------------------------------------

#------------------------------------------------
# 6. Single variable scatters
#------------------------------------------------
plot(x=1:48,y=rock$area,xlab = "index", ylab = "area")
plot(x=1:48,y=rock$peri,xlab = "index", ylab = "peri")
plot(x=1:48,y=rock$shape,xlab = "index", ylab = "shape")
plot(x=1:48,y=rock$perm,xlab = "index", ylab = "perm")

#------------------------------------------------

#------------------------------------------------
# 7. correlations
#------------------------------------------------
corr_mat <- round(cor(rock),2)
#------------------------------------------------


#-----------------------End of Step 1------------------------------------


#------------------------------------------------------------------------
# Step 2 : Plots using ggplot 2
#------------------------------------------------------------------------

# 1, Trend in area of pores space
# geom used : line (geom_line())
ggplot2::qplot(x = 1:48,y=rock$area, geom = "line",xlab = "index",ylab = "Area") + 
  labs(caption = "This trend chart shows that pore area is higher on\n the first two cross-sections and decreases\n as we keep progressing to the next sections") 

# 2, Distribution of Shape
# geom used : geom_density()
ggplot2::ggplot(data = rock) + aes(rock$shape) +
  geom_density(aes(rock$shape),data = rock,color="darkblue",fill="lightblue")+
  labs(subtitle = "shape follows a Gamma Distribution which usually arises naturally in processes for which the waiting times between two events are significant.\n The shape of a rock is influenced by various climatic events and geological surroundings and the fact that these rocks are collected at various reservoirs,\n it definitely would show a trend of a queueing models and hence the Gamma Distribution") 

# 3, correlation between shape and permeability
# does the shape of a rock effect it's permeability?
cor(x = rock$shape, y = rock$perm)
# geom used : geom_point() and geom_smooth()

ggplot2::qplot(x = rock$shape, y = rock$perm, geom = "point") + 
  xlab("Shape")+ylab("Permeability") + 
  geom_smooth(method = 'auto') +
  labs(subtitle = "The correlation coeff is approx 56% and the graph shows that there is a 'fair' amount of influence of shape of a rock on it's permeability",
       caption = "permeability is the abilty of a rock to store or allow gases and liquids to pass through them. Reservoirs of lower permiability are more preferable for further explorations"
       ) 

# 4. Overall correlations on a heatmap
# geom used : geom_tile()
# visualizaing correlation in a heatmap

melted_cormat <- reshape2::melt(corr_mat)
heatmap <- ggplot2::ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, 
                       name="Corr%") +
  geom_tile()
# adding captions via geom_text()
heatmap <- heatmap + geom_text(aes(Var2, Var1, label = paste0(value*100,"%")), color = "black", size = 4)
plot(heatmap)

#------------------------------------------------------------------------
