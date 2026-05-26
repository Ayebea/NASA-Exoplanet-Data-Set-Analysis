#load dataset and library
path <- "/Users/efuaasamani-baah/Downloads/cleaned_5250.csv"

e <- read.csv(path)

library(ggplot2)

#Distance analysis

p <- ggplot(e,aes(x=planet_type,y=distance)) + geom_jitter()+ 
  labs(title = "Exoplanet Distance by Planet Type", x = "Planet Type", y = "Distance in Light Years") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p2 <- ggplot(e,aes(x=distance)) + geom_histogram(binwidth = 2000, color = "black", fill = "lightgrey") + 
  labs(title = "Histogram of Distance", x = "Distance", y = "Frequency") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
mean(e$distance,na.rm = TRUE)
max(e$distance,na.rm = TRUE)
min(e$distance,na.rm = TRUE)
quantile(e$distance,na.rm = TRUE)

#Stellar Magnitude analysis

p3 <- ggplot(e,aes(x=planet_type,y=stellar_magnitude)) + geom_jitter()+ 
  labs(title = "Exoplanet Stellar Magnitude by Planet Type", x = "Planet Type", y = "Stellar Magnitude") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p4 <- ggplot(e,aes(x=stellar_magnitude)) + geom_histogram(binwidth = 5, color = "black", fill = "lightgrey") + 
  labs(title = "Histogram of Stellar Magnitude", x = "Distance", y = "Frequency") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
mean(e$stellar_magnitude,na.rm = TRUE)
max(e$stellar_magnitude,na.rm = TRUE)
min(e$stellar_magnitude,na.rm = TRUE)
quantile(e$stellar_magnitude,na.rm = TRUE)

#Planet Types

c <- table(e$planet_type)
typepercent<- round(100 * c / sum(c), 1)
p5 <- pie(c,main = "Planet Type Distribution",labels = typepercent,col =rainbow(length(e)))
legend("topright",c("Gas Giant","Super Earth","Neptune-like","Terrestrial","Unknown"),fill=rainbow(length(e)))

#Mass Wrt

e_clean <- subset(e, mass_wrt == "Earth" | mass_wrt == "Jupiter")
d <- table(e_clean$mass_wrt)
masswrtpercent<- round(100 * d / sum(d),1)
p6 <- pie(d,main = "Mass Wrt Distribution",labels = masswrtpercent,col =rainbow(length(e_clean)))
legend("topright",c("Earth","Jupiter"),fill=rainbow(length(e_clean)))

#Radius Wrt
e1_clean <- subset(e, radius_wrt == "Earth" | radius_wrt == "Jupiter")
f <- table(e1_clean$radius_wrt)
radiuswrtpercent<- round(100 * f / sum(f),1)
p7 <- pie(f,main = "Radius Wrt Distribution",labels = radiuswrtpercent,col =rainbow(length(e1_clean)))
legend("topright",c("Earth","Jupiter"),fill=rainbow(length(e1_clean)))

#Mass multiplier
p8 <- ggplot(e,aes(x=mass_multiplier)) + geom_histogram(color = "black", fill = "lightgrey") + 
  labs(title = "Histogram of Mass Multiplier", x = "Mass Multiplier", y = "Frequency") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
mean(e$mass_multiplier,na.rm = TRUE)
max(e$mass_multiplier,na.rm = TRUE)
min(e$mass_multiplier,na.rm = TRUE)
quantile(e$mass_multiplier,na.rm = TRUE)

#Radius Multiplier
p9 <- ggplot(e,aes(x=planet_type,y=radius_multiplier)) + geom_jitter()+ 
  labs(title = "Exoplanet Radius Multiplier by Planet Type", x = "Planet Type", y = "Radius Multiplier") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p10 <- ggplot(e,aes(x=radius_multiplier)) + geom_histogram( color = "black", fill = "lightgrey") + 
  labs(title = "Histogram of Radius Multiplier", x = "Radius Multiplier", y = "Frequency") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
mean(e$radius_multiplier,na.rm = TRUE)
max(e$radius_multiplier,na.rm = TRUE)
min(e$radius_multiplier,na.rm = TRUE)
quantile(e$radius_multiplier,na.rm = TRUE)

#Orbital Radius
p11 <- ggplot(e,aes(x=planet_type,y=orbital_radius)) + geom_jitter()+ 
  labs(title = "Exoplanet Orbital Radius by Planet Type", x = "Planet Type", y = "Orbital Radius") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p12 <- ggplot(e,aes(x=orbital_radius)) + geom_histogram(binwidth = 100, color = "black", fill = "lightgrey") + 
  labs(title = "Histogram of Orbital Radius", x = "Orbital Radius", y = "Frequency") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
mean(e$orbital_radius,na.rm = TRUE)
max(e$orbital_radius,na.rm = TRUE)
min(e$orbital_radius,na.rm = TRUE)
quantile(e$orbital_radius,na.rm = TRUE)

#Correlation Testing for distance and stellar magnitude
p13 <- ggplot(e,aes(x=stellar_magnitude,y=distance)) + geom_point(shape = 1, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  labs(title = "Distance vs Stellar Magnitude", x = "Stellar Magnitude", y = "Distance") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

coef(lm( distance ~ stellar_magnitude, data = e))

cor.test(e$distance,e$stellar_magnitude, method = 'pearson')

#R-squared value
cor(ep$distance, ep$stellar_magnitude, method = "pearson", use = "complete.obs")^2

#Linear Regression Models
plot(lm( distance ~ stellar_magnitude, data = e))

#Normal Q-Q Plot of Residuals
model <- lm(distance ~ stellar_magnitude, data = e)
residuals <- residuals(model)
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
qqline(residuals, col = "red", lwd = 2)

#Residual standard error
summary(model)$sigma

#Planet Type vs Stellar Magnitude Box Plot
p14 <- ggplot(e,aes(x=planet_type,y=stellar_magnitude)) +
  geom_boxplot(color = "black", linewidth = 0.5,fill = "gray") +
  labs(title = "Planet Type vs Stellar Magnitude",x="Planet Type", y = "Stellar Magnitude") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#ANOVA test for Planet Type vs Stellar Magnitude
aov_test <-aov(stellar_magnitude ~ planet_type,data = e)
summary(aov_test)

#Normal Q-Q Plot
model2 <- lm(stellar_magnitude ~ planet_type, data = e)
residuals <- residuals(model2)
qqnorm(residuals, main = "Normal Q-Q Plot", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")

#Pairwise Test
pairwise.t.test(e$stellar_magnitude,e$planet_type,p.adjust.method = "bonferroni")

#Mass Multiplier vs Mass Wrt Box Plot
p15 <- ggplot(e1_clean,aes(x=mass_wrt,y=mass_multiplier)) +
  geom_boxplot(color = "black", linewidth = 0.5,fill = "gray") +
  labs(title = "Mass Multiplier vs Mass Wrt",x="Mass Wrt", y = "Mass Multiplier") + 
  theme_classic(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#ANOVA test for Mass Multiplier vs Mass Wrt
aov_test <-aov(mass_multiplier ~ mass_wrt,data = e)
summary(aov_test)

#Normal Q-Q Plot
model3 <- lm(mass_multiplier ~ mass_wrt, data = e)
residuals <- residuals(model3)
qqnorm(residuals, main = "Normal Q-Q Plot", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")

#Tukey Test
e.lm <- lm(mass_multiplier ~ mass_wrt, data = e)
e.av <- aov(e.lm)
tukey.test <- TukeyHSD(e.av)
tukey.test
