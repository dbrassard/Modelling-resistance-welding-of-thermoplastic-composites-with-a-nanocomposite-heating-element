# Load packages

library(ggplot2)
library(sp)
library(raster)
library(grid)
library(scales)
library(readr)
library(dplyr)
library(viridis)

############################################################
### Fonctions
############################################################


### Nettoyage des données
datacleanup <- function(data_lab,data_validation) {

  Time <- data_lab$Temps
  Temperature <- data_lab$T1
  Point <- rep("Thermocouple 1",NROW(data_lab))
  Category <- rep("Lab",NROW(data_lab))
  graph.data <- data.frame(Time, Temperature, Point, Category)
  
  Temperature <- data_lab$T2
  Point <- rep("Thermocouple 2",NROW(data_lab))
  Category <- rep("Lab",NROW(data_lab))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Temperature <- data_lab$T3
  Point <- rep("Thermocouple 3",NROW(data_lab))
  Category <- rep("Lab",NROW(data_lab))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Temperature <- data_lab$T4
  Point <- rep("Thermocouple 4",NROW(data_lab))
  Category <- rep("Lab",NROW(data_lab))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Time <- data_validation$`t (s)`
  Temperature <- data_validation$`Thermocouple 1`
  Point <- rep("Thermocouple 1",NROW(data_validation))
  Category <- rep("Comsol",NROW(data_validation))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Temperature <- data_validation$`Thermocouple 2`
  Point <- rep("Thermocouple 2",NROW(data_validation))
  Category <- rep("Comsol",NROW(data_validation))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Temperature <- data_validation$`Thermocouple 3`
  Point <- rep("Thermocouple 3",NROW(data_validation))
  Category <- rep("Comsol",NROW(data_validation))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)
  
  Temperature <- data_validation$`Thermocouple 4`
  Point <- rep("Thermocouple 4",NROW(data_validation))
  Category <- rep("Comsol",NROW(data_validation))
  temp.data <- data.frame(Time, Temperature, Point, Category)
  graph.data <- bind_rows(graph.data, temp.data)

  return(graph.data)
}


############################################################
### Création d'un thème pour les articles d'Elsevier
############################################################

width_pdf <- 3.25
height_pdf1 <- 3
height_pdf2 <- 2.5
ligne_geometrie <- 0.2
taille_axe <- 0.4
n <- 3

elsevier_theme <- theme_bw() + theme( 
  line = element_line(size=0.4),
  axis.line = element_line(size=taille_axe, color = "black"),
  text = element_text(size = 16, family="Times", colour = "black"),
  axis.text = element_text(colour="black"), 
  axis.ticks = element_line(colour = "black"),
  panel.background = element_rect(fill="white", colour="black"),
  panel.grid = element_blank()
)

LegendTitle = c("Thermocouple 1", "Thermocouple 2", "Thermocouple 3", "Thermocouple 4")
linetype = c("dotted","solid", "twodash")

############################################################
### colorblind-friendly palette
############################################################

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
col1 <- "#000000"
col2 <- "#D55E00"
col3 <- "#0072B2"
col4 <- "#E69F00"
ColLegend = rep(c(col1, col2, col3, col4),2)
# from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

############################################################
### Graphiques de validation
############################################################

#### 275kW-900-10-000
#### Chargement des données

data_lab <- read_csv("./Data/275kW-900-10-000-4UD_modif.csv")
data_validation <- read_csv("./Data/275kW-900-10-000-validation.csv")

#### Massage des données

graph.data <- datacleanup(data_lab,data_validation)

#### Graphique


plot_a <- ggplot(data = graph.data, aes(x = Time, y = Temperature, colour = factor(Point), linetype = factor(Category)))
plot_a <- plot_a + geom_line(size=1) + xlim(0,900) + ylim(20,275)
plot_a <- plot_a + xlab("Time [s]") + ylab("Temperature [°C]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(0,300,25)) + scale_x_continuous(breaks=seq(0,900,100))
plot_a <- plot_a + scale_color_manual(name="Locations", labels = LegendTitle , values = ColLegend)
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("COMSOL", "Experiment") , values = linetype )
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.position=c(0.95, 0.05), legend.justification = c("right", "bottom"))
pdf("275kW-900-10-000-validation.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("275kW-900-10-000-validation.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("275kW-900-10-000-validation.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 275kW-900-10-130
#### Chargement des données

data_lab <- read_csv("./Data/275kW-900-10-130-2UD_modif.csv")
data_validation <- read_csv("./Data/275kW-900-10-130-validation.csv")

#### Massage des données

graph.data <- datacleanup(data_lab,data_validation)

#### Graphique

plot_a <- ggplot(data = graph.data, aes(x = Time, y = Temperature, colour = factor(Point), linetype = factor(Category)))
plot_a <- plot_a + geom_line(size=1) + xlim(0,900) + ylim(20,275)
plot_a <- plot_a + xlab("Time [s]") + ylab("Temperature [°C]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(0,300,25)) + scale_x_continuous(breaks=seq(0,900,100))
plot_a <- plot_a + scale_color_manual(name="Locations", labels = LegendTitle , values = ColLegend)
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("COMSOL", "Experiment") , values = linetype )
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.position=c(0.95, 0.05), legend.justification = c("right", "bottom"))
pdf("275kW-900-10-130-validation.pdf", width = 2*width_pdf, height = 2*height_pdf2)
plot_a
dev.off() 

svg("275kW-900-10-130-validation.svg", width = 2*width_pdf, height = 2*height_pdf2)
plot_a
dev.off() 

tiff("275kW-900-10-130-validation.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 350kW-120-10-000
#### Chargement des données

data_lab <- read_csv("./Data/350kW-120-10-000-4UD_modif.csv")
data_validation <- read_csv("./Data/350kW-120-10-000-validation.csv")

#### Massage des données

graph.data <- datacleanup(data_lab,data_validation)

#### Graphique

plot_a <- ggplot(data = graph.data, aes(x = Time, y = Temperature, colour = factor(Point), linetype = factor(Category)))
plot_a <- plot_a + geom_line(size=1) 
plot_a <- plot_a + xlab("Time [s]") + ylab("Temperature [°C]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(0,200,25)) + scale_x_continuous(breaks=seq(0,120,20))
plot_a <- plot_a + scale_color_manual(name="Locations", labels = LegendTitle , values = ColLegend)
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("COMSOL", "Experiment") , values = linetype )
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.position=c(0.95, 0.05), legend.justification = c("right", "bottom")) 
pdf("350kW-120-10-000-validation.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("350kW-120-10-000-validation.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("350kW-120-10-000-validation.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 350kW-120-10-130
#### Chargement des données

data_lab <- read_csv("./Data/350kW-120-10-130-1UD_modif.csv")
data_validation <- read_csv("./Data/350kW-120-10-130-validation.csv")

#### Massage des données

graph.data <- datacleanup(data_lab,data_validation)

#### Graphique

plot_a <- ggplot(data = graph.data, aes(x = Time, y = Temperature, colour = factor(Point), linetype = factor(Category)))
plot_a <- plot_a + geom_line(size=1) 
plot_a <- plot_a + xlab("Time [s]") + ylab("Temperature [°C]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(0,225,25)) + scale_x_continuous(breaks=seq(0,120,20))
plot_a <- plot_a + scale_color_manual(name="Locations", labels = LegendTitle , values = ColLegend)
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("COMSOL", "Experiment") , values = linetype )
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.position=c(0.95, 0.05), legend.justification = c("right", "bottom")) 
pdf("350kW-120-10-130-validation.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("350kW-120-10-130-validation.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("350kW-120-10-130-validation.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

############################################################
### Graphiques profil de température
############################################################

#### 275kW-900-10-000
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_275kW-900-10-000.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=900`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(100, 500, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("275kW-900-10-000-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("275kW-900-10-000-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("275kW-900-10-000-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 275kW-900-10-130
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_275kW-900-10-130.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7 -1.3
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=900`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(100, 500, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("275kW-900-10-130-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("275kW-900-10-130-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("275kW-900-10-130-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 275kW-900-10-200
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_275kW-900-10-200.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7 -2
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=900`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(100, 400, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("275kW-900-10-200-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("275kW-900-10-200-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("275kW-900-10-200-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 350kW-120-10-000
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_350kW-120-10-000.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=120`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(100, 500, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("350kW-120-10-000-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("350kW-120-10-000-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("350kW-120-10-000-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 350kW-120-10-130
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_350kW-120-10-130.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7 -1.3
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=120`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(100, 500, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("350kW-120-10-130-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("350kW-120-10-130-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off()

tiff("350kW-120-10-130-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

#### 350kW-120-10-200
#### Chargement des données

ProfilT <- read_csv("./Data/Profil_T_nanocomposite_350kW-120-10-200.csv", skip = 8)

Xcoord <- ProfilT$`% s1` *1000 -12.7 -2
Ycoord <- ProfilT$s2 *1000
Temperature <- ProfilT$`T (degC) @ t=120`

TField <- data.frame( Xcoord, Ycoord, Temperature)

#### Graphique

plot_a <- ggplot(data = TField, aes(x = Xcoord, y = Ycoord))
plot_a <- plot_a + geom_raster(aes(fill = Temperature), interpolate = TRUE) 
plot_a <- plot_a + scale_fill_viridis(option="inferno", name = "Temperature \n [°C]", breaks = c(seq(300, 500, by=50), floor(max(Temperature)), ceiling(min(Temperature))))
plot_a <- plot_a + xlab("X coordinate [mm]") + ylab("Y coordinate [mm]") 
plot_a <- plot_a + scale_x_continuous(breaks=seq(0,25,5)) + scale_y_continuous(breaks=seq(0,12.5,2.5))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(aspect.ratio = 0.5, legend.position = "bottom", legend.key.width = unit(2,"cm"))
pdf("350kW-120-10-200-Tprofile.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("350kW-120-10-200-Tprofile.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("350kW-120-10-200-Tprofile.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

############################################################
### Graphiques tolérance "Clamping Distance"
############################################################

PowerDensity <- rep(c(275, 300, 325, 350),2)
Distance <- c(1.25, 1.2, 1.15, 1.1, 1.7, 1.6, 1.6, 1.5)
Condition <- c(rep("Equal temperature",4), rep("Edge degradation",4))

graph.data <- data.frame(PowerDensity, Distance, Condition)

plot_a <- ggplot(data = graph.data, aes(x = PowerDensity, y = Distance, linetype = factor(Condition)))
plot_a <- plot_a + geom_line(size=1) 
plot_a <- plot_a + xlab("Power density [kW/mÂ²]") + ylab("Clamping distance [mm]")
plot_a <- plot_a + scale_x_continuous(breaks=seq(275,350,25)) + scale_y_continuous(breaks=seq(1,2,.1))
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("Edge degradation", "Equal temperature") , values = linetype )
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.position="bottom") 
pdf("Clamping_Distance_tolerance.pdf", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("Clamping_Distance_tolerance.svg", width = 2*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("Clamping_Distance_tolerance.tiff", width = 3543, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

############################################################
### Graphiques fenêtre de procédé
############################################################

linetype = c("solid", "dotted", "dotdash")

average_temperature_evolution <- read_csv("Data/average_Temperature_evolution.csv")

avg_300 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 300)
  avg_300 <- c(avg_300, b$y)
}

avg_350 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 350)
  avg_350 <- c(avg_350, b$y)
}

avg_360 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 360)
  avg_360 <- c(avg_360, b$y)
}

avg_370 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 370)
  avg_370 <- c(avg_370, b$y)
}

avg_375 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 375)
  avg_375 <- c(avg_375, b$y)
}

max_temperature_evolution <- read_csv("Data/max_Temperature_evolution.csv")

max_440 <- c()
for (PD in seq(280, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 440)
  max_440 <- c(max_440, b$y)
}

max_400 <- c()
for (PD in seq(260, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 400)
  max_400 <- c(max_400, b$y)
}

max_370 <- c()
for (PD in seq(260, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 370)
  max_370 <- c(max_370, b$y)
}

P <- c(seq(280, 400, by = 20), seq(260, 400, by = 20), seq(260, 400, by = 20))
t <- c(max_440, max_400, max_370)
Category <- c(rep("T_max_440",length(max_440)), rep("T_max_400",length(max_400)), rep("T_max_370",length(max_370)))

#P <- c(rep(seq(240, 400, by = 20), 2), seq(280, 400, by = 20))
#t <- c(190, 112, 63, 42.5, 28.5, 22.5, 18.5, 15.25, 13.25, 955, 430, 244, 149, 103, 68, 52, 34, 27, 2000, 850, 450, 275, 188, 130, 95)
#Category <- c(rep("T_moy_300",9),rep("T_moy_350",9), rep("T_max_440",7))

graph.data <- data.frame(P, t, Category)

### Remettre en ordre les lignes
graph.data$Category <- factor(graph.data$Category, levels = c("T_max_440", "T_max_400", "T_max_370"))

P <-        c( 350,  350,  350,  350,  350,  350,   300,  300,  300,  300,  300,  290,  290,  290, 275,  275,  250,  250,  200)
t <-        c(  45,   60,   70,   90,  120,  200,   150,  240,  360,  600,  900,  300,  600,  900, 900,  500,  360,  500,  900)
LSS <-      c(17.4, 16.4, 18.6, 15.5, 24.9, 21.0, 14.85, 13.8, 17.7, 19.4, 16.5, 18.9, 17.8, 21.4,  20, 14.3, 13.2, 14.1,  3.1)

graph.data2 <- data.frame(P, t, LSS)

# # Large process zone
# polygon_X <- c( 37.7, 186.2, 241.8, 380.8, 707.3, 1637.4, 1637.4, 707.3, 455.3, 277.5, 188.8, 143.3,  37.7)
# polygon_Y <- c(355.3, 355.3, 341.9, 321.0, 300.2, 280.3,   268.1, 268.1, 281.6, 297.6, 311.1, 320.3, 355.3)

# Small process zone
polygon_X <- c( 84.9, 186.3, 241.8, 380.8, 707.3, 1637.4, 1637.4, 707.3, 474.6, 314.1, 162.2, 115.0,  84.9)
polygon_Y <- c(355.3, 355.3, 341.9, 321.0, 300.2, 280.3,   268.1, 268.1, 280.2, 294.6, 322.8, 338.0, 355.3)

graph.polygon <- data.frame(polygon_X, polygon_Y)

plot_a <- ggplot(data = graph.data, aes(x = t, y = P ))
plot_a <- plot_a + geom_line(size=1, aes(linetype = factor(Category)))
plot_a <- plot_a + geom_point(data = graph.data2 , size =3, aes(x = t, y = P, shape=factor(floor(graph.data2$LSS / 5))))
plot_a <- plot_a + geom_polygon(data = graph.polygon, aes(x = polygon_X , y = polygon_Y), alpha = 0.15 )
plot_a <- plot_a + xlab("Time [s]") + ylab("Power density [kW/m²]")
plot_a <- plot_a + scale_x_log10(breaks = c(10 ,100, 1000, 10000), limits = c(10, 10000)) + scale_y_continuous(breaks=seq(200,420,50))
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("Maximum temperature over 440 °C", "Maximum temperature over 400 °C", "Maximum temperature over 370 °C"),  values = linetype )
plot_a <- plot_a + scale_shape(solid = FALSE, name="Average LSS of welding experiments [MPa]", label=c("[0 , 5[","[10 , 15[","[15 , 20[","[20 , 25[","[25 , 30]"))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.key.height=unit(1, "cm"))
plot_a <- plot_a + guides(col = guide_legend(nrow = 2))
plot_a

pdf("Process_window_log.pdf", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("Process_window_log.svg", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off()

tiff("Process_window_log.tiff", width = 3543*1.5, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 14, res = 500, compression = "lzw")
plot_a
dev.off() 


plot_a <- ggplot(data = graph.data, aes(x = t, y = P ))
plot_a <- plot_a + geom_line(size=1, aes(linetype = factor(Category)))
plot_a <- plot_a + geom_point(data = graph.data2, size = 3, aes(x = t, y = P, shape=factor(floor(graph.data2$LSS / 5))))
plot_a <- plot_a + xlab("Time [s]") + ylab("Power density [kW/mÂ²]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(200,420,20))
plot_a <- plot_a + scale_linetype_manual(name="Test", labels = c("Maximum temperature over 400 °C", "Maximum temperature over 440 °C"),  values = linetype )
plot_a <- plot_a + scale_shape(solid = FALSE, name="Average LSS of welding experiments [MPa]", label=c("[0 , 5[","[10 , 15[","[15 , 20[","[20 , 25[","[25 , 30]"))
#plot_a <- plot_a + geom_point(data = graph.data3, size = 3, color = "red", aes(x = t, y = P))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.key.height=unit(1, "cm"))
plot_a <- plot_a + guides(col = guide_legend(nrow = 2))
plot_a

pdf("Process_window.pdf", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("Process_window.svg", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("Process_window.tiff", width = 3543 *1.5, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 

############################################################
### Graphiques fenêtre de procédé traduit
############################################################

linetype = c("solid", "dotted", "dotdash")

average_temperature_evolution <- read_csv("Data/average_Temperature_evolution.csv")

avg_300 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 300)
  avg_300 <- c(avg_300, b$y)
}

avg_350 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 350)
  avg_350 <- c(avg_350, b$y)
}

avg_360 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 360)
  avg_360 <- c(avg_360, b$y)
}

avg_370 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 370)
  avg_370 <- c(avg_370, b$y)
}

avg_375 <- c()
for (PD in seq(240, 400, by = 20)){
  b <- approx(subset(average_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 375)
  avg_375 <- c(avg_375, b$y)
}

max_temperature_evolution <- read_csv("Data/max_Temperature_evolution.csv")

max_440 <- c()
for (PD in seq(280, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 440)
  max_440 <- c(max_440, b$y)
}

max_400 <- c()
for (PD in seq(260, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 400)
  max_400 <- c(max_400, b$y)
}

max_370 <- c()
for (PD in seq(260, 400, by = 20)){
  b <- approx(subset(max_temperature_evolution, Power_density == PD, select = c(Temperature,time)), xout = 370)
  max_370 <- c(max_370, b$y)
}

P <- c(seq(280, 400, by = 20), seq(260, 400, by = 20), seq(260, 400, by = 20))
t <- c(max_440, max_400, max_370)
Category <- c(rep("T_max_440",length(max_440)), rep("T_max_400",length(max_400)), rep("T_max_370",length(max_370)))

#P <- c(rep(seq(240, 400, by = 20), 2), seq(280, 400, by = 20))
#t <- c(190, 112, 63, 42.5, 28.5, 22.5, 18.5, 15.25, 13.25, 955, 430, 244, 149, 103, 68, 52, 34, 27, 2000, 850, 450, 275, 188, 130, 95)
#Category <- c(rep("T_moy_300",9),rep("T_moy_350",9), rep("T_max_440",7))

graph.data <- data.frame(P, t, Category)

### Remettre en ordre les lignes
graph.data$Category <- factor(graph.data$Category, levels = c("T_max_440", "T_max_400", "T_max_370"))

P <-        c( 350,  350,  350,  350,  350,  350,   300,  300,  300,  300,  300,  290,  290,  290, 275,  275,  250,  250,  200)
t <-        c(  45,   60,   70,   90,  120,  200,   150,  240,  360,  600,  900,  300,  600,  900, 900,  500,  360,  500,  900)
LSS <-      c(17.4, 16.4, 18.6, 15.5, 24.9, 21.0, 14.85, 13.8, 17.7, 19.4, 16.5, 18.9, 17.8, 21.4,  20, 14.3, 13.2, 14.1,  3.1)

graph.data2 <- data.frame(P, t, LSS)

# # Large process zone
# polygon_X <- c( 37.7, 186.2, 241.8, 380.8, 707.3, 1637.4, 1637.4, 707.3, 455.3, 277.5, 188.8, 143.3,  37.7)
# polygon_Y <- c(355.3, 355.3, 341.9, 321.0, 300.2, 280.3,   268.1, 268.1, 281.6, 297.6, 311.1, 320.3, 355.3)

# Small process zone
polygon_X <- c( 84.9, 186.3, 241.8, 380.8, 707.3, 1637.4, 1637.4, 707.3, 474.6, 314.1, 162.2, 115.0,  84.9)
polygon_Y <- c(355.3, 355.3, 341.9, 321.0, 300.2, 280.3,   268.1, 268.1, 280.2, 294.6, 322.8, 338.0, 355.3)

graph.polygon <- data.frame(polygon_X, polygon_Y)

plot_a <- ggplot(data = graph.data, aes(x = t, y = P ))
plot_a <- plot_a + geom_line(size=1, aes(linetype = factor(Category)))
plot_a <- plot_a + geom_point(data = graph.data2 , size =3, aes(x = t, y = P, shape=factor(floor(graph.data2$LSS / 5))))
plot_a <- plot_a + geom_polygon(data = graph.polygon, aes(x = polygon_X , y = polygon_Y), alpha = 0.15 )
plot_a <- plot_a + xlab("Temps [s]") + ylab("Densité de puissance [kW/m²]")
plot_a <- plot_a + scale_x_log10(breaks = c(10 ,100, 1000, 10000), limits = c(10, 10000)) + scale_y_continuous(breaks=seq(200,420,50))
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("Température maximale supérieure à 440 °C", "Température maximale supérieure à 400 °C", "Température maximale supérieure à 370 °C"),  values = linetype )
plot_a <- plot_a + scale_shape(solid = FALSE, name="Résistance au cisaillement [MPa]", label=c("[0 , 5[","[10 , 15[","[15 , 20[","[20 , 25[","[25 , 30]"))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.key.height=unit(1, "cm"))
plot_a <- plot_a + guides(col = guide_legend(nrow = 2))
plot_a

pdf("Process_window_log_fr.pdf", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("Process_window_log_fr.svg", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off()

tiff("Process_window_log_fr.tiff", width = 3543*1.5, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 14, res = 500, compression = "lzw")
plot_a
dev.off() 


plot_a <- ggplot(data = graph.data, aes(x = t, y = P ))
plot_a <- plot_a + geom_line(size=1, aes(linetype = factor(Category)))
plot_a <- plot_a + geom_point(data = graph.data2, size = 3, aes(x = t, y = P, shape=factor(floor(graph.data2$LSS / 5))))
plot_a <- plot_a + xlab("temps [s]") + ylab("Densité de puissance [kW/m²]")
plot_a <- plot_a + scale_y_continuous(breaks=seq(200,420,20))
plot_a <- plot_a + scale_linetype_manual(name="", labels = c("Température maximale supérieure à 400 °C", "Température maximale supérieure à 440 °C"),  values = linetype )
plot_a <- plot_a + scale_shape(solid = FALSE, name="Résistance au cisaillement moyenne [MPa]", label=c("[0 , 5[","[10 , 15[","[15 , 20[","[20 , 25[","[25 , 30]"))
#plot_a <- plot_a + geom_point(data = graph.data3, size = 3, color = "red", aes(x = t, y = P))
plot_a <- plot_a + elsevier_theme
plot_a <- plot_a + theme(legend.key.height=unit(1, "cm"))
plot_a <- plot_a + guides(col = guide_legend(nrow = 2))
plot_a

pdf("Process_window_fr.pdf", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

svg("Process_window_fr.svg", width = 3*width_pdf, height = 2*height_pdf2) 
plot_a
dev.off() 

tiff("Process_window_fr.tiff", width = 3543 *1.5, height = 3543 * height_pdf2/width_pdf, unit = "px", pointsize = 8, res = 500, compression = "lzw")
plot_a
dev.off() 
