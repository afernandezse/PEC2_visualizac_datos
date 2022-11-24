#### Plot 1 PEC2 (circle packing)


data1 <- read.csv("/home/flatline/Documentos/Master_Data_Science/aaVisualizacion_datos/PEC2/Vacunaci__per_al_COVID-19__dosis_administrades_per_municipi.csv")
# data1 <- data1[sample(nrow(data1), 50000),]

data1 <- aggregate(RECOMPTE ~  PROVINCIA + COMARCA + MUNICIPI, data = as.data.frame(data1), FUN = sum)

# data1 <- data1[sample(nrow(data1), 50),]


install.packages("data.tree")
install_github("jeromefroe/circlepackeR")
library(circlepackeR)

library(data.tree)
data1$pathString <- paste("Catalunya",data1$PROVINCIA, data1$COMARCA, data1$MUNICIPI, sep = "/")
population <- as.Node(data1)



# Make the plot
p <- circlepackeR(population, size = "RECOMPTE", color_min = "hsl(56,80%,80%)", 
                  color_max = "hsl(341,30%,40%)")

p

# https://rpubs.com/anferse/969104


## Plot 2: Voronoy Diagram

metroDf <- read.csv("/home/flatline/Documentos/Master_Data_Science/aaVisualizacion_datos/PEC2/MetroBarcelona.csv")

library(ggvoronoi)

data3 <- read.csv("/home/flatline/Documentos/Master_Data_Science/aaVisualizacion_datos/PEC2/Directori_de_centres_docents_TRA_ACOVID.csv")

x <- data3$Coordenades.GEO.X
y <- data3$Coordenades.GEO.Y

points <- data.frame(x, y,
                     distance = sqrt((x-100)^2 + (y-100)^2))
points <- unique(points)

ggplot(points) +
  geom_voronoi(aes(x,y,fill=distance))

ggplot(points,aes(x,y)) +
  stat_voronoi(geom="path") +
  # borders("world", "spain") +
  geom_point()+
  theme_minimal()


library(maps)
library(mapdata)
library(maptools)
library(spatstat)
library(spatstat.utils)
library(rgeos)
library(sp)
library(leaflet)


mapAreas <- c("Spain")

counties <- map("worldHires",
                "Spain",
                exact = TRUE,
                fill=TRUE,
                plot=FALSE)

countries <- gUnaryUnion(map2SpatialPolygons(counties,
                                             IDs = counties$names,
                                             proj4string = CRS("+proj=longlat +datum=WGS84")))
W <- as(countries, "owin")



X <- ppp(x = data3$Coordenades.UTM.X,
         y = data3$Coordenades.UTM.Y,
         window = W)

data3<-data3[data3$Coordenades.UTM.X %in% X$x,]
data3 <- data3[!(duplicated(data3$Coordenades.UTM.X,)),]


y <- dirichlet(X)


names(y$tiles) <- make.unique(data3$Denominació.completa, sep = ".")

owin2Polygons <- function(x, id="1") {
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(data3) { data3[c(seq(nrow(data3)), 1),] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords = closering(cbind(p$x, p$y)),
                             hole = is.hole.xypolygon(p))
                   })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nom <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nom[i])
  return(SpatialPolygons(z))
}

tessSP <- tess2SP(y)

proj4string(tessSP) <- CRS("+proj=utm +zone=31 ellps=WGS84")

tessSP <- spTransform(tessSP,
                      CRS("+proj=longlat +datum=WGS84"))


leaflet(width = "50%") %>%
  setView(lng = mean(metroDf$Longitude),
          lat = mean(metroDf$Latitude),
          zoom = 11) %>%
  # Base map
  addProviderTiles("Hydda.Full") %>%
  # Voronoi polygons
  addPolygons(data = tessSP,
              stroke = TRUE,
              color = "black", weight = 0.5,
              fill=TRUE, fillOpacity = 0,
              label = names(y$tiles),
              popup = names(y$tiles))


## Plot 3: Convex Hull Iris dataset

library(tidyverse)

# Define the scatterplot
plot <- ggplot(iris, aes(Sepal.Length,
                         Sepal.Width, color=Species))+
  geom_point(size=3) +
  scale_color_manual(values=c("setosa"="#388d5d","versicolor"= "#d6a34a","virginica" ="#5a431b")) +
  #scale_fill_manual(values=c("#f2bc94", "#00154f", "#f4af1b"))+
  #scale_color_brewer(values=c("#f2bc94", "#00154f", "#f4af1b"))+
  theme_test()

# display plot
plot

hull <- iris %>% group_by(Species) %>%
  slice(chull(Sepal.Length, Sepal.Width))

plot + geom_polygon(data = hull, alpha = 0.2,
                    aes(colour = Species, fill = Species))+
  scale_color_manual(values=c("setosa"="#388d5d","versicolor"= "#d6a34a","virginica" ="#5a431b"))+
  scale_fill_manual(values=c("setosa"="#388d5d","versicolor"= "#d6a34a","virginica" ="#5a431b"))

