#Importamos la librer�a
require(kohonen)
#Importamos los datos del archivo "community_structure.csv"
data<-read.csv("./data/community_structure.csv", header=TRUE, sep =",", dec=".")
names(data)
typeof(data)
#Definimos la paleta de colores que vamos a usar
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

#Definimos y entrenamos el mapa autorganizativo
df2=data.matrix(data)
grid.df2=somgrid(5,5,"hexagonal")
som.df2<-supersom(df2, grid = grid.df2)
#Visualizamos el mapa obtenido
plot(som.df2, type = "changes", main = "Convergencia")
plot(som.df2, type="dist.neighbours")
plot(som.df2, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
plot(som.df2, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#Estudiamos el n�mero �ptimo de clusters
mydata <- getCodes(som.df2)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
#Seg�n la gr�fica, parece que el n�mero �ptimo de clusters
#en los que se agrupan los datos est� entre 2 y 3 grupos.
#Realizamos el clustering jer�rquico para poder estudiar la segmentaci�n
som_cluster<-cutree(hclust(dist(getCodes(som.df2))),2)
som_cluster
#Visualizamos los clusters
plot(som.df2, type="counts", bgcol = som_cluster, main = "Clusters", col=5)
add.cluster.boundaries(som.df2, som_cluster)
plot(som.df2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som.df2, som_cluster)
# Seg�n el SOM hay dos grupos de datos claramente diferenciados
#por lo que parece que las bacterias del problema en cuesti�n
#se segmentan en 2 comunidades.
