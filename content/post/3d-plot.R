library(plotly)
library(readxl)

original_data = read_xlsx("2019-data/ExperimentsFlowBenchmark.xlsx")

original_data = original_data[,c("WIP","Variability", "Throughput")]

plot_data = as.matrix(original_data)  

names = colnames(plot_data)

# Produce a Smooth Surface:


surface = akima::interp(plot_data[,1],plot_data[,2],plot_data[,3])

names(surface) = names

x = surface$WIP
y = surface$Variability
z = surface$Throughput

aX <- list(title = "WIP")
aY <- list(title = "Variability")

# Weird but you need to use t(z) here:
# z <- t(z)

plot_ly(x=x,y=y,z=z,type = "surface",colors=colors) %>% layout(scene = list(xaxis = aX, yaxis = aY, dragmode="turntable"))

colors <- colorRampPalette(c("white","royalblue","seagreen","orange","red","brown"))(500)


plot_ly(x = surface[[1]], y = surface[[2]], z = surface[[3]]) %>% add_surface() %>% layout(xaxis = x, yaxis = y)


plot_ly(x = plot_data[,1], y = plot_data[,2], z = plot_data[,3]) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
) %>%
  layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )

# Plotando a População Final
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
  )
x <- list(
    title = "Variability",
    titlefont = f
  )
y <- list(
    title = "WIP",
    titlefont = f
  )
z <- list(
    title = "Throughput",
    titlefont = f
  )
  

dataf <- data.frame(x=as.numeric(1:50), y=as.numeric(1:50), z=as.numeric(1:5))

plot_ly(z = as.matrix(plot_data), type = "surface")
