

# Using the queueing library, please refer to the library help for info:
# https://journal.r-project.org/archive/2017/RJ-2017-051/index.html

library(queueing)
library(plyr)

simple_queueing_model = function(wip, rb = 0.5) {
  
  # WE're giving more demand than the system can take:
  lambda = rb * 3
  
  model_input = NewInput.MM1K(lambda = lambda, mu = rb, k = wip)
  
  CheckInput(queueing_model)
  
  model = QueueingModel(model_input)
  
  c(WIP = wip, Throughput = model$Throughput, CycleTime = model$W)
  
}

wip = unique(data_benchmark$WIP)

queueuing_model_results = plyr::ldply(.data = wip,
                                      .fun = simple_queueing_model, rb = 0.5) 






library(queueing)
library(plyr)

simple_queueing_model = function(wip, rb = 0.5) {
    
    # WE're giving more demand than the system can take:
    lambda = rb * 3
    
    model_input = NewInput.MM1K(lambda = lambda, mu = rb, k = wip)
    CheckInput(queueing_model)
    model = QueueingModel(model_input)
    
    c(WIP = wip, Trhoughput = model$Throughput, CycleTime = model$W)
    
  }

wip = 1:20
queueuing_model_results = plyr::ldply(.data = wip, .fun = simple_queueing_model, rb = 0.5) 


data = c(0,1,0,0,)

NewInput.CJN()

data <- c(0, 0.3, 0.2, 0, 0, 0, 0, 0, 0.7, 0.1, 0, 0, 0, 0.8, 0.15, 0, 0.4, 0.3, 0, 0.3)
prob <- matrix(data=data, byrow = TRUE, nrow = 5, ncol=5)
gd <- NewInput.MM1(lambda=10, mu=25, n=0)
ort <- NewInput.MM1(lambda=0, mu=18, n=0)
car <- NewInput.MM1(lambda=0, mu=20, n=0)
tb <- NewInput.MMC(lambda=0, mu=12, c=15, n=0)
hos <- NewInput.MMInf(lambda=0, mu=0.012, n=0)
hospital <- NewInput.OJN(prob=prob, gd, ort, car, tb, hos)
model = QueueingModel(hospital)

lambda = 3
wip = 1

transition_data <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1)
prob <- matrix(data=transition_data, byrow = TRUE, nrow = 4, ncol=4)
op1 = NewInput.MM1(lambda=lambda, mu=rb, n=0)
op2 <- NewInput.MM1(lambda=0, mu=rb, n=0)
op3 <- NewInput.MM1(lambda=0, mu=rb, n=0)
op4 <- NewInput.MM1(lambda=0, mu=rb, n=0)

company = NewInput.CJN(prob = prob, n = 10, z = 0, operational = F, method = 1, tol = 0.001, op1, op2, op3, op4)


model = QueueingModel(company)

company = NewInput.OJN(prob=prob, n = wip,z = 0, operational = F,method = 1,)


hospital <- NewInput.OJN(prob=prob, gd, ort, car, tb, hos)


rb

n <- 2
n1 <- NewInput.MM1(lambda=1, mu=1/0.2, n=0)
n2 <- NewInput.MM1(lambda=0, mu=1/0.4, n=0)
n3 <- NewInput.MM1(lambda=0, mu=1/0.2, n=0)
n4 <- NewInput.MM1(lambda=0, mu=1/0.4, n=0)

## think time = 0
z <- 0

## operational value
operational <- FALSE

## definition of the transition probabilities
prob <- matrix(data=c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1), nrow=4, ncol=4, byrow=TRUE)

model <- NewInput.CJN(prob, n, z, operational, 0, 0.001, n1, n2, n3, n4)

QueueingModel(model)















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
