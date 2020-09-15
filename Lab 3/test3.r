library(ggplot2)
library(ggpubr)
library(arulesViz)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
data <- read.csv(url, header = FALSE, fill = TRUE,na.strings = c("?"))
colnames(data) <- c("classname","date","plant-stand","precip","temp","hail","crop-hist","area-damaged","severity","seed-tmt","germination","plant-growth","leaves","leafspots-halo","leafspots-marg","leafspot-size","leaf-shread","leaf-malf","leaf-mild","stem","lodging","stem-cankers","canker-lesion","fruiting-bodies","external decay","mycelium","int-discolor","sclerotia","fruit-pods","fruit spots","seed","mold-growth","seed-discolor","seed-size","shriveling","roots")
df <- data.frame(data)

soporte <- 0.2
confianza <- 0.5

reglas <- apriori(df,
                  parameter = list(support = soporte, confidence = confianza, minlen = 2, maxlen = 3, target = "rules"),
                  )
allRules <- inspect(sort(x=reglas, decreasing = TRUE, by = "confidence"))