library(neuralnet)
library(nnet)
library(devtools)


input <- data.frame(table(data1970.2009$country_txt, data1970.2009$attacktype1_txt))

input <- reshape(input, timevar = c("Var2"), idvar = c("Var1"), direction = "wide")

res <- data.frame(table(data$country_txt, data$nkill > 25))
res$is_true <- ifelse(test = res$Var2 == TRUE & res$Freq > 0, 1,0)
res <- res[210:nrow(res),]

input$result <- res$is_true

setnames(input, colnames(input), c("country", "x1", "x2", "x3", "x4","x5","x6","x7","x8","x9","result"))


form.in<-as.formula('result~x1+x2+x3+x4+x5+x6+x7+x8+x9')

mod2<-neuralnet(form.in,data=input,hidden=10, linear.output = FALSE)

plot(mod2)
