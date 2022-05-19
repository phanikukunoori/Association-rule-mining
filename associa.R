transactions <-read.csv("C:/Users/Phani/Documents/Transactions.csv",header=T, colClasses="factor") 
setwd("C:/Users/Phani/Documents") 
names(transactions) 
head(transactions) 
tail(transactions)
summary(transactions) 
str(transactions) 
dim(transactions) 
#colSums() function computes the sums of columns. 
YES <- colSums(transactions == "YES") 
YES
NO <-colSums(transactions=="NO") 
NO
visited <- rbind(YES,NO)
visited
barplot(visited,legend=rownames(visited)) #Plot 1
barplot(visited, beside=T,legend=rownames(visited))# Plot 2
#arules package is a powerful tool for mining associative rules in transactional databases. The most 
install.packages("arules") # install "arules" package. 
library(arules) # activate "arules" package 
#The apriori () function from the arule package implements the 
#NOte that, by default, the apriori () function executes all the 
#Usage of apriori () function
rules <- apriori(transactions, maxlen = 5)
summary(rules)
#The result tells you that there was 2 rules with 3 item and 255 rules 
inspect(rules, maxlen=20) 
#inspect function prints the internal representation of an R object 
# When the max len parameter is NOt set, the algorithm continues 
#set the minlen=2,maxlen=3 and confident = 0.95
rules <- apriori(transactions, parameter =list(minlen=2,maxlen=3, conf = 0.95))
summary(rules) 
inspect(rules) 
#Plotting could be the easiest way to find the most visited 
rules <- apriori(transactions, parameter = list(minlen=2, maxlen=3,conf = 0.95), appearance= list(rhs=c("WINE=YES"),default="lhs")) 
barplot(visited, beside=T,legend=rownames(rules)) 
#random variable selection
rules <- apriori(transactions, 
                 parameter = list(minlen=2, maxlen=3,conf = 0.70), 
                 appearance= list(rhs=c("WINE=YES"),default="lhs")) 
summary(rules) 
inspect(rules)
install.packages("arulesViz") # install "arulesViz" 
library(arulesViz) # activate "arules" package 
plot(rules)
plot(rules, jitter=0)
plot(rules, method="grouped") 
plot(rules@quality) 

rules3 <- apriori(transactions, parameter = list(minlen=2,maxlen=3, conf = 0.80), appearance =list(rhs=c("TOBACCO=YES","BEER=YES"),default="lhs"))
summary(rules3)
inspect(rules3)
barplot(visited, beside=T,legend=rownames(visited)) 

library(plotly)
install.packages("arulesViz") 
library(arulesViz)
plot(rules3, engine = "plotly")
#removing jitter
plot(rules3, engine = "plotly", jitter = 0)
plot(rules3, measure = c("support", "lift"), shading = "confidence", engine = "plotly")



rules2 <- apriori(transactions, parameter = list(minlen=2, maxlen=5,conf = 0.6),
                  appearance =list(rhs=c("BABY.CARE=YES"),lhs=c("BATTERIES=YES", "BEDDING=YES","GIFTS=YES", "CANDY=YES","BEVERAGES=YES",
                                                                "HOUSEHOLD.CLEANING=YES","AMERICAN.GREETINGS=YES","SUNGLASSES=YES"),default="none"))
inspect(rules2) 
rules_ex <-apriori(transactions, 
                   parameter =list(minlen=2,maxlen=4,conf=0.75)) 
#Explore association rules using interactive manipulations and 
ruleExplorer(rules_ex)

library(shinydashboard)
library(shinyapp)

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

