library(ggplot2)

top10costs <- storms[with(storms, order(-COSTS)), ][1:10,c(1,3)]
to10causalities <- storms[with(storms, order(-CAUSALITIES)), ][1:10,c(1,2)]

#If in RStudio, copy this function call to the terminal
ggplot(data = top10costs, aes(x = EVTYPE, y = COSTS)) +
	geom_bar(stat = "identity",colour = "black", fill = "lightblue") +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "black"))+
	labs(y = "Avg cost in dollars", x = "Events")

ggplot(data = to10causalities, aes(x = EVTYPE, y = CAUSALITIES)) +
	geom_bar(stat = "identity",colour = "black", fill = "lightblue") +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "black"))+
	labs(y = "Avg number of causalities", x = "Events")