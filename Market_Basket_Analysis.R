library(Matrix)
library(arules)
library(grid)
library(arulesViz)
library(ggplot2)

raw_data <- read.transactions('ElectronidexTransactions2017.csv', rm.duplicates = TRUE, sep = ',')

#EXPLORE
'inspect(raw_data)
max(size(raw_data))  #max size = 30
min(size(raw_data))  #min size = 0
LIST(raw_data)
mean(size(raw_data))  #mean itemset size = 4.38'
item_names <- itemLabels(raw_data)

itemFrequencyPlot(raw_data, topN = 25, horiz = TRUE, type ='absolute', xlab = 'sales')

#PREP GGPLOT
vector_raw_data <- (itemFrequency(raw_data, type = 'absolute')) #named numeric vector
df_raw_data <- data.frame(vector_raw_data, names(vector_raw_data))  #put into data frame
colnames(df_raw_data) <- c("Sales","Item")  #set column names
matrix_raw_data <- as.matrix(df_raw_data)
item_freq <- (head((df_raw_data[order(-df_raw_data$Sales),]),25)) 
item_freq$Item <- factor(item_freq$Item, levels = item_freq$Item)

#GGPLOT TOP 25 SALES V ITEM
ggplot(item_freq, aes(x = Item, y = Sales)) + 
  labs(title="Electronidex's Top Selling Items") + 
  geom_bar(width=.75, fill="tomato3", stat = "identity") +
  theme(axis.text.x = element_text(angle=65, vjust = 1)) +
  coord_flip()

#PLOT TRANSACTIONS V ITEMS
image(sample(raw_data,100))

#APPLY APRIORI
highconf_rules <- apriori(raw_data, 
                          parameter = list(supp = 0.0011, confidence = 0.9, minlen = 2))
summary(highconf_rules)

#SORT RULES BY PARAMETER
rules_sorted_lift <- sort(highconf_rules, by = "lift", order = FALSE)
inspect(rules_sorted_lift[0:10],itemSep = "+", ruleSep = ">>>", setStart = "", setEnd = "")
  #return 10 rules sorted by highest support
inspect(sort(highconf_rules, by = "support", order = FALSE)[0:10], 
        itemSep = "+", ruleSep = ">>>", setStart = "", setEnd = "")
  #return 10 rules sorted by highest confidence
inspect(sort(highconf_rules, by = "confidence", order = FALSE)[0:10], 
        itemSep = "+", ruleSep = ">>>", setStart = "", setEnd = "")

#SORT RULES BY TOP SELLING PRODUCTS
imac_rules <- subset(highconf_rules, items %in% ("iMac"))
inspect(imac_rules) #70 rules

viewsonic_rules <- subset(highconf_rules, items %in% "ViewSonic Monitor")
inspect(viewsonic_rules)  #50 rules

lenovodesktop_rules <- subset(highconf_rules, items %in% "Lenovo Desktop Computer")
inspect(lenovodesktop_rules)  #42 rules

hplaptop_rules <- subset(highconf_rules, items %in% "HP Laptop")
inspect(hplaptop_rules)  #54 rules

macbookair_rules <- subset(highconf_rules, items %in% "Apple MacBook Air")
inspect(macbookair_rules)  #3 rules

#FIND REDUNDANT RULES
is.redundant(highconf_rules)
inspect(highconf_rules[is.redundant(highconf_rules)]) #only 7

#PLOT RULES
plot(highconf_rules, measure = c('support','confidence'), jitter = 1.5, method = 'two-key plot', control=list(main='Support v Confidence for 95 Rules'))
