setwd("C:/Users/sy/Documents/bda")

## 1
rds <- readRDS("products.rds")

library("arules")
rds.trans <- as(rds, "transactions")

## 2
rds.trans
inspect(head(rds.trans, 3))

## 3
rules <- apriori(rds.trans, parameter = list(supp = 0.002,
                                             conf = 0.25,
                                             maxlen = 2,
                                             target = "rules"))

## 4
is.redundant(rules)
inspect(rules[is.redundant(rules)])
inspect(rules[!is.redundant(rules)])

## 5
inspect(head(sort(rules, by="lift"), 10))

## 6 
rules2 <- apriori(rds.trans, parameter = list(supp = 0.001,
                                             conf = 0.1,
                                             maxlen = 2,
                                             target = "rules"))
Banana_rules <- subset(rules2, subset=lhs %in% 'Banana')
inspect(sort(Banana_rules[!is.redundant(Banana_rules)],
             by="confidence"))
