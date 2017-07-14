---
title: "CHIS exercise"
author: "Adanna  Alutu"
date: "July 5, 2017"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}

library(ggplot2) 
3 library(reshape2) 
4 library(dplyr) 
5 library(ggthemes) 
6 
 
7 # Script generalized into a function 
8 mosaicGG <- function(data, X, FILL) { 
9      
10     # Proportions in raw data 
11     DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]])) 
12     DF$groupSum <- rowSums(DF) 
13     DF$xmax <- cumsum(DF$groupSum) 
14     DF$xmin <- DF$xmax - DF$groupSum 
15     DF$X <- row.names(DF) 
16     DF$groupSum <- NULL 
17     DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL") 
18     library(dplyr) 
19     DF_melted <- DF_melted %>%  
20         group_by(X) %>%  
21         mutate(ymax = cumsum(value/sum(value)), 
22                ymin = ymax - value/sum(value)) 
23      
    # Chi-sq test 
results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x 
26     resid <- melt(results$residuals) 
27     names(resid) <- c("FILL", "X", "residual") 
28      
29     # Merge data 
30     DF_all <- merge(DF_melted, resid) 
31      
32     # Positions for labels 
33     DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2 
34     index <- DF_all$xmax == max(DF_all$xmax) 
35     DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2 
36      
37     # plot: 
38     g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,  
39                             xmax = xmax, fill = residual)) +  
40         geom_rect(col = "white") + 
41         geom_text(aes(x = xtext, label = X), 
42                   y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) + 
43         geom_text(aes(x = max(xmax),  y = ytext, label = FILL), 
44                   size = 3, hjust = 1, show.legend = FALSE) + 
45         scale_fill_gradient2("Residuals") + 
46         scale_x_continuous("Individuals", expand = c(0,0)) + 
47         scale_y_continuous("Proportion", expand = c(0,0)) + 
48         theme_tufte() + 
49         theme(legend.position = "bottom") 
50     print(g) 
51 } 
52 
 
53 # BMI described by age 
54 mosaicGG(adult, "SRAGE_P", "RBMI") 
55 
 
56 # Poverty described by age 
57 mosaicGG(adult, "SRAGE_P", "POVLL") 

 
59 # mtcars: am described by cyl 
60 mosaicGG(mtcars, "cyl", "am") 
61 
 
62 # Vocab: vocabulary described by education 
63 library(car) 
64 mosaicGG(Vocab, "education", "vocabulary") 


```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
