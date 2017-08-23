library(data.table)
library(ggplot2)
library(scales)
library(dplyr)

winning <- fread('winning.csv')
winning[,dd_asked := factor(dd_asked)]
winning[,dd_correct := factor(dd_correct)]
winning[,ca_centered := clues_picked - median(clues_picked)]
winning[,subpop := factor(paste0(dd_asked,dd_correct))]
ask_mod <- glm(winner ~ subpop*(ca_centered), data = winning[dd_asked %in% c(0,1)])

center <- median(winning$clues_picked)
top <- max(winning$correct_answers)
plot_df <- data.table(ca_centered = rep(seq(top) - center,3),
                      subpop = factor(rep(c('00','10','11'),each = 45)))
preds <- predict(ask_mod,plot_df, type = 'link', se.fit = T)
plot_df[,prob := 1/(1 + exp(-preds$fit))]
plot_df[,lower := 1/(1 + exp(-(preds$fit - 1.96*preds$se.fit)))]
plot_df[,upper := 1/(1 + exp(-(preds$fit + 1.96*preds$se.fit)))]

labeller <- c("00" = 'No Daily Doubles', "10" = "One Daily Double, Answered Incorrectly",
              "11" = "One Daily Double, Answered Correctly")
plot_df[,type := labeller[subpop]]

ggplot(plot_df,aes(x = ca_centered + 19, y = prob, color = type)) + 
        geom_line() +
        scale_y_continuous(labels = percent) + 
        labs(title = 'Probability of Winning',
             subtitle = 'Conditioning on # Clues Picked and # Daily Doubles Answered',
             x = '# Clues Picked',
             y = 'Probability of winning',
             color = '') +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              legend.text = element_text(size = 7))


ggplot(winning,aes(correct_answers)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent) + 
        labs(title = 'Distribution of Correct Answers in Jeopardy',
             x = 'Correct Answers',
             y = '% of Contentasts')  +
        theme(plot.title = element_text(hjust = 0.5))


labeller2 <- c("0" = 'No Daily Doubles Found', "1" = "One Daily Double Found",
               "2" = "Two Daily Doubles Found", "3" = "Three Daily Doubles Found")
winning[,distri := factor(labeller2[dd_asked],levels = labeller2)]

ggplot(winning,aes(clues_picked)) + 
        facet_wrap(~distri, nrow = 4) + 
        geom_bar() +
        scale_y_continuous() + 
        labs(title = 'Distribution of Number of Clues Picked',
             subtitle = 'Broken out by # Daily Doubles Found',
             x = 'Clues Picked',
             y = 'No. of Contentasts')  +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))

