library(reshape2)
library(corrplot)

# correlation matrix for attributes
sj_cor_matrix <- round(cor(sj_train[5:25]), 2)

iq_cor_matrix <- round(cor(iq_train[5:25]), 2)

# plot the correlations
corrplot(sj_cor_matrix, type = 'full', tl.col = 'black',
         method="shade")

corrplot(iq_cor_matrix, type = 'full', tl.col = 'black',
         method="shade")

#get features 
sj_cor_atr <-as.data.frame(t(sj_cor_matrix[21, 1:20]))
iq_cor_atr <- as.data.frame(t(iq_cor_matrix[21, 1:20]))

sj_cor_atr <- sort(sj_cor_atr)
iq_cor_atr <- sort(iq_cor_atr)

imp_atr <- cbind(t(sj_cor_atr), t(iq_cor_atr))
names(imp_atr) <- c('sj_cor_atr', 'iq_cor_atr')

mean(as.numeric(sj_cor_atr))
mean(as.numeric(iq_cor_atr))
which(iq_cor_atr > 0.1)
names(sj_cor_atr[ 16:20])

sj_train_imp <- sj_train[names(sj_cor_atr[ 16:20])]
sj_train_imp$total_cases <- sj_train$total_cases

iq_train_imp <- iq_train[names(sj_cor_atr[ 16:20])]
iq_train_imp$total_cases <- iq_train$total_cases



