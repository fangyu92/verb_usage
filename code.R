# Section 3.1 
# to visualize Zipf's law in the two constructions
library(ggplot2)
library(dplyr)
library(xlsx)
library(ggrepel)

# the double object construction
dodata <- read.xlsx("construction_verb.xlsx",sheetName = "do",encoding = "UTF-8")
word_count <- dodata# Data frame containing words and their frequency 
colnames(word_count) <- c("label","word", "count")
alpha <- 1
# generate the theoretical data
word_count <- word_count %>%
  mutate(word = factor(word, levels = word),
         rank = row_number(),
         zipfs_freq = ifelse(rank == 1, count, dplyr::first(count) / rank^alpha))
# create the plot
zipfs_plot <- ggplot(word_count, aes(x = rank, y = count)) + 
  geom_line(aes(color = "observed")) +
  geom_point(aes(color = "observed"))+
  theme_bw() +
  geom_line(aes(y = zipfs_freq, color = "theoretical")) +
  geom_point(aes(y = zipfs_freq, color = "theoretical")) +
  labs(x = "rank", y = "frequency", title = "verb frequency distribution of double object construction") +
  geom_text_repel(aes(label=label),size=4,color = "red4") +
  scale_colour_manual(name = "Frequency count", values=c("theoretical" = "gray", "observed" = "blue2")) +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5,size = 16))

# the prepositional dative construction
prepdata <- read.xlsx("construction_verb.xlsx",sheetName = "prep",encoding = "UTF-8")
word_count <- prepdata# Data frame containing words and their frequency 
colnames(word_count) <- c("label","word", "count")
alpha <- 1
# generate the theoretical data
word_count <- word_count %>%
  mutate(word = factor(word, levels = word),
         rank = row_number(),
         zipfs_freq = ifelse(rank == 1, count, dplyr::first(count) / rank^alpha))
# create the plot
zipfs_plot <- ggplot(word_count, aes(x = rank, y = count)) + 
  geom_line(aes(color = "observed")) +
  geom_point(aes(color = "observed"),size=1)+
  theme_bw() +
  geom_line(aes(y = zipfs_freq, color = "theoretical")) +
  geom_point(aes(y = zipfs_freq, color = "theoretical"),size=0.5) +
  labs(x = "rank", y = "frequency", title = "verb frequency distribution of prepositional dative construction") +
  geom_text_repel(aes(label=label),size=4,color = "red4") +
  scale_colour_manual(name = "Frequency count", values=c("theoretical" = "gray", "observed" = "blue2")) +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5,size = 16))

# Section 3.2
# to estimate association strength using Coll.analysis 4.0
# The instructions was provided in https://www.stgries.info/teaching/groningen/readme.txt
# the analysis we want to perform is 1, the construction is double object and prepositional dative respectively,
# the corpus size is 7186865 (the tokens of verbs in our corpus)
# the data for the two constructions are <DO_gei_coll.csv> and <prep_gei_coll.csv>

library(ggcorrplot)
library(corrplot)
# to estimate the correlation between the three measures of association strength
# the correlation coefficients in the double object construction
corr <- read.xlsx("associationstrength_cor.xlsx",sheetName = "DO",encoding = "UTF-8")
corr <- corr[3:5]
corrplot(corr = cor(corr,method = "spearman"),p.mat = cor_pmat(corr,method = "spearman"),method = "circle",
         type="upper",
         tl.pos="lt", tl.cex=0.8, tl.col="red",tl.srt = 45,tl.offset=0.5,
         insig="label_sig",sig.level = c(.001, .01, .05),
         pch.cex = 0.7,pch.col = "red") # tl.*参数依次设置变量标签的位置为左侧和右侧，字符大小为1，颜色为黑色，倾斜45度，距离热图边界0.5。pch*参数依次设置显著性标签大小为0.8，颜色为红色。
corrplot(corr = cor(corr,method = "spearman"),method = "number",
         type="lower",add=TRUE,
         tl.pos = "n",cl.pos = "n",diag=FALSE,
         number.digits = 3,number.cex = 1,number.font = NULL,col = "black"
)

# the correlation coefficients in the prepositional construction
corr <- read.xlsx("associationstrength_cor.xlsx",sheetName = "PREP",encoding = "UTF-8")
corr <- corr[3:5]
corrplot(corr = cor(corr,method = "spearman"),p.mat = cor_pmat(corr,method = "spearman"),method = "circle",
         type="upper",
         tl.pos="lt", tl.cex=0.8, tl.col="red",tl.srt = 45,tl.offset=0.5,
         insig="label_sig",sig.level = c(.001, .01, .05),
         pch.cex = 0.7,pch.col = "red") # tl.*参数依次设置变量标签的位置为左侧和右侧，字符大小为1，颜色为黑色，倾斜45度，距离热图边界0.5。pch*参数依次设置显著性标签大小为0.8，颜色为红色。
corrplot(corr = cor(corr,method = "spearman"),method = "number",
         type="lower",add=TRUE,
         tl.pos = "n",cl.pos = "n",diag=FALSE,
         number.digits = 3,number.cex = 1,number.font = NULL,col = "black"
) 

# semantic similarity to the best exemplar
# semantic similarity to the best exemplar in the double object construction
dodata <- read.xlsx("semantic_similarity.xlsx",sheetName = "DO",encoding = "UTF-8")
dodata <- dodata[2:86,]
cor.test(dodata$frequency,dodata$synonyms,method = "spearman")
addline_format <- function(x){
  gsub('12','\n',x)
}
do_plot <- ggplot(dodata, mapping = aes(x = synonyms, y = frequency)) + 
  geom_point(alpha=0.6,size=1.2,color="cyan3") +
  theme_bw() +
  geom_smooth(se = T,method = "lm",size = 0.5,color="deepskyblue3") +
  labs(x = addline_format("semantic similarity by Synonyms 12(spearman's rho=0.494, p<0.001)"), y = "verb frequency", title = "(b) The 85 verbs in the double object structure") +
  theme(plot.title = element_text(size = 11),plot.subtitle = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

# semantic similarity to the best exemplar in the prepositional dative construction
prepdata <- read.xlsx("semantic_similarity.xlsx",sheetName = "PREP",encoding = "UTF-8")
prepdata <- prepdata[2:407,]
cor.test(prepdata$frequency,prepdata$synonyms,method = "spearman")
prep_plot <- ggplot(prepdata, mapping = aes(x = synonyms, y = frequency)) + 
  geom_point(alpha=0.6,size=1.2,color="cyan3") +
  theme_bw() +
  geom_smooth(se = T,method = "lm",size = 0.5,color="deepskyblue3") +
  labs(x = addline_format("semantic similarity by Synonyms 12(spearman's rho=0.174, p<0.001)"), y = "verb frequency", title = "(d) The 406 verbs in the prepositional dative structure") +
  theme(plot.title = element_text(size = 11),plot.subtitle = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

# the linear regression model for the double object construction
linear_regression_do <- read.xlsx("linear_regression.xlsx",sheetName = "DO")
linear_regression_do <- within(linear_regression_do,{
  betweenesscentralitysyn <- (betweenesscentralitysyn-mean(betweenesscentralitysyn))/sd(betweenesscentralitysyn)
  frequency <- (frequency-mean(frequency))/sd(frequency)
  FYE <- (FYE-mean(FYE))/sd(FYE)
  synonyms <- (synonyms-mean(synonyms))/sd(synonyms)
})
model_do <- lm(frequency~FYE+betweenesscentralitysyn+synonyms,data = linear_regression_do)
summary(model_do)
library(car)
vif(model_do)
# the linear regression model for the prepositional dative construction
linear_regression_prep <- read.xlsx("linear_regression.xlsx",sheetName = "PREP")
linear_regression_prep <- within(linear_regression_prep,{
  betweenesscentralitysyn <- (betweenesscentralitysyn-mean(betweenesscentralitysyn))/sd(betweenesscentralitysyn)
  frequency <- (frequency-mean(frequency))/sd(frequency)
  FYE <- (FYE-mean(FYE))/sd(FYE)
  synonyms <- (synonyms-mean(synonyms))/sd(synonyms)
})
model_prep <- lm(frequency~FYE+betweenesscentralitysyn+synonyms,data = linear_regression_prep)
summary(model_prep)
vif(model_prep)


# Section 3.3
# plot correlation matrix
corr<-read.xlsx("dative_alternation.xlsx",sheetName = "correlation")
corrplot.mixed(cor(corr),order="hclust", tl.pos = "lt",diag = "u",lower.col = "black",upper = "ellipse",tl.cex=1.2,cl.cex=1.2,number.cex=1.2)
# to build the two mixed effects logistic regression models
library(lme4)
df<-read.xlsx("dative_alternation.xlsx",sheetName = "model")
df<-within(df,{
  CONSTRUCTION<-factor(CONSTRUCTION)
  recipientani<-factor(recipientani)
  recipientdef<-factor(recipientdef)
  recipientgive<-factor(recipientgive)
  recipientpro<-factor(recipientpro)
  themeani<-factor(themeani)
  themedef<-factor(themedef)
  themegive<-factor(themegive)
  themepro<-factor(themepro)
  verb<-factor(verb)
  syndai<-(syndai-mean(syndai))/sd(syndai)
  syngei<-(syngei-mean(syngei))/sd(syngei)
  hownetdai <- (hownetdai-mean(hownetdai))/sd(hownetdai)
  hownetgei <- (hownetgei-mean(hownetgei))/sd(hownetgei)
})

# the first model
mod_fit_1<-glmer(CONSTRUCTION~syngei+syndai+(1|verb), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mod_fit_1)
vif(mod_fit_1)

# the second model
mod_fit_2<-glmer(CONSTRUCTION~recipientani+recipientdef+themeani+themedef+themegive+themepro+syngei+syndai+(1|verb), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# drop fixed effects in the second model according to likelihood ratio test
Anova(mod_fit_2,type = "III",test.statistic = "Chisq")

# the final model
mod_fit_3<-glmer(CONSTRUCTION~recipientani+recipientdef+themeani+themegive+syngei+syndai+(1|verb), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mod_fit_3)
vif(mod_fit_3)

# the intercept only model
mod_fit_4<-glmer(CONSTRUCTION~1+(1|verb), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# compare the final model and the intercept only model
anova(mod_fit_3,mod_fit_4,method="LR")

# without random effect
mod_fit_5<-glm(CONSTRUCTION~recipientani+recipientdef+themeani+themegive, data=df, family = "binomial")

# compare the final model with and without random effect
anova(mod_fit_3,mod_fit_5,method="LR")

#compute R-square of the first model and the final model
library(MuMIn)
r.squaredGLMM(mod_fit_1)
r.squaredGLMM(mod_fit_3)

# classification accuracy of the final model
predictions.num <- fitted(mod_fit_3)
predictions.cat <- ifelse(predictions.num>=0.5, "PREP", "DO")
predict_result <- table(df$CONSTRUCTION, predictions.cat)
(predict_result[1,1]+predict_result[2,2])/length(df$CONSTRUCTION)

# binomial test
binom.test((predict_result[1,1]+predict_result[2,2]),length(df$CONSTRUCTION),p = 4645/length(df$CONSTRUCTION))

# the importance of each variable in predicting syntactic choice
Anova(mod_fit_3,type = "III")
library(forcats)
IMP<-data.frame('name'=c('RECDEF','RECANI','THEMEGIVE','SYNDAI','THEMEANI','SYNGEI'),'value'=c(142.562,50.386,31.139,20.518,16.320,6.309)) # increase in deviance if factor removed
IMP %>%
  mutate(name = fct_reorder(name, value)) %>%
  ggplot(aes(x=name, y=value,fill=name)) +
  geom_bar(stat="identity", alpha=.6, width=.6) +
  coord_flip()+
  ylab('Chisq')+
  xlab('')+
  theme_bw()+
  scale_fill_brewer(palette = 'GnBu')+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),plot.title = element_text(size=13))+
  labs(title = "Variable importance in the second model")