library(tidyverse)
library(corrplot)
library(showtext)
library(MASS)
library(Rtsne)
library(RColorBrewer)
library(ggrepel)
library(gridExtra)
library(dendextend)
library(ggdendro)
library(lme4)

showtext_auto(enable = TRUE)
font_add('KaiTi', regular='KaiTi.ttf')

fpsp <- read_csv('fpsp.csv')
with(fpsp, table(年份, 地区))
with(fpsp, table(题目, 地区))

fpspt <- fpsp %>% dplyr::select(年份:题目, 步骤1总分, 步骤2总分, 步骤3总分, 步骤4和步骤5总分, 步骤6总分, 总体回顾得分, 最终得分) %>% mutate(步骤得分 = 最终得分 - 总体回顾得分)
fpspt <- fpspt[complete.cases(fpspt),] 
fpspc <- fpspt %>% dplyr::select(步骤1总分:总体回顾得分)
fpspcor <- cor(fpspc)
row.names(fpspcor) <- c('步骤1','步骤2','步骤3','步骤4和步骤5','步骤6', '总体回顾得分')
colnames(fpspcor) <- c('步骤1','步骤2','步骤3','步骤4和步骤5','步骤6', '总体回顾得分')
write.csv(fpspcor,'fpspcor.csv')
pdf('fcor.pdf')
corrplot(fpspcor,family='KaiTi')
dev.off()

cortest2 <- cor.mtest(fpspc)
cm2p <- cortest2$p
diag(cm2p) <- 1
cm2p3s <- cm2p < 0.001  ##  3 stars
cm2p2s <- cm2p >= 0.001 & cm2p < 0.01  ##  2 stars
cm2p1s <- cm2p >= 0.01 & cm2p < 0.05  ##  1 star
ccor2p <- matrix('-', 6, 6)
ccor2p[cm2p3s] <- '***'
ccor2p[cm2p2s] <- '**'
ccor2p[cm2p1s] <- '*'
write.csv(ccor2p, 'corpval.csv')

fpsptg <- fpspt %>% gather(key = 步骤, value = 得分, 步骤1总分:最终得分)
# summarize by region and year
fpspsry <- fpsptg %>% filter(步骤 == '最终得分') %>% group_by(地区, 年份) %>% summarize(平均得分 = mean(得分))

# summarize by region, grade and year
fpspsrgy <- fpsptg %>% filter(步骤 == '最终得分') %>% group_by(地区, 年份, 年级) %>% summarize(平均得分 = mean(得分))

# summarize by region, year, grade and questions
fpspsrgyq <- fpsptg %>% filter(步骤 == '最终得分') %>% group_by(地区, 年份, 年级, 题目) %>% summarize(平均得分 = mean(得分))

## only one question
fpsprgyq1 <- fpspsrgyq %>% filter(题目 == 'qualifying problem')

## scatterplot
pdf('fscatter.pdf', width = 12, height = 8)
fpspt %>% #mutate(年级 =  factor(年级, levels = c('小学', '初中', '高中')))
  ggplot(aes(x=步骤得分, y = 总体回顾得分, col = 年级, shape = 题目))+
  theme(text = element_text(family = "KaiTi"))+
  geom_point()+
  facet_grid(年份 ~ 地区)
dev.off()

## one question
pdf('fscatter1.pdf', width = 12, height = 8)
fpspt %>% filter(题目 == 'qualifying problem') %>%#mutate(年级 =  factor(年级, levels = c('小学', '初中', '高中')))
  ggplot(aes(x=步骤得分, y = 总体回顾得分, col = 年级, shape = 年级))+
  theme(text = element_text(family = "KaiTi"))+
  geom_point()+
  facet_grid(年份 ~ 地区)
dev.off()

## only teams
pdf('fscatter2.pdf', width = 12, height = 8)
fpspt %>% filter(团队 == '团队组') %>%#mutate(年级 =  factor(年级, levels = c('小学', '初中', '高中')))
  ggplot(aes(x=步骤得分, y = 总体回顾得分, col = 年级, shape = 题目))+
  theme(text = element_text(family = "KaiTi"))+
  geom_point()+
  facet_grid(年份 ~ 地区)
dev.off()

## boxplot
pdf('fbox.pdf', width = 8, height = 12)
fpspt %>% #mutate(年级 =  factor(年级, levels = c('小学', '初中', '高中')))
  ggplot(aes(x=年级, y = 最终得分))+
  theme(text = element_text(family = "KaiTi"))+
  geom_boxplot()+
  scale_x_discrete(limits = c("小学组", "初中组", "高中组"))+
  facet_grid(地区 ~ 年份)
dev.off()

pdf('fbox1.pdf', width = 12, height = 8)
fpspt %>% #mutate(年级 =  factor(年级, levels = c('小学', '初中', '高中')))
  ggplot(aes(x=年级, y = 最终得分))+
  theme(text = element_text(family = "KaiTi"))+
  geom_boxplot()+
  scale_x_discrete(limits = c("小学组", "初中组", "高中组"))+
  facet_grid(年份 ~ 地区)
dev.off()

## mixed effects model on teams data
fpspr <- fpspt %>% filter(团队 == '团队组') %>% mutate(年级 =  factor(年级), 年份 =  factor(年份), 地区 = factor(地区), 题目 = factor(题目))
fmod1 <- lmer(最终得分 ~ 年份 + 年级 + 题目 + (1|地区), data = fpspr)
summary(fmod1)

fmod2 <- lmer(最终得分 ~ 年份 + 年级 + (1|地区), data = fpspr)
summary(fmod2)

flmod1 <- lm(最终得分 ~ 年份 + 年级 + 地区, data = fpspr)
summary(flmod1)

## on the qualifying question
fpsprq <- fpspt %>% filter(题目 == 'qualifying problem') %>% mutate(年级 =  factor(年级), 年份 =  factor(年份), 地区 = factor(地区))
fqmod1 <- lmer(最终得分 ~ 年份 + 年级 + (1|地区), data = fpsprq)
summary(fqmod1)

fqmod2 <- lmer(最终得分 ~ 年份 + 年级 + (1|地区), data = fpsprq)
summary(fqmod2)
frep <- data.frame(coef(summary(fqmod2))[(1:4),], pvalue =2*(1-pnorm(abs(coef(summary(fqmod2))[(1:4),3]))))
print(frep, digits = 4)
fqlmod1 <- lm(最终得分 ~ 年份 + 年级 + 地区, data = fpsprq)
summary(fqlmod1)

## each step
(faovt <- anova(aov(最终得分 ~ 年级 + 地区, data = fpsprq)))
(faovr <- anova(aov(总体回顾得分 ~ 年级 + 地区, data = fpsprq)))
(faov1 <- anova(aov(步骤1总分 ~ 年级 + 地区, data = fpsprq)))
(faov2 <- anova(aov(步骤2总分 ~ 年级 + 地区, data = fpsprq)))
(faov3 <- anova(aov(步骤3总分 ~ 年级 + 地区, data = fpsprq)))
(faov45 <- anova(aov(步骤4和步骤5总分 ~ 年级 + 地区, data = fpsprq)))
(faov6 <- anova(aov(步骤6总分 ~ 年级 + 地区, data = fpsprq)))

## global tests
g18j <- read_csv('global18junior.csv') %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g18jf <- g18j %>% filter(国家 == 1)
g18js <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g18j, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2018全球赛小学组得分')+
  geom_label_repel(data = g18jf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g18j.pdf')
g18js
dev.off()

g18m <- read_csv('global18middle.csv') %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g18mf <- g18m %>% filter(国家 == 1)
g18ms <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g18m, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2018全球赛初中组得分')+
  geom_label_repel(data = g18mf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g18m.pdf')
g18ms
dev.off()

g18s <- read_csv('global18senior.csv') 
g18s <- g18s[complete.cases(g18s),] %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g18sf <- g18s %>% filter(国家 == 1)
g18ss <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g18s, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2018全球赛高中组得分')+
  geom_label_repel(data = g18sf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g18s.pdf')
g18ss
dev.off()

g19j <- read_csv('global19junior.csv') %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g19jf <- g19j %>% filter(国家 == 1)
g19js <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g19j, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2019全球赛小学组得分')+
  geom_label_repel(data = g19jf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g19j.pdf')
g19js
dev.off()

g19m <- read_csv('global19middle.csv') %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g19mf <- g19m %>% filter(国家 == 1)
g19ms <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g19m, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2019全球赛初中组得分')+
  geom_label_repel(data = g19mf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g19m.pdf')
g19ms
dev.off()

g19s <- read_csv('global19senior.csv') 
g19s <- g19s[complete.cases(g19s),] %>% filter(评分1 > 10 & 评分2 > 10) %>% mutate(国家 = as.factor(ifelse(is.na( iconv(Affiliate, "", "ASCII") ), 1, 0)))
g19sf <- g19s %>% filter(国家 == 1)
g19ss <- ggplot()+
  theme(text = element_text(family = "KaiTi"), legend.position = "none")+
  geom_point(data = g19s, aes(x=评分1, y=评分2, col=国家, shape=国家))+
  labs(title = '2019全球赛高中组得分')+
  geom_label_repel(data = g19sf, aes(x=评分1, y=评分2, label = Affiliate), size = 2)

pdf('g19s.pdf')
g19ss
dev.off()

pdf('s1819.pdf',width=12, height = 8)
grid.arrange(g18js,g18ms,g18ss,g19js,g19ms,g19ss,nrow=2)
dev.off()
