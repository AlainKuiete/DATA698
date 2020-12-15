library(ggplot2)
# Create multiple curves to plot
roc1 <- roc(TARGET ~., data = prediction)
ggroc(roc1)


plot(roc(asth.mgt.min37$TARGET~en.pred.m$T), print.auc = TRUE)

par(mex=1)
plot(varImp(elastic.mod))

?plot



par(mfrow=c(3,3), mar = c(2,2,2,1.5))
barplot(sort(table(asthma.factor$TCH.SIGN),decreasing=TRUE),main="TCH.SIGN",las=2)
barplot(sort(table(asthma.factor$TCH.RESP),decreasing=TRUE),main="TCH.REPS",las=2)
barplot(sort(table(asthma.factor$TCH.MON),decreasing=TRUE),main="TCH.MON",las=2)
barplot(sort(table(asthma.factor$MGT.PLAN),decreasing=TRUE),main="MGT.PLAN",las=2)
barplot(sort(table(asthma.factor$MGT.CLAS),decreasing=TRUE),main="MGT.CLAS",las=2)
barplot(sort(table(asthma.factor$INHALERW),decreasing=TRUE),main="INHALERW",las=2)
barplot(sort(table(asthma.factor$MOD.ENV),decreasing=TRUE),main="MOD.ENV",las=2)



par(mfrow =c(3,3), mar=c(2,2,2,1.6))
barplot(sort(table(asthma.factor$LAST.MD), decreasing=TRUE),main="LAST.MD",las=2)
barplot(sort(table(asthma.factor$LAST.MED),decreasing=TRUE),main="LAST.MED",las=2)
barplot(sort(table(asthma.factor$HOSP.VST),decreasing=TRUE),main="HOSP.VST",las=2)
barplot(sort(table(asthma.factor$DUR.30D),decreasing=TRUE),main="DUR.30D",las=2)
barplot(sort(table(asthma.factor$ER.VISIT),decreasing=TRUE),main="ER.VISIT",las=2)
barplot(sort(table(asthma.factor$LAST.SYMP),decreasing=TRUE),main="LAST.SYMP",las=2)
barplot(sort(table(asthma.factor$COMPASTH),decreasing=TRUE),main="COMPASTH",las=2)
barplot(sort(table(asthma.factor$INCINDT),decreasing=TRUE),main="INCINDT",las=2)
barplot(sort(table(asthma.factor$WORKTALK),decreasing=TRUE),main="WORKTALK",las=2)
barplot(sort(table(asthma.factor$AGEG.F7),decreasing=TRUE),main="AGEG.F7",las=2)
barplot(sort(table(asthma.factor$ASRXCOST),decreasing=TRUE),main="ASRXCOST",las=2)
barplot(sort(table(asthma.factor$EDUCAL),decreasing=TRUE),main="EDUCAL",las=2)
#barplot(sort(table(asthma.factor$INS2),decreasing=TRUE),main="INS2",las=2)
barplot(sort(table(asthma.factor$SMOKE100),decreasing=TRUE),main="SMOKE100",las=2)
barplot(sort(table(asthma.factor$COPD),decreasing=TRUE),main="COPD",las=2)
barplot(sort(table(asthma.factor$BRONCH),decreasing=TRUE),main="BRONCH",las=2)
barplot(sort(table(asthma.factor$DEPRESS),decreasing=TRUE),main="DEPRESS",las=2)
barplot(sort(table(asthma.factor$EMPHY),decreasing=TRUE),main="EMPHY",las=2)
barplot(sort(table(asthma.factor$SEX),decreasing=TRUE),main="SEX",las=2)
barplot(sort(table(asthma.factor$X_INCOMG),decreasing=TRUE),main="X_INCOMG",las=2)
#barplot(sort(table(asthma.factor$X_GEOSTR),decreasing=TRUE),main="X_GEOSTR",las=2)
barplot(sort(table(asthma.factor$X_RFBMI5),decreasing=TRUE),main="X_RFBMI5",las=2)



cor_asthma.adult <- cor(asthma.mgt.adult1[,-c(1:7)], use = "na.or.complete")
corrplot(cor_asthma.adult, order = 'hclust', type = 'lower')


#### Elbow method to find the number of clusters 
#We run kmeans with different clusters from 1 to 16 and we produce a 
#scree plot to determine the number of cluster at the elbow.

set.seed(25)
# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:16) {
  # Fit the model: km.out
  km.out <- kmeans(responses, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:16, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares"
)


plot(resp.asthma$target)

ggplot(egt.sign, aes(x=target, y=proportion, group=TCH.SIGN, linetype=TCH.SIGN))+geom_line()

ggplot(egt.resp, aes(x=target, y=proportion, group=TCH.RESP, linetype=TCH.RESP))+geom_line()

ggplot(egt.mon, aes(x=target, y=proportion, group=TCH.MON, linetype=TCH.MON))+geom_line()

ggplot(egt.plan, aes(x=target, y=proportion, group=MGT.PLAN, linetype=MGT.PLAN))+geom_line()

ggplot(egt.clas, aes(x=target, y=proportion, group=MGT.CLAS, linetype=MGT.CLAS))+geom_line()

ggplot(egt.inhal, aes(x=target, y=proportion, group=INHALERW , linetype=INHALERW))+geom_line()

ggplot(egt.env, aes(x=target, y=proportion, group=MOD.ENV, linetype=MOD.ENV))+geom_line()





