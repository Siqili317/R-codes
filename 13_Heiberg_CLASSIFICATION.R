#File created at 12/18/2018
#This file aimed to classify Landsat based on field inventory plot classes

library("foreign")
setwd("G:/ESF/Manuscript3/Heibergdata/Rdata")


plotdt = read.dbf("test_plot_v2.dbf")

traindt_forest = plotdt[,-c(1,2,3,16)]
traindt_AGB = plotdt[,-c(1,2,3,15)]
#traindt_tree = plotdt[,-c(1,2,3,4,5)]

testdt = read.dbf("test_lidar.dbf")
#names(testdt)[1] = "B1"
#names(testdt)[8] = "MSAVI2"

library("randomForest")

#############################MODEL WITH ALL VARIABLES#####################################################
RF.forest = randomForest(Forest_Typ ~ ., traindt_forest, importance = TRUE, keep.forest = TRUE )
importance(RF.forest)
varImpPlot(RF.forest)
#OOB estimate of  error rate: 23.7% 
Pre.plot.forest = predict(RF.forest,testdt)
write.csv(Pre.plot.forest, file = "Pred_plot_foresttype.csv",row.names = TRUE)


RF.AGB = randomForest(AGB_classe ~ ., traindt_AGB, importance = TRUE, keep.forest = TRUE )
importance(RF.AGB)
varImpPlot(RF.AGB)
#OOB estimate of  error rate: 59.26% 
Pre.plot.AGB = predict(RF.AGB,testdt)
write.csv(Pre.plot.AGB, file = "Pred_plot_AGBdensity.csv",row.names = TRUE)

# 
# RF.tree = randomForest(Forest_treeDensity ~ ., traindt_tree, importance = TRUE, keep.forest = TRUE )
# importance(RF.tree)
# varImpPlot(RF.tree)
# #OOB estimate of  error rate: 66.3% 
# Pre.plot.tree = predict(RF.tree,testdt)
# write.csv(Pre.plot.tree, file = "Pred_plot_treedensity.csv",row.names = TRUE)
################################Final models #################################################################

# Pre.forest = predict(RF.forest,testdt)
# write.csv(Pre.forest, file = "G:/ESF/Manuscript3/Classification/AGB_based/Pred_forest_type.csv",row.names=TRUE)