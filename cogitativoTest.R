### test.R ###

# library(biglm)

setwd("C:/Users/Joe/Google Drive/Jobs/Cogitativo")

data = read.csv('claim.sample.csv')

# Construct dependent variable:
pattern = c("F13","J8G","J05","JB8","JE1","JC9","JF1","JF9","JG1","JPA","JES")
pattern1 = paste(pattern, collapse="|")
data$depVar = as.numeric(grepl(pattern1, data$Denial.Reason.Code))

# There is too much data to run the regression on my laptop, so I take a random sample of size N
# Note that unused factor levels are dropped
# the code is written to be able to use random samples, so it can handle even larger datasets if necessary
N = 60000 
dataSample = droplevels(data[sample(nrow(data), N), ])

# Count number of NA's in each variable
# for (var in names(dataSample)) {
#   print(paste(var))
#   print(sum(is.na(dataSample[[paste(var)]])))
# }

# dataSample$Claim.Number = factor(dataSample$Claim.Number) # too many NA's and too many levels - don't use
dataSample$Claim.Line.Number = factor(dataSample$Claim.Line.Number)
# dataSample$Member.ID = factor(dataSample$Member.ID) # too many levels - leave off for now
dataSample$Provider.ID = factor(dataSample$Provider.ID)
# dataSample$Line.Of.Business.ID = factor(dataSample$Line.Of.Business.ID) # too many NA's - don't use
dataSample$Revenue.Code = factor(dataSample$Revenue.Code)
dataSample$Service.Code = factor(dataSample$Service.Code)
# dataSample$Place.Of.Service.Code = factor(dataSample$Place.Of.Service.Code) # too many NA's - don't use
# dataSample$Procedure.Code = factor(dataSample$Procedure.Code) # too many levels
# dataSample$Diagnosis.Code = factor(dataSample$Diagnosis.Code) # too many levels
dataSample$Denial.Reason.Code = factor(dataSample$Denial.Reason.Code)
dataSample$Price.Index = factor(dataSample$Price.Index)
dataSample$In.Out.Of.Network = factor(dataSample$In.Out.Of.Network)
dataSample$Reference.Index = factor(dataSample$Reference.Index)
dataSample$Pricing.Index = factor(dataSample$Pricing.Index)
dataSample$Capitation.Index = factor(dataSample$Capitation.Index)
# dataSample$Group.Index = factor(dataSample$Group.Index) # too many levels
# dataSample$Subscriber.Index = factor(dataSample$Subscriber.Index) # too many levels - leave off for now
# dataSample$Subgroup.Index = factor(dataSample$Subgroup.Index) # too many levels
dataSample$Claim.Type = factor(dataSample$Claim.Type)
dataSample$Claim.Subscriber.Type = factor(dataSample$Claim.Subscriber.Type)
dataSample$Claim.Pre.Prince.Index = factor(dataSample$Claim.Pre.Prince.Index)
dataSample$Claim.Current.Status = factor(dataSample$Claim.Current.Status)
dataSample$Network.ID = factor(dataSample$Network.ID)
dataSample$Agreement.ID = factor(dataSample$Agreement.ID)

# Count total number of factors in data frame to make sure there are significantly fewer variables than obs:
sum = 0
for (var in names(dataSample)) {
  print(paste(var))
  levels = length(levels(dataSample[[paste(var)]]))
  print(levels)
  sum = sum + levels
}
sum

# bigglm runs faster than glm if there is only one chunk, but the predict function won't compute se's
# I decided to use OLS rather than probit because it works perfectly in predicting out-of-sample. 
# Since probit is much slower than OLS, I don't use it here, even though it is unbiased.
ptm = proc.time()
model = lm(depVar ~ Claim.Charge.Amount + Subscriber.Payment.Amount + Provider.Payment.Amount 
              + Claim.Line.Number + Provider.ID + Revenue.Code + Service.Code + Denial.Reason.Code
              + Price.Index + In.Out.Of.Network + Reference.Index + Pricing.Index + Capitation.Index
              + Claim.Type + Claim.Subscriber.Type + Claim.Pre.Prince.Index
              + Claim.Current.Status + Network.ID + Agreement.ID, 
              data=dataSample)
time = proc.time() - ptm
time

# look at NA coefs to see what's happening:
# coefs = data.frame(coef(model))
# coefsNA = subset(coefs, is.na(coefs$coef.model.))
# rownames(coefsNA)

# Do prediction based on all of the data (rather than just the random sample)
# Note that since the random sample does not contain all of the factors in the original data, we have to delete any observations with this new factors in order to make predictions based on that observation
shared.Claim.Line.Number = levels(dataSample$Claim.Line.Number)
shared.Provider.ID = levels(dataSample$Provider.ID)
shared.Revenue.Code = levels(dataSample$Revenue.Code)
shared.Service.Code = levels(dataSample$Service.Code)
shared.Denial.Reason.Code = levels(dataSample$Denial.Reason.Code)
shared.Price.Index = levels(dataSample$Price.Index)
shared.In.Out.Of.Network = levels(dataSample$In.Out.Of.Network)
shared.Reference.Index = levels(dataSample$Reference.Index)
shared.Pricing.Index = levels(dataSample$Pricing.Index)
shared.Capitation.Index = levels(dataSample$Capitation.Index)
shared.Claim.Type = levels(dataSample$Claim.Type)
shared.Claim.Subscriber.Type = levels(dataSample$Claim.Subscriber.Type)
shared.Claim.Pre.Prince.Index = levels(dataSample$Claim.Pre.Prince.Index)
shared.Claim.Current.Status = levels(dataSample$Claim.Current.Status)
shared.Network.ID = levels(dataSample$Network.ID)
shared.Agreement.ID = levels(dataSample$Agreement.ID)

dataShared = droplevels(data[data$Claim.Line.Number %in% shared.Claim.Line.Number
                             & data$Provider.ID %in% shared.Provider.ID
                             & data$Revenue.Code %in% shared.Revenue.Code 
                             & data$Service.Code %in% shared.Service.Code 
                             & data$Denial.Reason.Code %in% shared.Denial.Reason.Code
                             & data$Price.Index %in% shared.Price.Index
                             & data$In.Out.Of.Network %in% shared.In.Out.Of.Network
                             & data$Reference.Index %in% shared.Reference.Index
                             & data$Pricing.Index %in% shared.Pricing.Index
                             & data$Capitation.Index %in% shared.Capitation.Index
                             & data$Claim.Type %in% shared.Claim.Type
                             & data$Claim.Subscriber.Type %in% shared.Claim.Subscriber.Type
                             & data$Claim.Pre.Prince.Index %in% shared.Claim.Pre.Prince.Index
                             & data$Claim.Current.Status %in% shared.Claim.Current.Status
                             & data$Network.ID %in% shared.Network.ID
                             & data$Agreement.ID %in% shared.Agreement.ID
                             ,])

new = data.frame(Claim.Charge.Amount = dataShared$Claim.Charge.Amount, 
                 Subscriber.Payment.Amount = dataShared$Subscriber.Payment.Amount,
                 Provider.Payment.Amount = dataShared$Provider.Payment.Amount,
                 Claim.Line.Number = factor(dataShared$Claim.Line.Number),
                 Provider.ID = factor(dataShared$Provider.ID),
                 Revenue.Code = factor(dataShared$Revenue.Code),
                 Service.Code = factor(dataShared$Service.Code),
                 Denial.Reason.Code = factor(dataShared$Denial.Reason.Code),
                 Price.Index = factor(dataShared$Price.Index),
                 In.Out.Of.Network = factor(dataShared$In.Out.Of.Network),
                 Reference.Index = factor(dataShared$Reference.Index),
                 Pricing.Index = factor(dataShared$Pricing.Index),
                 Capitation.Index = factor(dataShared$Capitation.Index),
                 Claim.Type = factor(dataShared$Claim.Type),
                 Claim.Subscriber.Type = factor(dataShared$Claim.Subscriber.Type),
                 Claim.Pre.Prince.Index = factor(dataShared$Claim.Pre.Prince.Index),
                 Claim.Current.Status = factor(dataShared$Claim.Current.Status),
                 Network.ID = factor(dataShared$Network.ID),
                 Agreement.ID = factor(dataShared$Agreement.ID),
                 depVar = dataShared$depVar)

# Check to make sure all of the factors are the same
# for (var in names(dataSample)) {
#   print(all(levels(new[[paste(var)]])==levels(dataSample[[paste(var)]])))
# }

# predictions with 95% confidence intervals (for some reason they won't show up, and the standard errors won't either)
pred.w.clim <- predict(model, newdata=new[1:100,], se.fit=TRUE, interval="confidence") # , se.fit=TRUE
# note that fit is rank-deficient due to large number of regressors (model is overfit).
# See http://www.mathworks.com/matlabcentral/newsreader/view_thread/134480
# Since it is not truncated above and below by 0 and 1, this mean is biased. However, it is close to 0, 
# which makes sense given the small number of positives
mean(pred.w.clim$fit) 
mean(pred.w.clim$se.fit) 

# Compute average probability of depVar==1 when depVar==1 vs. when depVar==0 (to see if model predicts well)
# I exclude obs in the sample data so that this is an out-of-sample test
data0 = dataShared[dataShared$depVar==0 & !(dataShared$X %in% dataSample$X),]
data1 = dataShared[dataShared$depVar==1 & !(dataShared$X %in% dataSample$X),]

new0 = data.frame(Claim.Charge.Amount = data0$Claim.Charge.Amount, 
                  Subscriber.Payment.Amount = data0$Subscriber.Payment.Amount,
                  Provider.Payment.Amount = data0$Provider.Payment.Amount,
                  Claim.Line.Number = factor(data0$Claim.Line.Number),
                  Provider.ID = factor(data0$Provider.ID),
                  Revenue.Code = factor(data0$Revenue.Code),
                  Service.Code = factor(data0$Service.Code),
                  Denial.Reason.Code = factor(data0$Denial.Reason.Code),
                  Price.Index = factor(data0$Price.Index),
                  In.Out.Of.Network = factor(data0$In.Out.Of.Network),
                  Reference.Index = factor(data0$Reference.Index),
                  Pricing.Index = factor(data0$Pricing.Index),
                  Capitation.Index = factor(data0$Capitation.Index),
                  Claim.Type = factor(data0$Claim.Type),
                  Claim.Subscriber.Type = factor(data0$Claim.Subscriber.Type),
                  Claim.Pre.Prince.Index = factor(data0$Claim.Pre.Prince.Index),
                  Claim.Current.Status = factor(data0$Claim.Current.Status),
                  Network.ID = factor(data0$Network.ID),
                  Agreement.ID = factor(data0$Agreement.ID),
                  depVar = data0$depVar)

N0 = 10000 # prediction sample size
pred.w.clim.0 <- predict(model, newdata = new0[sample(nrow(new0), N0), ], interval="confidence", se.fit=TRUE)
# Note that the average prediction is approximately 0, and the average standard error is extremely small.
# Thus, the prediction is extremely accurate.
mean(pred.w.clim.0$fit) # mean predicted probability of depVar==0
mean(pred.w.clim.0$se.fit)

# Classify as depVar==0 iff P(depVar==1)<0.5
fit = data.frame(pred.w.clim.0$fit)
positives0 = subset(fit, fit>=0.5)
negatives0 = subset(fit, fit<0.5)
specificity = length(t(negatives0)) / (length(t(positives0)) + length(t(negatives0)))
specificity

new1 = data.frame(Claim.Charge.Amount = data1$Claim.Charge.Amount, 
                 Subscriber.Payment.Amount = data1$Subscriber.Payment.Amount,
                 Provider.Payment.Amount = data1$Provider.Payment.Amount,
                 Claim.Line.Number = factor(data1$Claim.Line.Number),
                 Provider.ID = factor(data1$Provider.ID),
                 Revenue.Code = factor(data1$Revenue.Code),
                 Service.Code = factor(data1$Service.Code),
                 Denial.Reason.Code = factor(data1$Denial.Reason.Code),
                 Price.Index = factor(data1$Price.Index),
                 In.Out.Of.Network = factor(data1$In.Out.Of.Network),
                 Reference.Index = factor(data1$Reference.Index),
                 Pricing.Index = factor(data1$Pricing.Index),
                 Capitation.Index = factor(data1$Capitation.Index),
                 Claim.Type = factor(data1$Claim.Type),
                 Claim.Subscriber.Type = factor(data1$Claim.Subscriber.Type),
                 Claim.Pre.Prince.Index = factor(data1$Claim.Pre.Prince.Index),
                 Claim.Current.Status = factor(data1$Claim.Current.Status),
                 Network.ID = factor(data1$Network.ID),
                 Agreement.ID = factor(data1$Agreement.ID),
                 depVar = data1$depVar)

pred.w.clim.1 = predict(model, new1, type="response", se.fit=TRUE)
# Note that the average prediction is 1, and the average standard error is extremely small.
# Thus, the prediction is extremely accurate.
mean(pred.w.clim.1$fit) # mean predicted probability of depVar==1
mean(pred.w.clim.1$se.fit)

# Classify as depVar==1 iff P(depVar==1)>=0.5
fit = data.frame(pred.w.clim.1$fit)
positives1 = subset(fit, fit>=0.5)
negatives1 = subset(fit, fit<0.5)
sensitivity = length(t(positives1)) / (length(t(positives1)) + length(t(negatives1)))
sensitivity
