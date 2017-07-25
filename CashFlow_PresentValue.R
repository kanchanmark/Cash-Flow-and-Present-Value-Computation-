#Cash flow and present value analysis

#install.packages("optiRum")
library(optiRum)

#Inputs for loan age 0
#age <- 0
#funded_amount <- 12000
#term <- 60
#interest_rate <- 0.0993
#discount_rate <- 0.05
#loss_given_default <- 0.905
#servicing_charge <- 0.015
#prepay_rate <- c(0.0076,0.0092,0.0092,0.0093,0.0093,0.0093,0.0093,0.0093,0.0093,0.0093,0.0093,0.0093,0.0092,0.0092,0.0091,0.009,0.0089,0.0088,0.0087,0.0086,0.0085,0.0084,0.0082,0.0081,0.0079,0.0077,0.0076,0.0074,0.0072,0.007,0.0068,0.0066,0.0064,0.0062,0.006,0.0057,0.0055,0.0053,0.005,0.0048,0.0046,0.0043,0.0041,0.0038,0.0035,0.0033,0.003,0.0028,0.0025,0.0023,0.002,0.0018,0.0016,0.0013,0.0011,0.0008,0.0006,0.0004,0.0002,0)
#default_rate <- c(0,0,0,0.0001,0.0002,0.0005,0.0009,0.0013,0.0017,0.0021,0.0025,0.0028,0.003,0.0032,0.0033,0.0033,0.0033,0.0033,0.0032,0.0031,0.003,0.0029,0.0027,0.0026,0.0025,0.0023,0.0022,0.002,0.0019,0.0017,0.0016,0.0015,0.0014,0.0013,0.0012,0.0011,0.001,0.0009,0.0008,0.0007,0.0007,0.0006,0.0006,0.0005,0.0005,0.0004,0.0004,0.0003,0.0003,0.0003,0.0002,0.0002,0.0002,0.0002,0.0001,0.0001,0.0001,0.0001,0.0001,0)
#option <- 0

#Inputs for loan age not 0
age <- 26
funded_amount <- 13000
payoff_amount <- 8364.49
term <- 60
interest_rate <- 0.1229
discount_rate <- 0.04
loss_given_default <- 0.905
servicing_charge <- 0.015
prepay_rate <- c(0.007,0.0069,0.0067,0.0065,0.0063,0.0062,0.006,0.0058,0.0056,0.0054,0.0051,0.0049,0.0047,0.0045,0.0043,0.0041,0.0038,0.0036,0.0034,0.0031,0.0029,0.0027,0.0024,0.0022,0.002,0.0017,0.0015,0.0013,0.0011,0.0008,0.0006,0.0004,0.0002,0)
default_rate <- c(0.0036,0.0034,0.0032,0.003,0.0028,0.0026,0.0025,0.0023,0.0021,0.002,0.0019,0.0017,0.0016,0.0015,0.0014,0.0013,0.0012,0.0011,0.001,0.0009,0.0008,0.0007,0.0007,0.0006,0.0005,0.0005,0.0004,0.0004,0.0003,0.0003,0.0002,0.0002,0.0002,0.0001)
option <- 1

#Outputs
#First Payment Month for loan irrespective of loan age
Starting_Balance <- c()
Principal <- c()
Prepayment <- c()
Charge_Off <- c()
payment_term_month <- age + 1
Starting_Balance[1] <- ifelse(option == 0,funded_amount,payoff_amount)
rate <- interest_rate/12
if (option == 0) {
  terms <- term
} else {
  terms <- term + 1 - payment_term_month
}
if (option == 0) {
  amount <- Starting_Balance[1] + 1 - payment_term_month
} else {
  amount <- Starting_Balance[1]
}
Payment <- -PMT(rate,terms,amount)
Net_Interest <- (interest_rate/12)*Starting_Balance
Principal[1] <- Payment - Net_Interest
Prepayment[1] <- (Starting_Balance - Principal)*prepay_rate[1]
Defaults <- (Starting_Balance - Principal - Prepayment)*default_rate[1]
Charge_Off[1] <- Defaults
Losses <- Charge_Off*loss_given_default
Recovery <- Charge_Off*(1 - loss_given_default)
Servicing <- (Starting_Balance)*(servicing_charge/12)
Cash_Flow <- Payment + Prepayment + Recovery - Servicing
disc_rate <- discount_rate/12
dis_rate <- 1 + disc_rate
payment_month <- payment_term_month - age
denom <- dis_rate^payment_month
Discount_Cash_Flow <- Cash_Flow/denom
d <- data.frame(payment_term_month,Starting_Balance,Payment,Net_Interest,Principal,prepay_rate[1],default_rate[1],Prepayment,Defaults,Charge_Off,Losses,Recovery,Servicing,Cash_Flow,Discount_Cash_Flow)
colnames(d) <- c("payment_term_month","Starting_Balance","Payment","Net_Interest","Principal","prepay_rate","default_rate","Prepayment","Defaults","Charge_Off","Losses","Recovery","Servicing","Cash_Flow","Discount_Cash_Flow")

#Payment next month onwards
t <- payment_term_month + 1
while (t <= term) {
for (n in 2:length(prepay_rate)) {
  payment_term_month[n] <- t
  Starting_Balance[n] <- Starting_Balance[n-1] - Principal[n-1] - Prepayment[n-1] - Charge_Off[n-1]
  rate <- interest_rate/12
  terms <- term + 1 - payment_term_month[n]
  amount[n] <- Starting_Balance[n]
  Payment[n] <- -PMT(rate,terms,amount[n])
  Net_Interest[n] <- (interest_rate/12)*Starting_Balance[n]
  Principal[n] <- Payment[n] - Net_Interest[n]
  Prepayment[n] <- (Starting_Balance[n] - Principal[n])*prepay_rate[n]
  Defaults[n] <- (Starting_Balance[n] - Principal[n] - Prepayment[n])*default_rate[n]
  Charge_Off[n] <- Defaults[n]
  Losses[n] <- Charge_Off[n]*loss_given_default
  Recovery[n] <- Charge_Off[n]*(1 - loss_given_default)
  Servicing[n] <- (Starting_Balance[n]*servicing_charge)/12
  Cash_Flow[n] <- Payment[n] + Prepayment[n] + Recovery[n] - Servicing[n]
  disc_rate <- discount_rate/12
  dis_rate <- 1 + disc_rate
  payment_month[n] <- payment_term_month[n] - age
  denom <- dis_rate^payment_month[n]
  Discount_Cash_Flow[n] <- Cash_Flow[n]/denom
  combine <- data.frame(payment_term_month[n],Starting_Balance[n],Payment[n],Net_Interest[n],Principal[n],prepay_rate[n],default_rate[n],Prepayment[n],Defaults[n],Charge_Off[n],Losses[n],Recovery[n],Servicing[n],Cash_Flow[n],Discount_Cash_Flow[n])
  colnames(combine) <- c("payment_term_month","Starting_Balance","Payment","Net_Interest","Principal","prepay_rate","default_rate","Prepayment","Defaults","Charge_Off","Losses","Recovery","Servicing","Cash_Flow","Discount_Cash_Flow")
  t <- t + 1
  d <- rbind(d,combine)
}
}


(Present_value <- sum(Discount_Cash_Flow))

  
