# load data
# delete NaNs and empty columns


df.MEI_Master <- data.frame(read.csv("data/02-2016/alldata.csv", header = TRUE, sep = ",", dec = ","))
ve.selectJP <- c(2,3,4,5,6,7,8,9)
ve.selectCH <- c(10,11,12,13,14,15,16,17)
ve.selectUK <- c(18,19,20,21,22,23,24,25)
ve.selectUS <- c(26,27,28,29,30,31,32)
ve.selectEA <- c(33,34,35,36,37,38,39,40)
ve.selectRU <- c(41,42,43,44,45,46,47,48)

df.US_JP <- df.MEI_Master[,c(1,ve.selectUS, ve.selectJP)]
df.US_CH <- df.MEI_Master[,c(1,ve.selectUS, ve.selectCH)]
df.US_UK <- df.MEI_Master[,c(1,ve.selectUS, ve.selectUK)]
df.US_EA <- df.MEI_Master[,c(1,ve.selectUS, ve.selectEA)]
df.US_RU <- df.MEI_Master[,c(1,ve.selectUS, ve.selectRU)]

df.log_US_JP <- data.frame(s=log(df.US_JP[,16]),                        # log exchange rate
                           m1=log(df.US_JP[,5])-log(df.US_JP[,12]),     # log m1 index
                           m3=log(df.US_JP[,6])-log(df.US_JP[,13]),     # log m3 index
                           gdp=log(df.US_JP[,15])-log(df.US_JP[,8]),    # log gdp                
                           irlt=df.US_JP[,2]-df.US_JP[,9],              # irlt
                           irst=df.US_JP[,3]-df.US_JP[,10],             # irst
                           immediate=df.US_JP[,4]-df.US_JP[,11],        # immediate
                           shares=log(df.US_JP[,7])-log(df.US_JP[,14])  # log shares index
)

df.log_US_CH <- data.frame(s=log(df.US_CH[,16]),                        # log exchange rate
                           m1=log(df.US_CH[,5])-log(df.US_CH[,12]),     # log m1 index
                           m3=log(df.US_Ch[,6])-log(df.US_CH[,13]),     # log m3 index
                           gdp=log(df.US_CH[,15])-log(df.US_CH[,8]),    # log gdp                
                           irlt=df.US_CH[,2]-df.US_CH[,9],              # irlt
                           irst=df.US_CH[,3]-df.US_CH[,10],             # irst
                           immediate=df.US_CH[,4]-df.US_CH[,11],        # immediate
                           shares=log(df.US_CH[,7])-log(df.US_CH[,14])  # log shares index
)

df.log_US_UK <- data.frame(s=log(df.US_UK[,16]),                        # log exchange rate
                           m1=log(df.US_UK[,5])-log(df.US_UK[,12]),     # log m1 index
                           m3=log(df.US_UK[,6])-log(df.US_UK[,13]),     # log m3 index
                           gdp=log(df.US_UK[,15])-log(df.US_UK[,8]),    # log gdp                
                           irlt=df.US_UK[,2]-df.US_UK[,9],              # irlt
                           irst=df.US_UK[,3]-df.US_UK[,10],             # irst
                           immediate=df.US_UK[,4]-df.US_UK[,11],        # immediate
                           shares=log(df.US_UK[,7])-log(df.US_UK[,14])  # log shares index
)

df.log_US_EA <- data.frame(s=log(df.US_EA[,16]),                        # log exchange rate
                           m1=log(df.US_EA[,5])-log(df.US_EA[,12]),     # log m1 index
                           m3=log(df.US_EA[,6])-log(df.US_EA[,13]),     # log m3 index
                           gdp=log(df.US_EA[,15])-log(df.US_EA[,8]),    # log gdp                
                           irlt=df.US_EA[,2]-df.US_EA[,9],              # irlt
                           irst=df.US_EA[,3]-df.US_EA[,10],             # irst
                           immediate=df.US_EA[,4]-df.US_EA[,11],        # immediate
                           shares=log(df.US_EA[,7])-log(df.US_EA[,14])  # log shares index
)

df.log_US_RU <- data.frame(s=log(df.US_RU[,16]),                        # log exchange rate
                           m1=log(df.US_RU[,5])-log(df.US_RU[,12]),     # log m1 index
                           m3=log(df.US_RU[,6])-log(df.US_RU[,13]),     # log m3 index
                           gdp=log(df.US_RU[,15])-log(df.US_RU[,8]),    # log gdp                
                           irlt=df.US_RU[,2]-df.US_RU[,9],              # irlt
                           irst=df.US_RU[,3]-df.US_RU[,10],             # irst
                           immediate=df.US_RU[,4]-df.US_RU[,11],        # immediate
                           shares=log(df.US_RU[,7])-log(df.US_RU[,14])  # log shares index
)

rm(df.US_JP, df.US_CH, df.US_UK, df.US_EA, df.US_RU)
rm(ve.selectUS, ve.selectJP, ve.selectCH, ve.selectUK, ve.selectEA, ve.selectRU)

# EA
df.log_US_EA <- df.log_US_EA[-439,]  # remove NaN rows
df.log_US_EA <- df.log_US_EA[,-8]    # remove shares
rownames(df.log_US_EA) <- seq(1:nrow(df.log_US_EA))

# JP
df.log_US_JP <- df.log_US_JP[-439,]
df.log_US_JP <- df.log_US_JP[-(1:12),]
rownames(df.log_US_JP) <- seq(1:nrow(df.log_US_JP))

# RU
df.log_US_RU <- df.log_US_RU[-(436:439),]
df.log_US_RU <- df.log_US_RU[-(1:198),]
df.log_US_RU <- df.log_US_RU[,-8]
rownames(df.log_US_RU) <- seq(1:nrow(df.log_US_RU))

# UK
df.log_US_UK <- df.log_US_UK[-439,]
df.log_US_UK <- df.log_US_UK[-(1:96),]
rownames(df.log_US_UK) <- seq(1:nrow(df.log_US_UK))

ve.errorEA <- summary(lm(s~., data=df.log_US_EA))$residuals
ve.errorJP <- summary(lm(s~., data=df.log_US_JP))$residuals
ve.errorRU <- summary(lm(s~., data=df.log_US_RU))$residuals
ve.errorUK <- summary(lm(s~., data=df.log_US_UK))$residuals

