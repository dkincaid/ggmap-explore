library(ggmap)
library(foreign)
library(maps)
library(reshape2)

county.data <- read.xport("CNTY12.xpt")
asthma.data <- county.data[,c("X_CASTHM1", "CNTYNAME", "X_STATE")]
asthma.data[asthma.data$X_CASTHM1==9,"X_CASTHM1"] <- NA
asthma.data$X_CASTHM1 = as.factor(asthma.data$X_CASTHM1)
asthma.data$X_STATE = as.factor(asthma.data$X_STATE)

data(state.fips)

state.lookup <- function(x) {
         abbrev = subset(state.fips, fips==x)$abb
         if (length(abbrev) > 0) as.character(abbrev[1]) else NA
}
         
asthma.data$state <- as.factor(sapply(asthma.data$X_STATE, state.lookup))

asthma.melted = melt(asthma.data)
asthma.summ = dcast(asthma.melted, CNTYNAME+state~variable, length, subset=.(value==2 & variable=='X_CASTHM1'), drop=TRUE)
asthma.summ[4] = NULL
colnames(asthma.summ) = c("county", "state", "asthma.count")

asthma.total = dcast(asthma.melted, CNTYNAME+state~variable, length, subset=.(variable=='X_CASTHM1'))
colnames(asthma.total) = c("county", "state", "totalpop")

asthma.summ = merge(asthma.summ, asthma.total)
asthma.summ$asthmaprev = with(asthma.summ, asthma.count / totalpop)
rm(asthma.melted)
rm(asthma.total)



