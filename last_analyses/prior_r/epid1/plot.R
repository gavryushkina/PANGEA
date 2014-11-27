library(ggplot2)

table1 <- read.table('300914_Village_scA_sample1_epi1.log', skip=638, header =T)
table2 <- read.table('300914_Village_scB_sample1_epi1.log', skip=559, header =T)
table3 <- read.table('300914_Village_scC_sample1_epi1.log', skip=598, header =T)

#sorted_origin = sort(origin, decreasing = FALSE)
#up_index = length(sorted_origin) - ceiling(length(sorted_origin)*0.025)
#up=sorted_origin[up_index]

size = length(table1$origin)
burnin = ceiling(size*0.1)
origin = table1$origin[(burnin+1):size]

up1=mean(origin)

size = length(table2$origin)
burnin = ceiling(size*0.1)
origin = table2$origin[(burnin+1):size]

up2=mean(origin)

size = length(table3$origin)
burnin = ceiling(size*0.1)
origin = table3$origin[(burnin+1):size]

up3=mean(origin)

xmax=max(c(up1,up2,up3))

step=xmax/300

size = length(table1$origin)
burnin = ceiling(size*0.1)
origin = table1$origin[(burnin+1):size]

library(coda)

origin_mcmc = as.mcmc(origin)
origin_hpd = HPDinterval(origin_mcmc, prob=0.95)

#up = origin_hpd[1,2]
up=mean(origin)


times <-c(0, up-27, up-24, up-21, up-18, up-15, up-12, up-9, up-6, up-3, up)

sizeminburnin = size - burnin
array_size = 5 * sizeminburnin
rnot <- array(1:array_size, dim=c(5,sizeminburnin))

current_index = match('rnot1', colnames(table1))

for (i in 1:5) {
    rnot[i,] = table1[,current_index][(burnin+1):size]
    current_index=current_index+1
}

meanrnot <- vector()
highrnot <- vector()
lowrnot <- vector()

for (i in 1:5) {
    current_rnot = rnot[i,]
    meanrnot <- c(meanrnot, median(current_rnot))
    rnot_mcmc = as.mcmc(current_rnot)
    rnot_hpd = HPDinterval(rnot_mcmc, prob=0.95)
    highrnot <- c(highrnot, rnot_hpd[1,2])
    lowrnot <- c(lowrnot, rnot_hpd[1,1])
}

times_split1 <- vector()
meanrnot_split1 <- vector()
highrnot_split1 <- vector()
lowrnot_split1 <- vector()

current_index=1
current_time=times[1]
for (i in 1:300) {
    if (times[current_index+1]<current_time) {
        current_index=current_index+1
    }
    times_split1 <- c(times_split1, current_time)
    meanrnot_split1 <- c(meanrnot_split1, meanrnot[current_index])
    highrnot_split1 <- c(highrnot_split1, highrnot[current_index])
    lowrnot_split1 <- c(lowrnot_split1, lowrnot[current_index])
    current_time=current_time+step
    print(current_time)
    print(up)
    if (current_time > up) {
        break
    }
}



size = length(table2$origin)
burnin = ceiling(size*0.1)

origin = table2$origin[(burnin+1):size]

library(coda)

origin_mcmc = as.mcmc(origin)
origin_hpd = HPDinterval(origin_mcmc, prob=0.95)

#up = origin_hpd[1,2]
up=mean(origin)

times <-c(0, up-27, up-24, up-21, up-18, up-15, up-12, up-9, up-6, up-3, up)

sizeminburnin = size - burnin
array_size = 5 * sizeminburnin
rnot <- array(1:array_size, dim=c(5,sizeminburnin))

current_index = match('rnot1', colnames(table2))

for (i in 1:5) {
    rnot[i,] = table2[,current_index][(burnin+1):size]
    current_index=current_index+1
}

meanrnot <- vector()
highrnot <- vector()
lowrnot <- vector()

for (i in 1:5) {
    current_rnot = rnot[i,]
    meanrnot <- c(meanrnot, median(current_rnot))
    rnot_mcmc = as.mcmc(current_rnot)
    rnot_hpd = HPDinterval(rnot_mcmc, prob=0.95)
    highrnot <- c(highrnot, rnot_hpd[1,2])
    lowrnot <- c(lowrnot, rnot_hpd[1,1])
}

times_split2 <- vector()
meanrnot_split2 <- vector()
highrnot_split2 <- vector()
lowrnot_split2 <- vector()

current_index=1
current_time=times[1]
for (i in 1:300) {
    if (times[current_index+1]<current_time) {
        current_index=current_index+1
    }
    times_split2 <- c(times_split2, current_time)
    meanrnot_split2 <- c(meanrnot_split2, meanrnot[current_index])
    highrnot_split2 <- c(highrnot_split2, highrnot[current_index])
    lowrnot_split2 <- c(lowrnot_split2, lowrnot[current_index])
    current_time=current_time+step
    if (current_time > up) {
        break
    }
}

size = length(table3$origin)
burnin = ceiling(size*0.1)

origin = table3$origin[(burnin+1):size]

library(coda)

origin_mcmc = as.mcmc(origin)
origin_hpd = HPDinterval(origin_mcmc, prob=0.95)

#up = origin_hpd[1,2]
up=mean(origin)

times <-c(0, up-27, up-24, up-21, up-18, up-15, up-12, up-9, up-6, up-3, up)

sizeminburnin = size - burnin
array_size = 5 * sizeminburnin
rnot <- array(1:array_size, dim=c(5,sizeminburnin))

current_index = match('rnot1', colnames(table3))

for (i in 1:5) {
    rnot[i,] = table3[,current_index][(burnin+1):size]
    current_index=current_index+1
}

meanrnot <- vector()
highrnot <- vector()
lowrnot <- vector()

for (i in 1:5) {
    current_rnot = rnot[i,]
    meanrnot <- c(meanrnot, median(current_rnot))
    rnot_mcmc = as.mcmc(current_rnot)
    rnot_hpd = HPDinterval(rnot_mcmc, prob=0.95)
    highrnot <- c(highrnot, rnot_hpd[1,2])
    lowrnot <- c(lowrnot, rnot_hpd[1,1])
}

times_split3 <- vector()
meanrnot_split3 <- vector()
highrnot_split3 <- vector()
lowrnot_split3 <- vector()

current_index=1
current_time=times[1]
for (i in 1:300) {
    if (times[current_index+1]<current_time) {
        current_index=current_index+1
    }
    times_split3 <- c(times_split3, current_time)
    meanrnot_split3 <- c(meanrnot_split3, meanrnot[current_index])
    highrnot_split3 <- c(highrnot_split3, highrnot[current_index])
    lowrnot_split3 <- c(lowrnot_split3, lowrnot[current_index])
    current_time=current_time+step
    if (current_time > up) {
        break
    }
}

ggplot() + ggtitle("Epidemic 3 sample G1 (declining)")+ geom_errorbar(aes(x = times_split1, y = meanrnot_split1, ymin = lowrnot_split1, ymax = highrnot_split1), colour = 'grey', width = 0.4)+ geom_point(aes(x = times_split1, y = meanrnot_split1))+xlab("Time since origin of epidemic")+ylab("R0")+ylim(0,5)+xlim(0,xmax)

ggplot() + ggtitle("Epidemic 3 sample H1 (stable)") + geom_errorbar(aes(x = times_split2, y = meanrnot_split2, ymin = lowrnot_split2, ymax = highrnot_split2), colour = 'grey', width = 0.4)+ geom_point(aes(x = times_split2, y = meanrnot_split2))+xlab("Time since origin of epidemic")+ylab("R0")+ylim(0,5)+xlim(0,xmax)

ggplot()  + ggtitle("Epidemic 3 sample I1 (growing)") + geom_errorbar(aes(x = times_split3, y = meanrnot_split3, ymin = lowrnot_split3, ymax = highrnot_split3), colour = 'grey', width = 0.4)+ geom_point(aes(x = times_split3, y = meanrnot_split3))+xlab("Time since origin of epidemic")+ylab("R0")+ylim(0,5)+xlim(0,xmax)

#qplot(x = grid, y = meanrnot, asp=1)
#qplot(x = grid, y = highrnot, asp=1)
#qplot(x = grid, y = lowrnot, asp=1)
