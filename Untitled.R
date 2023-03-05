### STAT372 A2 ###

# Q1 STARDEW VALLEY OCEAN FISHING
# 1 (Bait + Dressed Spinner)
# 2 (Wild Bait + Dressed Spinner)
# 3 (Bait + Curiosity Lure)
# 4 (Wild Bait + Curiosity Lure)

# simulated by 15 * runif(14, r, 5) r = 3, 3.5
###
# fishes = c(round(15 * runif(14, 3.5, 5)), 
#           round(15 * runif(14, 3.5, 5)), 
#           round(15 * runif(14, 3, 5)),
#           round(15 * runif(14, 3, 5)))
###
write.csv(fishing.data, "Fishing.csv")
fishing <- read.csv("Fishing.csv")


treat = rep(1:4, each = 14)
treat = factor(treat)
day = rep(1:14, each = 1, len = (14*4))
day = factor(day)
bait = c(rep(c("Bait", "Wild Bait"), each = 14), rep(c("Bait", "Wild Bait"), each = 14))
bait = factor(bait)
tackle = c(rep("D. Spinner", each = 28), rep("C. Lure", each = 28))
tackle = factor(tackle)

fishing.data = data.frame(day, treat, bait, tackle, fishes)

                        
interaction.plot(bait, tackle, fishing.data$fishes, 
                 ylab= 'Average Encountered', xlab = 'Fishing Bait')

fish_mod = lm(fishes ~ bait + tackle + bait:tackle + day, data = fishing.data)
anova(fish_mod)

# summary of treat avgs
test_tab <- as.table(rbind(c(63.64286, 63.85714, 63.64286), c(61.5, 62, 61.5),
                           c(63.64286, 61.5, 63.64286)))
rownames(test_tab) <- c("Bait", "Wild Bait", "average")
colnames(test_tab) <- c("Dressed Spinner", "Curiosity Lure", "average")

# residuals testing
# 1) constant variance
fish_res = residuals(fish_mod)
plot(fish_res~treat,cex.lab=1.5,cex.axis=1.5,xlab='Treatment')
fligner.test(fish_res, treat)

# 2) normality of residuals
hist(fish_res,main='Histogram of Residuals')
qqnorm(fish_res) ; qqline(fish_res)
shapiro.test(fish_res)

# 3) Constant mean and independence
plot(fish_res)
abline(h=0,lty=2)
fish_y.hat <- fitted(fish_mod)
plot(fish_res~fish_y.hat)




### Q3 ###

hr.business = c(500,580,550)
hr.engineer = c(540,460,490)
hr.arts = c(480,400,420)

day.business = c(460,540,500)
day.engineer = c(560,620,580)
day.arts = c(420,480,450)

week.business = c(560,600,580)
week.engineer = c(600,580,600)
week.arts = c(480,410,430)



# a) boxplot the data

par(mfrow=c(1,3))
boxplot(hr.business,day.business,week.business, main = 'Scores of Business', 
        xlab = "Prep Program", ylab = "Score", 
        names = c("3-hr", "1-day", "2-week"), 
        cex.axis = 0.8, cex.main = 0.7)
boxplot(hr.engineer,day.engineer,week.engineer, main = 'Scores of Engineering', 
        xlab = "Prep Program", ylab = "Score", 
        names = c("3-hr", "1-day", "2-week"), 
        cex.axis = 0.8, cex.main = 0.7)
boxplot(hr.arts,day.arts,week.arts, main = 'Scores of Arts Sciences', 
        xlab = "Prep Program", ylab = "Score", 
        names = c("3-hr", "1-day", "2-week"), 
        cex.axis = 0.8, cex.main = 0.7)

# b)

rev.treat = rep(1:3, each = 9)
rev.treat = factor(rev.treat)
rev.block = rep(1:9, each = 3)
rev.block = factor(rev.block)
scores = c(hr.business, hr.engineer, hr.arts, 
           day.business, day.engineer, day.arts,
           week.business, week.engineer, week.arts)

score.mod = lm(scores ~ rev.treat + rev.block)
anova(score.mod)

# c)

program = rep(c("3-hr", "1-day", "2-week"), each = 9)
program = factor(program)
faculty = rep(c("Bus", "Eng", "Art"), each = 3, len = (9*3))
faculty = factor(faculty)

par(mfrow=c(1,1))
interaction.plot(program, faculty, scores)

# e)

score.mod2 = lm(scores ~ program + faculty + program:faculty)
anova(score.mod2)

# f) residuals for part d)

f.treat = rep(1:9, each = 3, len = 27)
f.treat = factor(f.treat)
score.mod2 = lm(scores ~ program + faculty + program:faculty)

# 1) constant variance
r = residuals(score.mod2)
par(mfrow=c(1,2))
stripchart(r~f.treat,vertical=T , pch=16, xlab='Treatment')
plot(r~f.treat,xlab='Treatment')

fligner.test(r,f.treat)

# 2) Normality of Residuals
par(mfrow=c(1,2))
hist(r,main='Histogram of Residuals')
qqnorm(r) ; qqline(r)

shapiro.test(r)


# 3) Constant Mean and Independence

par(mfrow=c(1,2)) 
plot(r) 
abline(h=0,lty=2) 
y.hat <- fitted(score.mod2) 
plot(r~y.hat) 



# NEW f) residuals for part b)
rev.treat = rep(1:3, each = 9)
rev.treat = factor(rev.treat)
rev.block = rep(1:9, each = 3)
rev.block = factor(rev.block)
scores = c(hr.business, hr.engineer, hr.arts, 
           day.business, day.engineer, day.arts,
           week.business, week.engineer, week.arts)
score.mod = lm(scores ~ rev.treat + rev.block)

# 1) constant variance

# residuals across treatments
r.b = residuals(score.mod)
par(mfrow=c(1,2))
stripchart(r.b~rev.treat,vertical=T , pch=16, xlab='Treatment')
plot(r.b~rev.treat,xlab='Treatment')

fligner.test(r.b,rev.treat)

# residuals across blocks
par(mfrow=c(1,2))
stripchart(r.b~rev.block,vertical=T , pch=16, xlab='Block')
plot(r.b~rev.block,xlab='Block')

fligner.test(r.b,rev.block)

# 2) Normality of Residuals
par(mfrow=c(1,2))
hist(r.b,main='Histogram of Residuals')
qqnorm(r.b) ; qqline(r.b)

shapiro.test(r.b)

# 3) Constant Mean and Independence

par(mfrow=c(1,2)) 
plot(r.b) 
abline(h=0,lty=2) 
y.hat <- fitted(score.mod) 
plot(r.b~y.hat) 



### Q4 ###

# a)
set.seed(372)
x = runif(500,1,2)

# INT  = (y1, ..., y500)
INT = (1/(2*pi)) * exp( (-1/2) * ((x-2)^2))

# estimate for B
b.hat = mean(INT)

# std dev
stdev.hat = sqrt(var(INT))

# 95% CI
ci.lower = b.hat - (1.96)*( (stdev.hat) / sqrt(500) )
ci.upper = b.hat + (1.96)*( (stdev.hat) / sqrt(500) )


# b)
# we want length = l of 2l <= 0.002
# so:
# l = c * sqrt(1 - n/N) * (stdev / sqrt(n))
# where c = 1.96, l = 0.001, we will ignore 1/N since it will be very small
#
# so n ~= (l^2 / (c^2 * stdev^2)) ^ (-1)

# stdev = sqrt( b.hat (1 - b.hat) )

# n = 453018.3
samp.stdev = sqrt( b.hat * (1 - b.hat) )
n.samp = ( (0.001)^2 / ((1.96)^2 * (samp.stdev)^2) )^(-1)

# n = 1314 (prob correct answer)
n = ( (0.001)^2 / ((1.96)^2 * (stdev.hat)^2) )^(-1)


### TESTS ###
set.seed(372)
x.test = runif(1314,1,2)

# INT  = (y1, ..., y500)
INT.test = (1/(2*pi)) * exp( (-1/2) * (x.test-2)^2)

# estimate for B
b.hat.test = mean(INT.test)

# std dev
stdev.hat.test = sqrt(var(INT.test))

# 95% CI
ci.lower.test = b.hat.test - (1.96)*(stdev.hat.test / sqrt(1314))
ci.upper.test = b.hat.test + (1.96)*(stdev.hat.test / sqrt(1314))
ci.upper.test - ci.lower.test

