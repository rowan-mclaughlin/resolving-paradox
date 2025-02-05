                  ####################################                     ︵
                 # Hardyland: a stochastic simulation #                  /   \
##################    by T. R. McLaughlin 2023-5      ################  |() ()|
                 #      rowan.mclaughlin.mu.ie        #                  \ | /
                  ####################################                    |=|

  rm(list = ls())
set.seed(653)

# Reshape population each year. Birth is the annual birthrate PER WOMAN
# infant is denominator of odds ratio of infant mortality (
year<-function(Xin,birth=0.1,infant=3,mage=17){
  # Bring out the dead
  X<-Xin[which(Xin$live),]
  X$age<-X$age+1
  # Trim the population and pair off the unmarried
  for(N in which(X$live)) {
    # Infant mortality: specify odds ratio of dying before 5
    if(X$age[N]==5 & sample(c(TRUE,rep(FALSE, infant)),1)) X$live[N]=FALSE #:_(

    # Overall mortality
    if(X$age[N] > X$expect[N]) X$live[N]<-FALSE # :-(

    # 'Marry' the women at mage to men over mage
    if(is.na(X$partner[N]) & X$sex[N]=='F' & X$age[N]>=mage){
      bachs<-X[which(X$sex=='M' & is.na(X$partner) & X$live & X$age>=mage),]
      luckyguy<-sample(bachs$id,1)
      X$partner[N]<-luckyguy
      X[X$id==luckyguy,'partner']<-X$id[N]
    }
  }

  # Birth a new child to the proportion of 'married' women under 45
  potential_mothers<-X[which(!is.na(X$partner) & X$age<45 & X$age>=mage & X$sex=='F' & X$live==TRUE),]
  Nmothers<-round(nrow(potential_mothers)*birth)

  if(Nmothers > 0) {
    mother_ids<-sample(potential_mothers$id,Nmothers)
    mothers<-X[X$id %in% mother_ids,]
    for(M in 1:nrow(mothers)){
      father <- X[which(X$id==mothers$partner[M]),]
      #print(M)
      baby <- data.frame(id=max(X$id)+1,
                       live=TRUE,
                       mother=mothers$id[M],
                       father=mothers$partner[M],
                       partner=NA,
                       ancestry=mean(c(mothers$ancestry[M],father$ancestry)),
                       sex=sample(c('M','F'),1),
                       age=0,
                       expect=rnorm(1,50,10))
      #print(baby)

      latest_baby <<- baby; latest_mother <<- mothers[M,]; latest_father <<- father

      if(anyNA(baby$ancestry)) warning(paste('Ancestry not found'))

      X<-rbind(X,baby) # :-)

    }
  }

  return(X)
}

# Initial pairing off
pairoff<-function(X,mage=17){
  FID<-which(X$sex=='F' & X$age>=mage)
  MID<-X[X$sex=='M' & X$age>=mage,'id']
  Ms<-sample(MID,length(FID),replace=TRUE)
  # Some males might have >1 partner especially if sex ratio is skewed that way
  X[FID,'partner']<-Ms
  return(X)
}

# Make an initial population
makepop<-function(N, ancestry, startid=1, mean_age=0, sd_age=20, mean_expect=50, sd_expect=10){
  pairoff(
    data.frame(id=startid:(N+startid-1),
                     live=rep(TRUE,N),
                     mother=NA,
                     father=NA,
                     partner=NA,
                     ancestry=rep(ancestry,N),
                     sex=sample(c('M','F'),N,replace=TRUE),
                     age=abs(rnorm(N,mean=mean_age,sd=sd_age)),
                     expect=rnorm(N,mean_expect,sd_expect)) )
}

# Derive annualized growth rate in %
gr<-function(x) (diff(x)/x[-1])*100

################################################################################
##############          Simulation experiment      #############################
################################################################################

# Generate starter populations
set.seed(531)
neo<-makepop(60,1)
meso<-makepop(100,-1,startid=10000)

# Initialize variables tracked during the simulation
pop_m <- pop_n <- births_m <- births_n <- c()
anc<-list()

#plot(1:Y,(births_m/pop_m)*1000);plot(1:Y,(births_n/pop_n)*1000)

# Run the ABM

pb <- txtProgressBar(initial=1, max=400)
for(Y in 1:700)  {
  # Allow the populaitons to do their things
  if(Y<450) neo<-year(neo, birth=0.096, infant=11) #0.096 is about
  else neo<-year(neo, birth=max(0.096-((Y-400)/1000),0.06), infant=11) # NB delta on birth for these years
  meso<-year(meso, birth=0.075, infant=11)

  # Save population numbers, birthrates and ancestry profiles for post-hoc analysis
  pop_n[Y]<-sum(neo$live); pop_m[Y]<-sum(meso$live)
  births_n[Y]<-sum(neo$age==0); births_m[Y]<-sum(meso$age==0)
  anc[[Y]]<-c(neo[which(neo$live),'ancestry'],meso[which(meso$live),'ancestry'])

  # The Mesolithic population begins to join the Neolithic when the Neolithic becomes twice its size
  if(pop_n[Y] > 2*pop_m[Y])
    num_deserters<-round(nrow(meso)*0.01) else num_deserters<-0
  if(num_deserters>0){
     deserter_ids <- sample(meso[which(meso$live),'id'],num_deserters)
     # Divorce deserters
     meso[which(meso$id %in% deserter_ids),'partner']<-NA
     neo<-rbind(neo, meso[which(meso$id %in% deserter_ids),])

     # Remove from Mesolithc pop
     meso<-meso[-which(meso$id %in% deserter_ids),]

     # Give new ID to the deserters
     neo[(nrow(neo)-1+length(deserter_ids)):nrow(neo),'id']<-max(neo$id)+c(1:(num_deserters))
  }
  setTxtProgressBar(pb, Y)
}
close(pb)


# Post-hoc analysis

# Meso ancestry is -1, neo is +1 so re-scale mean between 0 and 100% of mesolithic
rescale_anc<-function(x) 100 - ((x+1)/2)*100
anc<-lapply(anc, rescale_anc)

# Changing ancestry with time
mean_anc<-sapply(anc, mean)


# Probablitiy of detecting ancestry over time
probm5 <- probm10 <- probm20 <- probm50 <- probm100 <- c()
for(i in 1:Y) {
  probm5[i]  <- sum(anc[[i]] > 5) / length(anc[[i]])
  probm10[i] <- sum(anc[[i]] > 10) / length(anc[[i]])
  probm20[i] <- sum(anc[[i]] > 20) / length(anc[[i]])
  probm50[i] <- sum(anc[[i]] > 50) / length(anc[[i]])
  probm100[i] <- sum(anc[[i]] == 100) / length(anc[[i]])
}

# Function to make a dot in a circle
ppoint<-function(x,y,col=1,cex=1){
  P<-c(19,1)
  X<-c(0.75,2)
  for(i in 1:2) points(x,y,pch=P[i],cex=X[i]*cex,col=col)
}

# Multiplot
#pdf('~/Dropbox/articles/Danish_shells/Fig8b.pdf',height=6,width=6.25)
par(omi=c(1,0.6,0.5,1),mar=c(0.2,0.8,0.8,0.1),mfcol=c(2,2))

plot( 1:Y,pop_n,type='l',lwd=2.5,col='#FF880099',xlab=NA,ylab=NA,ylim=c(0,1200),xaxt='n')
axis(3,at=seq(100,700,200),lab=seq(4000,3400,-200)); rug(seq(0,600,200),side=3,ticksize = -0.03)
mtext(side=2,'Population size',line=3,cex=0.9)
lines(1:Y,pop_m,lwd=2.5,col='#0044AA99')
lines(1:Y,pop_m+pop_n,lty=2,col=1)
legend('topleft',lty=1,lwd=2,col=c('#FF880099','#0044AA99'),legend=c('Neolithic','Mesolithic'),bty='n',xjust=1)
legend('topright',lty=c(2),legend=c('Total\npop.'),bty='n')

plot( 2:Y,runmed(gr(pop_n),5),type='l',lwd=.5,col='#FF880099',xlab=NA,ylab=NA,ylim=c(-1,1))
mtext(side=2,'Growth rate (%)',line=3,cex=0.9)
lines(2:Y,runmed(gr(pop_m),5),lwd=.5,col='#0044AA99')
#lines(2:Y,runmed(gr(pop_m+pop_n),101),lty=2,col=1)
ppoint(200,0.4,'#FF8800'); text(250,0.58,'0.4%',col='#FF8800')
ppoint(200,0,'#0044AA'); text(250,-0.2,'0%',col='#0044AA')

plot(1:Y, mean_anc, type='l',yaxt='n',ylab=NA, xlab=NA,lwd=1,ylim=c(0,160),xaxt='n')
lines(1:Y, (mean_anc/100)*(pop_m+pop_n), lty=2)
axis(4)
axis(3,at=seq(100,700,200),lab=seq(4000,3400,-200)); rug(seq(0,600,200),side=3,ticksize = -0.03)
mtext(side=4,'% Mesolithic\nresources exploited',line=3.5,cex=0.9)
legend('topright',lty=c(1,2),c('Per agent','In total'),bty='n')

plot(1:Y, probm5, type='l',ylab=NA,yaxt='n', xlab=NA,lwd=1,ylim=c(0,1))
axis(4)
mtext(side=4,'Prob. detecting\nMesolithic ancestry',line=3.5,cex=0.9)
lines(1:Y, probm10, type='l',col=2,lty=2,lwd=1)
lines(1:Y, probm20, type='l',col=3,lty=3,lwd=1)
lines(1:Y, probm50, type='l',col=4,lty=4,lwd=1)
#lines(1:Y, probm100, type='l',col=5,lty=5,lwd=2) very similar to 50%

text(c(265,350,300,40),c(0.92,0.5,0.25,0.1),c('>5%','>10%','>20%','>50%'),col=c(1,2,3,4))
mtext('Year of simulation',side=1,outer=TRUE,line=3,cex=0.9)

#dev.off()
stop()

#     &&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&
#     @&&&&&&&&&&&&&&&&%&&%%%%&&&&&&&&&&&&&&&&&&&&&&&&&
#     @@&&&&&&&&&&&@#//*,,,/#(#%&&&&&&&&&&&&&&&&&&&&&&&
#     @@&&&&&&&&&/*,,,,.... .,.....(&&@&%%&&&&&&&&&&&&&
#     &&&&&&&@@#(/**,,.....        . /&@&%%%%%%&&&&&&&&
#     &&&&&&&@%#(//***,,,........  . ./&@&%%%%%%&&&&&&&
#     &&&&%&@&&%(/*,,.......   .  ...,/%@@%%%%%%&&&&&&&
#     %%%%%&@&&%%&%#/,,,,,,,,*(#(/,..,*&&@&%%%%%%%%%&&&
#     %%###%@&&@&%#%&&&%*,(%%%%#/*/*..,%&@@%%%%%%%%%&&&
#     #####@@&&%&#/(//#%, .*/((,,......#&##@%%%%%%%&&&&
#     ###((&@@%#//*,,,*#,  .   ..  ..,*%#@((%%%%%%%%&&&
#     (((((/&@&%%(*,,(*/,...*,...**,**&&,/,%%%%%%%&&&&&
#     ///////@@&%#(/(#%@@@@%..*,.,**(#&##%%%%%%%%%%%&&&
#     ////***%@&&##&%&&@&@&%(#((**(#&&&##%%%%%%%%%%&&&&
#     //******%&&%&&&&%(//(%&@@&&(&&@&####%%%%%%%%%&&&&
#     //*******&&&@&&%%##%#(%&&@@&&@&#####%%%%%%%%&&&&&
#     //*****@@@&&&%#%%&#/%(#&&@@@@%/&####%%%%%%%&&&&&&
#     /**%@@@@@&&&&&&&&%#%&%&&&@@&#  @@%##%%%%%%%%&&&&&
#     @@@@@@@&&&&&&&#/#&&&%&&&&#(   (@@@&#####%%%%%&&&&
#     @@@@@&&&&&&&&&&@#///&&%#/   ,/@@@@@@&#####%%%&&&&
#     &&@@@@&@&&&&&@@@@@%##(//*/#*/*@@@@@@@@@@##%%%%&&&
#     &&&@@@&@@&@&&&@@@@@@#(/***%%*@&&@&@&&&@@@@@&%%%%%
#     @@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&@@@@@@
#     @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&@@@@@
#
# I shall do one thing in this life - one thing certain -
#                                             -- T. Hardy


# Generate starter populations
set.seed(531)
neo<-makepop(60,1)
meso<-makepop(200,-1,startid=10000)

# Initialize variables tracked during the simulation
pop_m <- pop_n <- births_m <- births_n <- c()
anc<-list()

#plot(1:Y,(births_m/pop_m)*1000);plot(1:Y,(births_n/pop_n)*1000)

# Run the ABM

pb <- txtProgressBar(initial=1, max=400)
for(Y in 1:700)  {
  # Allow the populaitons to do their things
  if(Y<450) neo<-year(neo, birth=0.096, infant=11) #0.096 is about
  else neo<-year(neo, birth=max(0.096-((Y-400)/1000),0.06), infant=11) # NB delta on birth for these years
  meso<-year(meso, birth=0.075, infant=11)

  # Save population numbers, birthrates and ancestry profiles for post-hoc analysis
  pop_n[Y]<-sum(neo$live); pop_m[Y]<-sum(meso$live)
  births_n[Y]<-sum(neo$age==0); births_m[Y]<-sum(meso$age==0)
  anc[[Y]]<-c(neo[which(neo$live),'ancestry'],meso[which(meso$live),'ancestry'])

  # The Mesolithic population begins to join the Neolithic when the Neolithic becomes twice its size
  if(pop_n[Y] > 2*pop_m[Y])
    num_deserters<-round(nrow(meso)*0.01) else num_deserters<-0
  if(num_deserters>0){
    deserter_ids <- sample(meso[which(meso$live),'id'],num_deserters)
    # Divorce deserters
    meso[which(meso$id %in% deserter_ids),'partner']<-NA
    neo<-rbind(neo, meso[which(meso$id %in% deserter_ids),])

    # Remove from Mesolithc pop
    meso<-meso[-which(meso$id %in% deserter_ids),]

    # Give new ID to the deserters
    neo[(nrow(neo)-1+length(deserter_ids)):nrow(neo),'id']<-max(neo$id)+c(1:(num_deserters))
  }
  setTxtProgressBar(pb, Y)
}
close(pb)


# Post-hoc analysis

# Meso ancestry is -1, neo is +1 so re-scale mean between 0 and 100% of mesolithic
rescale_anc<-function(x) 100 - ((x+1)/2)*100
anc<-lapply(anc, rescale_anc)

# Changing ancestry with time
mean_anc<-sapply(anc, mean)


# Probablitiy of detecting ancestry over time
probm5 <- probm10 <- probm20 <- probm50 <- probm100 <- c()
for(i in 1:Y) {
  probm5[i]  <- sum(anc[[i]] > 5) / length(anc[[i]])
  probm10[i] <- sum(anc[[i]] > 10) / length(anc[[i]])
  probm20[i] <- sum(anc[[i]] > 20) / length(anc[[i]])
  probm50[i] <- sum(anc[[i]] > 50) / length(anc[[i]])
  probm100[i] <- sum(anc[[i]] == 100) / length(anc[[i]])
}

# Function to make a dot in a circle
ppoint<-function(x,y,col=1,cex=1){
  P<-c(19,1)
  X<-c(0.75,2)
  for(i in 1:2) points(x,y,pch=P[i],cex=X[i]*cex,col=col)
}

# Multiplot
#pdf('~/Dropbox/articles/Danish_shells/Fig8b.pdf',height=6,width=6.25)
par(omi=c(1,0.6,0.5,1),mar=c(0.2,0.8,0.8,0.1),mfcol=c(2,2))

plot( 1:Y,pop_n,type='l',lwd=2.5,col='#FF880099',xlab=NA,ylab=NA,ylim=c(0,1200),xaxt='n')
axis(3,at=seq(100,700,200),lab=seq(4000,3400,-200)); rug(seq(0,600,200),side=3,ticksize = -0.03)
mtext(side=2,'Population size',line=3,cex=0.9)
lines(1:Y,pop_m,lwd=2.5,col='#0044AA99')
lines(1:Y,pop_m+pop_n,lty=2,col=1)
legend('topleft',lty=1,lwd=2,col=c('#FF880099','#0044AA99'),legend=c('Neolithic','Mesolithic'),bty='n',xjust=1)
legend('topright',lty=c(2),legend=c('Total\npop.'),bty='n')

plot( 2:Y,runmed(gr(pop_n),5),type='l',lwd=.5,col='#FF880099',xlab=NA,ylab=NA,ylim=c(-1,1))
mtext(side=2,'Growth rate (%)',line=3,cex=0.9)
lines(2:Y,runmed(gr(pop_m),5),lwd=.5,col='#0044AA99')
#lines(2:Y,runmed(gr(pop_m+pop_n),101),lty=2,col=1)
ppoint(200,0.4,'#FF8800'); text(250,0.58,'0.4%',col='#FF8800')
ppoint(200,0,'#0044AA'); text(250,-0.2,'0%',col='#0044AA')

plot(1:Y, mean_anc, type='l',yaxt='n',ylab=NA, xlab=NA,lwd=1,ylim=c(0,160),xaxt='n')
lines(1:Y, (mean_anc/100)*(pop_m+pop_n), lty=2)
axis(4)
axis(3,at=seq(100,700,200),lab=seq(4000,3400,-200)); rug(seq(0,600,200),side=3,ticksize = -0.03)
mtext(side=4,'% Mesolithic\nresources exploited',line=3.5,cex=0.9)
legend('topright',lty=c(1,2),c('Per agent','In total'),bty='n')

plot(1:Y, probm5, type='l',ylab=NA,yaxt='n', xlab=NA,lwd=1,ylim=c(0,1))
axis(4)
mtext(side=4,'Prob. detecting\nMesolithic ancestry',line=3.5,cex=0.9)
lines(1:Y, probm10, type='l',col=2,lty=2,lwd=1)
lines(1:Y, probm20, type='l',col=3,lty=3,lwd=1)
lines(1:Y, probm50, type='l',col=4,lty=4,lwd=1)
#lines(1:Y, probm100, type='l',col=5,lty=5,lwd=2) very similar to 50%

text(c(265,350,300,40),c(0.92,0.5,0.25,0.1),c('>5%','>10%','>20%','>50%'),col=c(1,2,3,4))
mtext('Year of simulation',side=1,outer=TRUE,line=3,cex=0.9)
