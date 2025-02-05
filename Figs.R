# R code to create figures for the paper :

#                  Marine exploitation and the arrival of farming:
#       resolving the paradox of the Mesolithic-Neolithic transition in Denmark

##############################################################
# Settings for all plots
ax<-function(side=1, tick=100, ticksize=-0.01, labs=NULL, Toffset=0) {
  ats<-pretty(par('usr')[1:2])
  if(is.null(labs)) labs<-c(abs(ats[which(ats< -1)]),ats[which(ats>-1)]+1)
  axis(side, at=ats+Toffset,lab=labs)
  rug(seq(ats[1]-500,ats[length(ats)]+500,tick),ticksize = ticksize, side=side,quiet = TRUE)
}
#Expressions for y-axis labels
d13Cpm<-expression(delta * ""^{13} * "C (‰)")
d15Npm<-expression(delta * ""^{15} * "N (‰)")

#divline<-function(x=-3900) for(i in 1:30) abline(v=x,lwd=i,col='#FF440004')
divline<-function(x0=-4100,x1=-3900,col='#FFC56640') {
  yw<-par('usr')[3:4]
  graphics::polygon(c(x0,x0,x1,x1),c(yw[1],yw[2],yw[2],yw[1]),border=NA,col=col)
}
##############################################################

# Figure 1

# Figure 1b (1a in GIS)

pdf('~/Dropbox/articles/Danish_shells/Fig1b.pdf', width=7.87, height=7.87)
par(mfrow=c(3,1),mar=c(0,4,0,4),omi=c(0.9,0,0.5,0),cex=1)

plot(dk_kde, axes=FALSE, xlim=c(-6000,-2800), ylim=c(-0.00002,0.00065),
     frame.plot=FALSE,ylab='Density')
plot(dk_spd, add=T, type='l',col='#00000066')
ax(side=3, ticksize = -0.02)
axis(2); abline(v=seq(-6000,-3000,1000),col='#00000033');divline()
abline(h=par('usr')[3])
legend('topleft',lty=c(NA,1,1),col=c(NA,'#00000066',1),legend=c(paste0('Shell middens, N=',nrow(dk)),'SPD','KDE'),bty='n')
#polygon(ggrsignif(ggr(dk_kde)))
plot(dk_natural_shell_kde, axes=FALSE, xlim=c(-6000,-2800),
     frame.plot=FALSE,ylab='',ylim=c(-0.00005,0.0008))
plot(dk_natural_shell_spd, add=T, type='l',col='#00000066')
axis(4); abline(v=seq(-6000,-3000,1000),col='#00000033');divline()
abline(h=par('usr')[3])
legend('topleft',lty=c(NA,1,1),col=c(NA,'#00000066',1),
  legend=c(paste0('Natural shell banks, N=',nrow(dk_natural_shell)),'SPD','KDE'),bty='n')
#polygon(ggrsignif(ggr(dk_natural_shell_kde)))
plot(nonSMD,xlim=c(-6000,-2800), xaxt='n', ylim=c(-0.00002,0.0003),lty=2,
     frame.plot=FALSE,ylab='Density')
plot(ALLD, add=T )
abline(h=par('usr')[3]); abline(v=seq(-6000,-3000,1000),col='#00000033');divline()
legend('topleft',lty=c(1,2),legend=c('All archaeological sites KDE, N=1610','(without shell middens, N=1088)'),bty='n')
#polygon(ggrsignif(ggr(nonSMD)))
ax()
mtext(side=1, outer=TRUE, line=2.5, 'BCE')

dev.off()



##############################################################

# Figure 2a and c

lewis<-read.csv('~/Dropbox/articles/Danish_shells/Lewis_et_al_spd.csv')
oy<-rowcalsum(dk_data[which(dk_data$Species=='Ostrea edulis'),cID[2:3]])
oyspd<-rowcalsum(dk_data[which(dk_data$Species=='Ostrea edulis'),cID[2:4]])

pdf('~/Dropbox/articles/Danish_shells/Fig2a_thickerLewis.pdf',width=7.87, height=7.87/2)
par(mfrow=c(1,1),mar=c(1,4.1,4.1,4.1),omi=c(0,0,0,0))
plot(dk_kde, xaxt='n', xlim=c(-6000,-2800),
     ylab='Density',xlab='',ylim=c(0,0.0008))
#plot(dk_spd, add=T, type='l', col='#00000066')
divline()
ax(side=3,ticksize = -0.02)
lines(lewis[,1],lewis[,2]/1600,col='#00AAFF',lty=3,lwd=3)
plot(oyspd, add=T, col='#66996688',type='l',lwd=1.5)
legend('topleft',lty=c(1,1,3),lwd=c(1,1.5,3),col=c(1,'#66996688','#00AAFF'),
       legend=c('All middens KDE','Oyster SPD','Lewis et al oyster SPD'),bty='n')

dev.off()

pdf('~/Dropbox/articles/Danish_shells/Fig2c.pdf',width=7.87, height=7.87)
par(mfrow=c(3,1),mar=c(0.1,4.1,0,4.1),omi=c(0.5,0,0.5,0),cex=1)

plot(ar.as.MCd(allres),xlim=c(-6000,-2800),xaxt='n',col='#005500',
     fill='#00550033',ylab='R',ylim=c(0.4,1.4))
abline(h=1,lty=2);ax(side=3, ticksize = -0.02)
legend('topright',col=c('#005500'),pch=NA,'Clark-Evans', bty='n')
#polygon(ggrsignif(ggr(dk_kde)))
divline()

plot(ar.as.MCd(allmeddist, mult=0.001),xlim=c(-6000,-2800),xaxt='n',
     ylim=c(50,130),col='#005500',fill='#00550033',ylab='',yaxt='n')
axis(4)
legend('topright',col=c('#005500'),pch=NA,'Mean inter-site distance', bty='n')
#polygon(ggrsignif(ggr(dk_kde)))
divline()

plot(ar.as.MCd(allnndist, mult=0.001),xlim=c(-6000,-2800),xaxt='n',
     col='#005500',fill='#00550033',ylim=c(5,35),ylab='Distance (km)' )
legend('topright',col=c('#005500'),pch=NA,'Distance to nearest site', bty='n')
ax(ticksize = -0.02)
#polygon(ggrsignif(ggr(dk_kde)))
divline()

mtext(side=1, outer=TRUE, line=2.5, 'BCE', cex=0.75)
mtext(side=4, outer=TRUE, line=-1.33, 'Distance (km)', cex=1)

dev.off()



# Figure 2b
library(Cairo)
cairo_pdf('~/Dropbox/articles/Danish_shells/Fig2b.pdf', height=4, width=6)

par(mfrow=c(1,3),mar=c(4.1,0,2,0.1),omi=c(0.1,0.3,0,0))
plot(smrD,ribside='bottom',clipwin=as.owin(dkc),addcontour=TRUE,
     main='All sites',col=hcl.colors(128,'Zissou 1'),
     contourargs=list(col='#FFBBFF88', drawlabels=FALSE))
axis(2, at=c(0,100,200)+6100, lab=c(0,100,'200 km'))
plot(sitD,ribside='bottom',clipwin=as.owin(dkc),addcontour=TRUE,
     main='Dated sites',col=hcl.colors(128,'Zissou 1'),
     contourargs=list(col='#FFBBFF88', drawlabels=FALSE))
plot(c14D,ribside='bottom',clipwin=as.owin(dkc),addcontour=TRUE,
     main='All dates',col=hcl.colors(128,'Zissou 1'),
     contourargs=list(col='#FFBBFF88', drawlabels=FALSE))
L<-c(rep(expression("Sites / km"*""^{2}),2),expression("Dates / km"*""^{2}))
mtext(L,outer=TRUE, side=1, adj=c(0.15,0.5,0.85),cex=0.75, line=-2)

dev.off()


##############################################################

# Fig 3a : Absolute MAU
# Study moving average of total MAU
mMAU <- function(corename){
  core<-MAU[which(MAU$Core==corename),]
  runmed_MAU<-MCrunmed(x=rowunif(x0=-core$`Est Start (BC)`,
                                 x1=-core$`Est End (BC)`),
                       y=core$`Total MAU`,k=9, N=500, y_blur=0.5, boot=FALSE)
  out<-MCr.as.MCd(runmed_MAU)
  attr(out,'N')<-nrow(core)
  return(out)
}

N77_MAU<-mMAU("Norsminde N77")
K37_MAU<-mMAU("Krabbesholm II 7737")
K38_MAU<-mMAU("Krabbesholm II 7738")
KNi_MAU<-mMAU("Krabbesholm II Nielsen")

pdf('~/Dropbox/articles/Danish_shells/Fig3a.pdf', width=6, height=4)
par(mfrow=c(2,2), mar=c(3,4,1,1), omi=c(0.2,0,0,0))
plot(N77_MAU, xlim=c(-5000,-2800), xaxt='n',ylim=c(500,2200),ylab='MAU');ax()
legend('topleft',pch=NA,bty='n',
       paste0("Norsminde N77\nTotal=",attr(N77_MAU,'N')))
divline()
plot(K37_MAU, xlim=c(-5000,-2800), xaxt='n',ylim=c(0,80),ylab='MAU');ax()
legend('topleft',pch=NA,bty='n',
       paste0("Krabbesholm II 7737\nTotal=",attr(K37_MAU,'N')))
divline()
plot(K38_MAU, xlim=c(-5000,-2800), xaxt='n', ylim=c(0,310),ylab='MAU');ax()
legend('topleft',pch=NA,bty='n',
       paste0("Krabbesholm II 7738\nTotal=",attr(K37_MAU,'N')))
divline()
plot(KNi_MAU, xlim=c(-5000,-2800), xaxt='n', ylim=c(0,50),ylab='MAU');ax()
legend('topleft',pch=NA,bty='n',
       paste0("Krabbesholm II Nielsen\nTotal=",attr(KNi_MAU,'N')))
mtext('BCE',side=1, outer=TRUE,line=0, adj=0.52)
divline()
dev.off()

pdf('~/Dropbox/articles/Danish_shells/Fig3b.pdf', width=6, height=4)
par(mfrow=c(1,1),mar=c(4,4,2,6),omi=c(0,0,0,0))
plot(oyster_mau,xaxt='n', xlab='BCE', xlim=c(-5000,-2800),col='#014F43',
     fill='#014F4322',lwd=1.5, ylab='% MAU / % maximum KDE')
ax()
divline()
plot(mussel_mau, add=T,col='#633AAA',fill='#633AAA22',lwd=1.5)
plot(pwink_mau, add=T,col='#AA6600',fill='#AA660022',lwd=1.5)
plot(cockle_mau, add=T,col='#556B2F',fill='#556B2F22',lwd=1.5)
plot(other_mau, add=T, col='#BBBBBB',fill='#BBBBBB22',lwd=1.5)
text(-4800,c(85,12),c('Oyster','Cockle'))
axis(4, at=c(42,27,17),lab=c('Periwinkle','Other','Mussel'),las=2,
     tick=FALSE,line=-0.4)
plot(dk_natural_shell_kde,add=T, fill=NA,smax=TRUE, scalefactor = 100,lty=2)
legend(-4200,100,legend='Natural shell banks\nradiocarbon KDE',lty=2,bty='n')
dev.off()



##############################################################
# Figure 4: Fish NISP etc

cairo_pdf('~/Dropbox/articles/Danish_shells/Fig4.pdf',width=10.5, height=6)

par(mfcol=c(2,3),mar=c(1,4.1,0,1),omi=c(0.7,0.12,0.7,0.1),xpd=FALSE)

plot(leisD, xlim=c(-6200,-2800), lty=2, col='#FF8080', fill='#FF808022',
     xaxt='n',ylab='Kernel density estimate')
plot(fingD, add=TRUE, col='#baba00', fill='#baba0022')
plot(dk_kde, add=T, fill='#00000010')
ax(side=3,ticksize = -0.015)
legend('topleft',lty=c(2,1,1,3),col=c('#FF8080','#baba00',1,1),bty='n',
       legend=c('Eel leisters','Weirs & traps','Shell middens','Other sites'))
plot(nonSMD, add=T, lty=3)
divline()

plot(log(nisp_fish),yaxt='n', ylim=c(0,10),col='#00AAB0',fill='#00AAB022',
     ylab='log ( NISP )',xaxt='n',xlim=c(-6200,-2800));ax()
axis(2, at=log(c(1,5,10,50,100,500,1000,5000,10000)),
     lab=c(1,NA,10,NA,100,NA,1000,NA,10000) ) # Log axis
plot(log(nisp_om),add=T,col='#B00B13',fill='#B00B1350')
plot(log(nisp_mm),add=T,col='#0044B0',fill='#0044B022')
legend('topright',lty=1,lwd=1.5,col=c('#00AAB0','#B00B13','#0044B0'),
       legend=c('Fish','Other mammals','Marine mammals'),bg='white',box.col='white')
ax(ticksize = -0.015)
divline()


plot(frich,ylab='N taxa / site',xaxt='n',xlim=c(-6200,-2800), ylim=c(0,14),
     col='#00AAB0',fill='#00AAB022')
plot(mrich,add=TRUE,col='#0044B0',fill='#0044B022')
legend('topright',lty=1,lwd=1.5,col=c('#00AAB0','#0044B0'),
       legend=c('Fish richness','Marine mammal\nrichness'),bty='n')
ax(side=3,ticksize = -0.015)
divline()


plot(ratio, ylab='NISP ratio',xaxt='n',xlim=c(-6200,-2800),ylim=c(0,0.125),
     col='#582862',fill='#58286222')
plot(mfratio, add=TRUE, col='#0077B0',fill='#0077B022')
legend('topleft',lty=1,col=c('#00AAB0','#582862'),bty='n',
       legend=c('Marine mammals : fish','Marine mammals : all mammals'))
ax(ticksize = -0.015)
divline()


mtext('BCE',side=1,outer=TRUE,line=2.5,adj = 0.54)
mtext(c('Finds density','Assemblage composition','Stable isotopes'), side=3,
      outer=TRUE, line=3, adj=c(0.17,0.53,0.90))

plot(Ct36,lty=2,ylab=d13Cpm,xaxt='n',xlim=c(-6200,-2800),
     col='#00AAB0',fill='#00AAB022', ylim=c(-15,-7))
plot(Ct32, add=T,lty=3,col='#00AAB0',fill='#00AAB022' )
plot(Ct41, add=T,col='#00AAB0',fill='#00AAB022')
divline()
ax(side=3,ticksize = -0.015)

plot(Nt36,lty=2,ylab=d15Npm,xaxt='n',xlim=c(-6200,-2800),
     col='#00AAB0',fill='#00AAB022', ylim=c(5.5,13.5))
plot(Nt41, add=T,col='#00AAB0',fill='#00AAB022')
ax(ticksize = -0.015)
divline()
plot(Nt32, add=T, lty=3, col='#00AAB0',fill='#00AAB022')


par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend(0.63,0.12,col=c(rep('#00AAB0',2)),lwd=1.5,lty=c(3,2,1),
       c('Trophic level 3.2','Trophic level 3.6','Trophic level 4.1'),bg='white')


dev.off()


##########################################################
# Figure 5


# Pollen diagram style visualisation of the fish data
# saltwater fish and freshwater fish
SWF<-c("Gadidae","Lotidae","Pleuronectidae","Scopthalmidae","Pleuronectidae/Scopthalmidae","Congridae","Trachinidae","Belonidae","Clupeidae","Scombridae","Cottidae","Syngnathidae","Squalidae","Zoarcidae","Triglidae","Sparidae","Labridae","Carangidae","Gobiidae","Moronidae","Anarhichadidae","Callionymidae","Ammodytidae","Xiphiidae","Lamnidae","Triakidae","Rajidae","Mugilidae","Engraulidae")
MgF<-c("Anguillidae","Salmonidae","Acipenseridae","Gasterosteidae")
FWF<-c("Cyprinidae","Percidae","Esocidae","Siluridae")
# More frequent taxa:
gSWF<-c("Gadidae","Pleuronectidae","Trachinidae","Belonidae","Clupeidae","Scombridae","Cottidae","Squalidae","Lamnidae")
gMgF<-c("Anguillidae","Salmonidae","Gasterosteidae")

cleancol<-function(x,j){
  out<-c()
  for(i in 1:nrow(x))
    if(is.null(x[,j][[i]])) out[i]<-NA else
      out[i]<-as.numeric(x[,j][[i]])
    return(out)
}

# Backup and clean dataframe and convert to % of NISP
fNISPbu<-fNISP
for(i in 1:ncol(fNISP)) fNISP[,i]<-cleancol(fNISP,i)
for(N in c(SWF,MgF,FWF)) fNISP[,N]<-(fNISP[,N]/fNISP$NISP)*100

#Order for plot
O<-order(fNISP$`Median age (BC)`)

require(rioja)

pdf('~/Dropbox/articles/Danish_shells/Fig5_raw.pdf',width=10)

par(omi=c(0.2,0.1,0.1,0.8))
strat.plot(fNISP[O,c(gSWF,gMgF)],yvar=fNISP[O,'Median age (BC)'],y.rev=TRUE,
           scale.percent=TRUE, ylabel="", plot.line=FALSE,
           col.bar=c(rep("#0099AA",9),rep('#0000AA',3)),
           lwd.bar=5, x.axis=TRUE, srt.xlabel=45, cex.xlabel=0.7,
           x.pc.lab = TRUE, las.xaxis=2)

dev.off()

###########################################################
# Figure 6

require(SIBER)
# Correct bug in SIBER that blocks changing colours of ellipses

plotGroupEllipses<- function(siber, iso.order = c(1, 2), col, ...) {
  x <- iso.order[1]
  y <- iso.order[2]
  for (i in 1:siber$n.communities) {
    for (j in 1:siber$n.groups[2, i]) {
      do.call("addEllipse", c(list(mu = siber$ML.mu[[i]][, c(x, y), j],
      sigma = siber$ML.cov[[i]][c(x, y),  c(x, y), j]),
      m = siber$sample.sizes[i, j], col=col[j], ...))
    }
  }
}

# Coords
smcrd<-hiso[which(hiso$`Site type`=='Shell midden'),c('Longitude','Latitude')]
dkcrd<-hiso[which(hiso$`Site type`!='Shell midden'),c('Longitude','Latitude')]
library(sf)

# Coastline
dkc<-st_read("~/Dropbox/GISMisc/Denmark/Denmark_UTM_outline.shp")
smP<-st_as_sf(smcrd, coords=c('Longitude','Latitude'), crs=4326) |> st_transform(32632)
dkP<-st_as_sf(dkcrd, coords=c('Longitude','Latitude'), crs=4326) |> st_transform(32632)

cairo_pdf('~/Dropbox/articles/Danish_shells/Fig6.pdf',width=10.5, height=6)

par(mfcol=c(2,3), mar=c(1,4.5,0,1), omi=c(0.75,0,0.6,0))
options(scipen = 999)
plot(dk_iso_kde, xaxt='n', col='#1AFF1A', fill='#1AFF1A22', xlim=c(-6200,-2800),
     ylab='Kernel density estimate')
ax(side=3, ticksize = -0.015)
plot(sm_iso_kde, add=TRUE, col='#4B0092', fill='#4B009222')
plot(dk_kde, add=T, fill=NA, lty=2)
divline()
legend('topleft',lty=2,bty='n','All midden\ndates')

plot(dkc$geometry, xlim=c(440000,776500), ylim=c(6067000,6400000),border='grey50')
plot(dkP,pch=19,col='#1AFF1AAA',add=TRUE)
plot(smP,pch=19,col='#4B0092AA',add=TRUE)
legend('topright',pch=19,col=c('#4B0092','#1AFF1A'),legend=c('Shell midden\nhumans','Other humans'))
axis(1,at=seq(440000,540000,50000),lab=c(0,NA,'100 km'))

plot(dkN, xlim=c(-6200,-2800), ylim=c(7,18), xaxt='n',xlab='', col='#1AFF1A',
     fill='#1AFF1A22', lwd=1.5, ylab=d15Npm); ax(side=3, ticksize = -0.015)
plot(smN, add=TRUE, col='#4B0092', lwd=1.5, fill='#4B009222')
divline()
#text(c(-4450,-3200),8,c('Mesolithic','Neolithic'),col=2,cex=1.2)

plot(dkC, xlim=c(-6200,-2800), ylim=c(-21,-10),ylab=d13Cpm, xlab='',
     col='#1AFF1A',fill='#1AFF1A22', lwd=1.5, xaxt='n'); ax(ticksize = -0.015)
plot(smC, add=TRUE, col='#4B0092', fill='#4B009222', lwd=1.5)
divline()

plot(dk_iso[dk_iso$median > -3800,'d13C'],dk_iso[dk_iso$median > -3800,'d15N'],
     pch=22,bg='#1AFF1A',xlim=c(-22,-9.5),ylim=c(7,18.5),xlab=NA,
     ylab=d15Npm,xaxt='n')
axis(3)
points(dk_iso[dk_iso$median < -3800,'d13C'],dk_iso[dk_iso$median < -3800,'d15N'],
       pch=19,col='#1AFF1A')
points(sm_iso[sm_iso$median > -3800,'d13C'],sm_iso[sm_iso$median > -3800,'d15N'],
       pch=22,bg='#4B0092')
points(sm_iso[sm_iso$median < -3800,'d13C'],sm_iso[sm_iso$median < -3800,'d15N'],
       pch=19,col='#4B0092')
legend('bottomright',pch=c(19,19,22,22),col=c('#4B0092','#1AFF1A',1,1),
       pt.bg=c(NA,NA,'#4B0092','#1AFF1A'),legend=c('Meso. shell midden',
                                                 'Meso. other','Neo. shell midden','Neo. other'))
plotGroupEllipses(all_iso,p.interval=0.68,col=c('#1AFF1A','#4B0092'))

gc<-c(1,2,'#FF8800')
plot(gh$`δ13C VPDB (‰)`,gh$`δ15N AIR (‰)`,col=gc[as.numeric(key)],pch=19,cex=1.1,
     xlab=NA, ylab=d15Npm,xlim=c(-25,-7),ylim=c(4.5,24))
points(bos, pch=1,col='#00AA00')
points(cap, pch=1,col='#006600')
points(sus, pch=1,col='#AAAA00')
points(t3.2$d13C,t3.2$`δ15N (‰)`,pch=2,col='#004450')

legend('topright',legend=unique(key)[c(2,3,1)],pch=19,col=c(2,1,'#FF8800'),
       title='Genetic cluster',bty='n')
legend('topleft',legend=c('Bos','Sus','Caprinae','Fish (t.l. 3.2)'),pch=c(1,1,1,2),col=c('#00AA00','#006600','#AAAA00','#004450'),bty='n')

mtext('BCE',side=1,line=2.5,outer=TRUE,adj=0.54,cex=0.66)
mtext(d13Cpm,side=1,line=2.5,outer=TRUE,adj=0.88,cex=0.66)

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend('top', ncol=2, lty=1, lwd=c(1.5,1.5), col=c('#4B0092','#1AFF1A'),
       legend=c('Shell midden humans','Other humans'), bty='n')

dev.off()


#########################################################
cairo_pdf('~/Dropbox/articles/Danish_shells/Fig7.pdf',height=6.5)

ax2<-function() axis(1,at=seq(-4500,-2500,500),lab=-seq(-4500,-2500,500))
BD<-expression({13}*"C"*""[18:0]~-~delta*""^{13}*"C"*""[16:0])

par(mfrow=c(3,3),mar=c(2.3,4.6,2,0.4),omi=c(0.24,0.05,0.2,0),xpd=FALSE)
plot(ora_coastalD,col='blue',fill='#0011AA22', main='Temporal Density',
     ylab='Kernel density estimate',xlim=c(-4500,-2500), ylim=c(0,0.0018),
     font.main=1, xaxt='n')
ax2()
plot(ora_inlandD,col='#CC9900',fill='#CC990022', add=TRUE)
plot(dk_kde, lty=2, fill=NA, add=T, scalefactor=2)
divline()
legend('topright',bty='n',lty=c(1,1,2),col=c('blue','#CC9900',1),
       legend=c('Coastal pots','Inland pots','Sh. middens'))

plot(MCr.as.MCd(I_d13C.bulk),fill='#CC990022', col='#CC9900',
     main=expression("Bulk "^{13}*"C"), ylab=d13Cpm,xaxt='n',
     ylim=c(-31,-21),  xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(C_d13C.bulk), add=TRUE, fill='#0000AA11', col='#0000AA')
divline()


plot(MCr.as.MCd(I_d15N.bulk),fill='#CC990022', col='#CC9900',
     main=expression("Bulk "^{15}*"N"), ylab=d15Npm,xaxt='n', ylim=c(6,10),
     xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(C_d15N.bulk), add=TRUE, fill='#0000AA11', col='#0000AA')
divline()


plot(MCr.as.MCd(I_d13C.16.0), main=expression("C"*""[16:0]),  ylab=d13Cpm,
     xaxt='n', ylim=c(-35,-20), fill='#CC990022', col='#CC9900',
     xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(C_d13C.16.0),add=TRUE,fill='#0000AA11', col='#0000AA')
divline()


plot(MCr.as.MCd(I_d13C.18.0),main=expression("C"*""[18:0]),
     ylab=d13Cpm,xaxt='n',ylim=c(-35,-20), fill='#CC990022', col='#CC9900',
     xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(C_d13C.18.0), add=TRUE,fill='#0000AA11', col='#0000AA')
divline()


plot(MCr.as.MCd(C_bigD), main=BD,ylim=c(-6,1),
     ylab=expression(Delta * ""^{13} * "C / ‰"),xaxt='n',fill='#0000AA11',
     col='#0000AA',xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(I_bigD), add=TRUE, fill='#CC990022', col='#CC9900')
divline()


plot(MCr.as.MCd(C_aq), ylim=c(0,100),fill='#0000AA11', col='#0000AA',
     main='Aquatic',ylab='Presence %',font.main=1,xaxt='n',xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(I_aq), add=TRUE,fill='#CC990022', col='#CC9900')
divline()


plot(MCr.as.MCd(C_da), ylim=c(0,100),fill='#0000AA11', col='#0000AA',
     main='Dairy',ylab='Presence %',font.main=1,xaxt='n',xlim=c(-4500,-2500))
ax2()
plot(MCr.as.MCd(I_da),add=TRUE,fill='#CC990022', col='#CC9900')
divline()


plot(MCr.as.MCd(C_pl), ylim=c(0,100),fill='#0000AA11', col='#0000AA',
     main='Plant (coastal sites)',ylab='Presence %', font.main=1, xaxt='n',
     xlim=c(-4500,-2500))
ax2()
divline()


mtext('BCE',side=1,line=0.6,adj=0.53,outer=TRUE,cex=0.75)
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend('topright',xpd=TRUE, ncol=2, lty=1, col=c('#CC9900','#0000AA'),legend=c('Inland','Coastal'),bty='n')

dev.off()


##################################################
# Figure 8: KDEs per resource

cairo_pdf('~/Dropbox/articles/Danish_shells/Fig8.pdf', width=4.5, height=5)

par(mfrow=c(3,1),mar=c(0.5,4.1,0,1),omi=c(0.66,0,0.5,0))

plot(marD, xlim=c(-5800,-3000),xaxt='n')
ax(ticksize = -0.02,side=3)
legend('topleft',lty=c(1),legend=c('All wild marine resources'),bty='n')
#legend('topleft',lty=c(1,3),legend=c('All wild marine resources','All sites (rescaled)'),bty='n')
#plot(ALLD, add=T,scalefactor=2.9,lty=3)
polygon(ggrsignif(ggr(marD)))
#MCmaxpoint(marD)
abline(v=-4000, lty=2)
text(c(-5000, -3200),0.0005,c('Mesolithic','Neolithic'),cex=1.33,col='darkgrey')
#text(-3850,0.00055,'3920±95\nBCE')

plot(daD, xlim=c(-5800,-3000),xaxt='n',yaxt='n')
#ax(ticksize = -0.02)
legend('topleft',lty=c(1),legend=c('Domesticated animals'),bty='n')
#legend('topleft',lty=c(1,3),legend=c('Domesticated animals','All sites (rescaled)'),bty='n')
#plot(ALLD, add=T,scalefactor=4.5,lty=3)
axis(2,at=seq(0,0.0015,0.0005),lab=c('0.000','0.0005','0.0010',NA))
polygon(ggrsignif(ggr(daD)))
mtext('BCE',side=1,line=2,adj=0.53,outer=TRUE,cex=0.75)
#MCmaxpoint(daD)
abline(v=-4000, lty=2)
#text(-3700,0.0009,'3750±70\nBCE')


plot(cerD, xlim=c(-5800,-3000),xaxt='n',yaxt='n',ylim=c(0,0.0007))
ax(ticksize = -0.02)
legend('topleft',lty=c(1),legend=c('Cereal grains'),bty='n')
#legend('topleft',lty=c(1,3),legend=c('Cereal grains','All sites (rescaled)'),bty='n')
#plot(ALLD, add=T,scalefactor=2.4,lty=3)
axis(2,at=seq(0,0.0006,0.0002),lab=c('0.000','0.004','0.004',NA))
polygon(ggrsignif(ggr(cerD)))
#MCmaxpoint(cerD[1:200,])
abline(v=-4000, lty=2)
#text(-3400,0.0006,'3595±10\nBCE')

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend('top', ncol=2, fill=c('#FF000040','#0000FF40'),
       border=NA,legend=c('Significant growth','Significant decline'), bty='n')

dev.off()


###############################################
# Figure 9a: Emperical part of ABM

# Plot density models zoomed into the transition
pdf('~/Dropbox/articles/Danish_shells/Fig9a.pdf',height=6,width=6.25)
par(omi=c(1,0.6,0.5,1),mar=c(0.2,0.8,0.8,0.1),mfcol=c(2,2))

plot(alldomD, xlim=c(-4100,-3400),xaxt='n',yaxt='n')
ax(side=3,ticksize = -0.03)
axis(2,at=c(0,0.0005,0.001,0.0015),lab=c(NA,0.0005,NA,0.0015))
divline()
plot(marD,add=T,lty=2)
legend('topleft',lty=c(1,2),legend=c('Domesticates','Marine'),bty='n')
mtext(side=2,'KDE',line=3,cex=0.9)

# Plot growthrates highlighting 3800 BC
plot(ggr(alldomD), xlim=c(-4100,-3400),ylim=c(-1,1),xaxt='n',ylab='')
ax(ticksize = -0.03)
divline()
plot(ggr(marD),add=T,lty=2)
mtext(side=2,'Growth rate (%)',line=3,cex=0.9)

domY<-mean(ggr(alldomD),-3900)
marY<-mean(ggr(marD),-3900)

P<-c(19,1); X<-c(0.75,2); for(i in 1:2) points(-3900,domY,pch=P[i],cex=X[i])
P<-c(19,1); X<-c(0.75,2); for(i in 1:2) points(-3900,marY,pch=P[i],cex=X[i])
text(-3850,domY+0.2,paste0(round(domY,1),'%'))
text(-3900,marY-0.25,paste0(round(marY,1),'%'))
mtext('BCE',side=1,outer=TRUE,line=3,cex=0.9,adj=0.24)

dev.off()


################################################################
# Figure 10: Comparison with Lewis et al

Tempelkrog<-structure(list(x = c(16279, 30233, 39535, 41860, 43023, 41860,
  41860, 29070, 29070, 40698, 30233, 41860, 26744, 43023, 36047,
  39535, 52326, 37209, 45349, 48837, 50000, 43023, 55814, 59302,
  45349, 47674, 43023, 43023, 59302, 59302, 53488, 75581, 75581,
  106977, 82558, 116279, 83721, 112791, 154651, 258140, 201163,
  141860, 140698, 125581, 154651, 137209, 244186, 104651, 139535,
  76744, 68605, 101163, 106977, 132558, 55814, 68605, 70930, 52326,
  43023, 66279, 43023, 67442, 45349, 53488, 27907, 24419, 26744,
  47674, 30233, 20930),
  y = c(8015, 8015, 7969, 7900, 7838, 7768,
  7768, 7691, 7691, 7676, 7614, 7552, 7483, 7413, 7375, 7305, 7266,
  7097, 6988, 6942, 6865, 6811, 6772, 6695, 6664, 6610, 6525, 6456,
  6386, 6332, 6270, 6147, 6147, 6116, 6062, 6023, 5938, 5931, 5915,
  5907, 5861, 5822, 5753, 5699, 5591, 5490, 5444, 5398, 5351, 5351,
  5266, 5205, 5143, 5058, 5027, 4958, 4911, 4834, 4764, 4726, 4672,
  4587, 4510, 4417, 4332, 4278, 4224, 4178, 4139, 4069)),
  row.names = c(NA,-70L), class = "data.frame")
# (Figure in Lewis et al SI was plotted vertically, so y is time x is pigment)


pdf("~/Dropbox/articles/Danish_shells/Fig10.pdf",height=4)
par(mar=c(5,4,1,4))
plot(1950-Tempelkrog$y,Tempelkrog$x,type='l',xaxt='n', xlim=c(-5500,-2800),
     ylab='Pigment flux (mg / cm² / yr)',xlab='BCE',col='darkred',ylim=c(0,280000))
ax(ticksize=-0.02)
divline()
plot(marD,add=T, scalefactor = 300000000, lty=2)
kdescale<-c(0,0.00025,0.0005,0.00075,0.001)
axis(4, at=kdescale*300000000, lab=kdescale)
mtext('KDE',side=4, line=2)
legend('topleft',lty=c(1,2),col=c('darkred',1),bty='n',y.intersp=1.5,
       c('Sediment pigment\n(Lewis et al. 2020)','Directly-dated marine\norganisms'))

dev.off()


