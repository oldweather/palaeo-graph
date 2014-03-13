# Composite plot of illustrative 'temperature' for the whole Cenezoic

library(grid)
pdf(file="Cenezoic.pdf",width=44,height=7,family="Helvetica",pointsize=18)

gp_red <- gpar(col=rgb(1,0,0,0.7),fill=rgb(1,0,0,0.7),lex=1)
gp_blue <- gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lex=3)
gp_uncertainty <- gpar(col=rgb(0,0,1,0.2),fill=rgb(0,0,1,0.2),lex=1)
gp_grey <- gpar(col=rgb(0,0,0,0.2),fill=rgb(0,0,0,0.2),lex=1)
gp_txt.small <- gpar(cex=0.75)

# Need a logarithmic time scaling
to.time<-function(x) {
  t<-log(2200-x)*-1
  return(t)
}
to.gst<-function(x,o,s) {
  t<-(x+o)*s
  return(t)
}

x.tics<-c(2100,2000,0,-10000,-100000,-1000000,-10000000,-65000000)
x.labels<-c('2100','2000','0','-10,000','-100,000','-1,000,000',
            '-10,000,000','-65,000,000')

boundaries<-c(1950-67.5173e+6,1950-5.32e+6,1950-801662,1950-49003.4,
              1950-11290,800,1850,2000,2100)

plot.best<-function(x,y,start) {
  blue<-c(0,0,1,1)
  for(s in seq(start,length(boundaries))) {
    x.r<-x[x<boundaries[s+1] & x>=boundaries[s]]
    if(length(x.r)<1) break
    y.r<-y[x<boundaries[s+1] & x>=boundaries[s]]
    gp <- gpar(col=rgb(blue[1],blue[2],blue[3],blue[4]),
               fill=rgb(blue[1],blue[2],blue[3],blue[4]),lex=2)
    grid.lines(x=unit(to.time(x.r),"native"),
               y=unit(y.r,'native'),gp=gp)
    blue[4]<-blue[4]/3
  }
}
plot.range<-function(x,y.t,y.b,start) {
  range<-c(0,0,1,0.4)
  for(s in seq(start,length(boundaries))) {
    x.r<-x[x<boundaries[s+1] & x>=boundaries[s]]
    if(length(x.r)<1) break
    y.t.r<-y.t[x<boundaries[s+1] & x>=boundaries[s]]
    y.b.r<-y.b[x<boundaries[s+1] & x>=boundaries[s]]
    gp.range <- gpar(col=rgb(range[1],range[2],range[3],0),
                    fill=rgb(range[1],range[2],range[3],range[4]),lex=1)
    grid.polygon(x=unit(to.time(c(x.r,rev(x.r))),"native"),
                 y=unit(c(y.b.r,rev(y.t.r)),'native'),
                 gp=gp.range)
    range[4]<-range[4]/3
  }
}

pushViewport(viewport(width=1.0,height=1.0,x=0.0,y=0.0,
                      just=c("left","bottom")))
pushViewport(plotViewport(margins=c(4,5,2,2)))
    pushViewport(dataViewport(to.time(c(-65000000,2101)),c(-6,12)))
    grid.xaxis(main=T,at=to.time(x.tics),label=x.labels)
    grid.text('Year',y=unit(-3,"lines"))
    grid.yaxis(main=T)
    grid.text('Sort-of global temperature anomaly (C)',
              x=unit(-3.5,"lines"), rot=90)
    upViewport()
    pushViewport(dataViewport(to.time(c(-65000000,2101)),c(-6,12),clip='on'))
    grid.lines(x=unit(to.time(c(-70000000,2101)),'native'),
               y=unit(c(0,0),'native'),
               gp=gp_grey)
    grid.lines(x=unit(to.time(c(-70000000,2101)),'native'),
               y=unit(c(2,2),'native'),
               gp=gp_grey)
    grid.lines(x=unit(to.time(c(-70000000,2101)),'native'),
               y=unit(c(4,4),'native'),
               gp=gp_grey)
    grid.lines(x=unit(to.time(c(-70000000,2101)),'native'),
               y=unit(c(-2,-2),'native'),
               gp=gp_grey)
    grid.lines(x=unit(to.time(c(-70000000,2101)),'native'),
               y=unit(c(-4,-4),'native'),
               gp=gp_grey)

 # Whole Cenozoic
    grid.lines(x=unit(to.time(1950-67.5173e+6),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    dD<-read.table('periods/cenozoic/zachos2001.txt.fmt',header=F)
    dD$V2 <- 1950-dD$V2*1.0e+6
    dD$V6 <- to.gst(dD$V6,-3.2,-3)
    plot.best(dD$V2,dD$V6,1)
    grid.text('Ocean Sediments',
                 x=unit(to.time(1950-67.5173e+6+1000000),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('(Benthic forams)',
                 x=unit(to.time(1950-67.5173e+6+20000000),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)
    grid.text('Zachos et al. 2001',
                 x=unit(to.time(1950-67.5173e+6+20000000),"native"),
                 y=unit(10.0,'native'),
                 just='left',
                 gp=gp_txt.small)

 # Benthic stack
    grid.lines(x=unit(to.time(1950-5.32e+6),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    dD<-read.table('periods/ocean-cores/LR04stack.txt.fmt',header=F)
    dD$V1 <- 1950-dD$V1
    dD$V2 <- to.gst(dD$V2,-3.2,-3)
    plot.best(dD$V1,dD$V2,2)
    grid.text('Ocean Sediments',
                 x=unit(to.time(1950-5.32e+6+100000),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('(Benthic forams)',
                 x=unit(to.time(1950-5.32e+6+100000),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)
    grid.text('Lisiecki & Raymo 2005',
                 x=unit(to.time(1950-5.32e+6+100000),"native"),
                 y=unit(10.0,'native'),
                 just='left',
                 gp=gp_txt.small)

  # Antarctic cores
    grid.lines(x=unit(to.time(1950-801662),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    dD<-read.table('periods/ice-cores/edc3deuttemp2007.txt.cut',header=F,fill=T)
    dD$V3 = 1950-dD$V3
    dD$V4 = to.gst(dD$V4,395,0.1)
    plot.best(dD$V3,dD$V4,3)
    grid.text('Antarctic ice core',
                 x=unit(to.time(1950-801662+10000),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('EPICA (Jouzel et al. 2007)',
                 x=unit(to.time(1950-801662+10000),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)

 # GISP core
    grid.lines(x=unit(to.time(1950-49003.4),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
  Gt<-read.table('periods/dansgaard-oeschger/gisp2_temp_accum_alley2000.txt.fmt',header=F)
  Gt$V1 <- 1950-Gt$V1*1000
  Gt$V2<-to.gst(Gt$V2,32,0.2)
  plot.best(Gt$V1,Gt$V2,4)
    grid.text('Greenland ice core',
                 x=unit(to.time(1950-49003.4+1000),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('Alley 2000',
                 x=unit(to.time(1950-49003.4+1000),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)
 
  # Holocene
    grid.lines(x=unit(to.time(1950-11290),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    mi<-read.table('periods/holocene/Marcott.SM.database.S1.txt',header=F,fill=T)
    plot.range(1950-mi$V1,mi$V2+mi$V3*2,mi$V2-mi$V3*2,5)
    plot.best(1950-mi$V1,mi$V2,5)
    grid.lines(x=unit(mi$V1,"native"),
               y=unit(mi$V2,'native'),gp=gp_uncertainty)
    grid.text('Decadal proxies (mostly alkenones)',
                 x=unit(to.time(1950-11290+300),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('Marcott et al 2013',
                 x=unit(to.time(1950-11290+300),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)

  # Last millennium
    grid.lines(x=unit(to.time(800),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    mi<-read.table('periods/last-millennium/B2000',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/BOS..2001',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/DWJ2006',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/HCA..2006',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/ECS2002',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/MBH1999',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/MJ2003',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/MSH..2005',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/O2005',header=F)
    plot.best(mi$V1,mi$V2,6)
    mi<-read.table('periods/last-millennium/RMO..2005',header=F)
    plot.best(mi$V1,mi$V2,6)
    grid.text('Annual proxies',
                 x=unit(to.time(850),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('(mostly tree-rings)',
                 x=unit(to.time(850),"native"),
                 y=unit(11.0,'native'),
                 just='left')
    grid.text('After AR4 F6.1',
                 x=unit(to.time(850),"native"),
                 y=unit(10.0,'native'),
                 just='left',
                 gp=gp_txt.small)


  # Instrumental period
    grid.lines(x=unit(to.time(1850),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
    instrumental<-read.table('periods/instrumental/annual',header=F)
    plot.range(instrumental$V1,instrumental$V11,instrumental$V12,7)
    plot.best(instrumental$V1,instrumental$V2,7)
   # Add a key
    grid.text('Thermometers',
                 x=unit(to.time(1863),"native"),
                 y=unit(9.0,'native'),
                 just='left')
    grid.text('HadCRUT3',
                 x=unit(to.time(1863),"native"),
                 y=unit(8.0,'native'),
                 just='left',
                 gp=gp_txt.small)

 # Next century predictions
    grid.lines(x=unit(to.time(2000),'native'),
               y=unit(c(5,20),'native'),
               gp=gp_grey)
  future<-read.table('periods/next-century/cmip3_range.out',header=F)
    plot.range(future$V1,future$V2,future$V3,8)
    grid.text('Simulations',
                 x=unit(to.time(2005),"native"),
                 y=unit(12.0,'native'),
                 just='left')
    grid.text('CMIP3 A1B ensemble',
                 x=unit(to.time(2005),"native"),
                 y=unit(11.0,'native'),
                 just='left',
                 gp=gp_txt.small)
                  
upViewport(0)
