
library(shiny)
library(ggplot2)
library(vegan)
library(EcoSimR)
library(dplyr)
library(readr)
library(rich)
library(devtools)
library(RCurl)


source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme
birds<-read.csv("https://raw.githubusercontent.com/collnell/R_vis/master/richness/example/birds.csv")
mono<-filter(birds,DIVERSITY=="M")
poly<-filter(birds, DIVERSITY=="P")
cols<-names(birds)
sps<-cols[-c(1:2)]
monom<-mono[,sps]
polym<-poly[,sps]


shinyServer(function(input,output){
  
  
  values<-reactiveValues()
  
  ##generate random community matrix using ecosimr
  test.matA<-eventReactive(input$rarefy, {#rows are sp, cols are sites
    ranMatGen(aBetaRow=input$eff_A+1, bBetaRow=2, 
              aBetaCol=input$eff_A+1, bBetaCol =2, 
              numRows=input$srA, numCols=input$eff_A, 
              mFill =.3, abun =input$abun_A,
              emptyRow=TRUE, emptyCol=TRUE)
  })
  test.matB<-eventReactive(input$rarefy, {#rows are sp, cols are sites
    ranMatGen(aBetaRow=input$eff_B+1, bBetaRow=2, 
              aBetaCol=input$eff_B+1, bBetaCol =2, 
              numRows=input$srB, numCols=input$eff_B, 
              mFill =.3, abun =input$abun_B,
              emptyRow=TRUE, emptyCol=TRUE)
  })
  
  
  
  output$rarefaction<-renderPlot({
    validate(
      need(input$rarefy,"Set community metric and press 'Rarefy!' to show species accumulation curve")
    )
    if(is.null(input$getcsv)) {
      
      sgA<-specaccum(t(test.matA()$m),method="rarefaction",conditioned="TRUE") ##run rarefaction using vegan
      sgB<-specaccum(t(test.matB()$m),method="rarefaction",conditioned="TRUE",gamma="chao") ##
      
      
    } else{
      inFile<-input$getcsv
      temp.df<-read.csv(inFile$datapath)##needs to sites x species matrix, with 1st col as grouping var
      temp.df[is.na(temp.df)]<-0
      X<-split(temp.df,as.factor(temp.df[,1]))
      adf<-X[[1]]
      adrun<-adf[-1]
      bdf<-X[[2]]
      bdrun<-bdf[-1]
      sgA<-specaccum(adrun,method="rarefaction",conditioned="TRUE")
      sgB<-specaccum(bdrun,method="rarefaction",conditioned="TRUE")
      
    }
    
      sdfA<-data.frame(ind = sgA$individuals, #create dataframe to plot 
                      rich =sgA$richness,
                      site=sgA$sites,
                      sd=sgA$sd)
      sdfA$Community<-rep("A",length(sdfA$ind))
      sdfB<-data.frame(ind = sgB$individuals,
                      rich =sgB$richness,
                      site=sgB$sites,
                      sd=sgB$sd)
      sdfB$Community<-rep("B",length(sdfB$ind))#create community variable for plotting
      
      plot.df<-rbind(sdfA,sdfB)#plotting values for rarefaction curve
      est.df<-rbind(sdfA,sdfB)
      
      SRestA<-specpool(t(test.matA()$m))  #estimate species richness
      SRestB<-specpool(t(test.matB()$m))
      Aest<-SRestA$chao
      Best<-SRestB$chao
    if (input$xvar == "ind"){
      xlabel<-("Individuals")
      plot.df$xvary<-plot.df$ind
    } else {
      xlabel<-("Samples")
      plot.df$xvary<-plot.df$site
    }
      
    rar.plot<-ggplot(data=plot.df,aes(x=xvary,y=rich,color=Community))+geom_line(size=2)+##need to add CI shaded around line
      labs(x=xlabel,y="Species Richness")+##make smooth line?
      theme_mooney(legend.location="bottom")+
      geom_ribbon(aes(x=xvary,ymin=rich-(sd*1.96),ymax=rich+(sd*1.96),fill=Community),alpha=.4,inherit.aes = FALSE)+##incorrect way to plot sd/CI
      scale_color_manual(values=c("darkgoldenrod3","#79B791"))+scale_fill_manual(values=c("darkgoldenrod3","#79B791"))
    rar.plot
    
    if (input$chao2 == TRUE) {
      chao.rar<-rar.plot+geom_line(y=SRestA$chao,lty="dashed",size=1.5,color="darkgoldenrod3")+
        geom_line(y=SRestB$chao,lty="dotted",size=1.5,color="#79B791")+
        geom_ribbon(aes(x=xvary,ymin=SRestA$chao-SRestA$chao.se,ymax=SRestA$chao+SRestA$chao.se),fill="darkgoldenrod3",alpha=.2,inherit.aes=FALSE)+
        geom_ribbon(aes(x=xvary,ymin=SRestB$chao-SRestB$chao.se,ymax=SRestB$chao+SRestB$chao.se),fill="#79B791",alpha=.2,inherit.aes=FALSE)+
        geom_text(aes(x=0, y=SRestA$chao, label=round(Aest,2)),color="black",size=5)+
        geom_text(aes(x=0, y=SRestB$chao, label=round(Best,2)),color="black",size=5)#labels are blurry
      chao.rar
    } else {
      rar.plot
    }
  })
  
    
    
    ##working on the exmaple tab
  
  output$richests<-renderPrint({
    levels(birds$DIVERSITY)<-c("Monoculture","Polyulture")
    all.est<-specpool(birds[,-2],birds[,1])  #estimate species richness
    all.est
    
  })
  output$divplots<-renderPlot({
    mono.rar<-specaccum(monom,method="rarefaction")
    poly.rar<-specaccum(polym,method="rarefaction")
    mono.df<-data.frame(ind = mono.rar$individuals, #create dataframe to plot 
                        rich =mono.rar$richness,
                        site=mono.rar$sites,
                        sd=mono.rar$sd)
    poly.df<-data.frame(ind = poly.rar$individuals, #create dataframe to plot 
                        rich =poly.rar$richness,
                        site=poly.rar$sites,
                        sd=poly.rar$sd)
    mono.df$Community<-rep("Monoculture",length(mono.df$ind))
    poly.df$Community<-rep("Polyculture",length(poly.df$ind))
    div.df<-rbind(mono.df,poly.df)
    
    m.est<-specpool(monom)  #estimate species richness
    p.est<-specpool(polym)
    M.est<-m.est$chao
    P.est<-p.est$chao
    
    if (input$xvar_ex == "ind"){
      xlabel<-("Individuals")
      div.df$xvary<-div.df$ind
    } else {
      xlabel<-("Samples")
      div.df$xvary<-div.df$site
    }
    
    
    div.plot<-ggplot(data=div.df,aes(x=xvary,y=rich,color=Community))+geom_line(size=2)+
      labs(x=xlabel,y="Species Richness")+
      theme_mooney(legend.location="bottom")+
      scale_color_manual(values=c("darkgoldenrod3","#79B791"))+scale_fill_manual(values=c("darkgoldenrod3","#79B791"))
    div.plot
    
    if (input$chao == TRUE) {
      chao.rar.div<-div.plot+geom_line(y=M.est,lty="dashed",size=1.5,color="darkgoldenrod3")+
        geom_line(y=P.est,lty="dotted",size=1.5,color="#79B791")+
        geom_ribbon(aes(x=xvary,ymin=M.est-m.est$chao.se,ymax=M.est+m.est$chao.se),fill="darkgoldenrod3",alpha=.2,inherit.aes=FALSE)+
        geom_ribbon(aes(x=xvary,ymin=P.est-p.est$chao.se,ymax=P.est+p.est$chao.se),fill="#79B791",alpha=.2,inherit.aes=FALSE)+
        geom_text(aes(x=0, y=M.est, label=round(M.est,2)),color="black",size=5)+
        geom_text(aes(x=0, y=P.est, label=round(P.est,2)),color="black",size=5)#labels are blurry
      chao.rar.div
    } else {
      div.plot
    }
  
    
  })
  
})