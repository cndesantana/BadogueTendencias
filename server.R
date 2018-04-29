library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(readxl)
library(maptools)
library(brmap)
library(maptools)
library(rgdal) # ensure rgdal is loaded
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)

options(shiny.maxRequestSize=30*1024^2)

function(input, output, session) {

   lm_eqn <- function(df){
      m <- lm(y ~ x, df);
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                       list(a = format(coef(m)[1], digits = 2), 
                            b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
      cat(paste(as.character(df$regiao)[1], format(coef(m)[2], digits = 2), sep=","),file=as.character(df$filename)[1],sep="\n",append=TRUE)
      as.character(as.expression(eq));                 
   }
   
   cleaningRelNI1Grupos = function(relNI1){
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, " \"","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\"]","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\\[","")
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\" ,",",")
      
      relNI1 <-  relNI1 %>%
         mutate(Grupos = strsplit(as.character(Grupos), ", ")) %>% 
         unnest(Grupos)
      relNI1 <-  relNI1 %>%
         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
         unnest(Grupos)
      
      relNI1 <- relNI1 %>% 
         mutate(Grupos = stringr::str_replace_all(Grupos, "[\\[\\]]","")) %>% 
         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
         unnest(Grupos) %>% 
         filter(Grupos != "CPBA_3629944") %>%
         filter(Grupos != "Rui Correria_9356576")  %>%
         filter(Grupos != "Carnaval_974981")%>%
         filter(Grupos != "Notícias Carnaval") %>%
         filter(Grupos != "teste_6503868") %>%
         filter(Grupos != "Neutro")%>%
         filter(Grupos != "Rui Correria") %>%
         filter(Grupos != "Carnaval") %>%
         filter(Grupos != "TESTE Região 11 Caetité") %>%
         filter(Grupos != "TESTE 11 Caetité") %>%
         filter(Grupos != " Correria") %>%
         filter(Grupos != "TESTE11 Caetité")
      
      relNI1$Grupos[which(relNI1$Grupos == "Região 8 - Feira + ParÁguaçu")] <- "Região 8 - Feira + Paraguaçu"
      relNI1$Grupos[which(relNI1$Grupos == "Região 8 - Feira + Paráguaçu")] <- "Região 8 - Feira + Paraguaçu"
      relNI1$Grupos[which(relNI1$Grupos == "Região 10 - Irecê/ChapadaDiamantina")] <- "Região 10 - Irecê/Chapada Diamantina"
      #   relNI1 <- relNI1[which(relNI1$Grupos != "TESTE11 Caetité_5992896"),]
      
      return(relNI1)
   }
   
   limpaPlanilha = function(relNI1){
      
      ################### CLEANING GRUPOS
      relNI1 <- cleaningRelNI1Grupos(relNI1)
      
      ################### CLEANING 'TEMAS'
      
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, '_.*', '')
      
      return(relNI1)
   }

   plotTendencias = function(){
      filepath <- input$file$datapath
      if(length(filepath) > 0){
            relNI1 <- read_xlsx(filepath)
            dfGrupos1 <- limpaPlanilha(relNI1)
            dfGrupos1 <- dfGrupos1[which(dfGrupos1$Grupos != "TESTE Região 11 Caetité"),]
            uniqueGrupos1 <- dfGrupos1 %>% 
               group_by(Grupos) %>% 
               summarise(count = n()) %>% 
               arrange(count) %>% tail(20) 
            
            dfGruposDataA1 <- dfGrupos1 %>% mutate(
               day = dmy_hm(Data) %>% 
                  as.Date() %>%
                  format("%d"), 
               month = dmy_hm(Data) %>% 
                  as.Date() %>%
                  format("%m"), 
               year = dmy_hm(Data) %>% 
                  as.Date() %>% 
                  format("%Y"), 
               hour = dmy_hm(Data) %>% 
                  as.Date() %>%
                  format("%H"), 
               min = dmy_hm(Data) %>% 
                  as.Date() %>%
                  format("%M"),
               week = (as.numeric(difftime(dmy_hm(dfGrupos1$Data),dmy_hm("01/08/2017 00:07"),units="days")) %/% 7) + 1)  %>%
               group_by(Grupos)
            
            dfGruposData1 <- dfGruposDataA1 %>% group_by(Grupos, week) %>%
               mutate(
                  npos = sum(Polaridade=="Positivo"), 
                  nneg = sum(Polaridade=="Negativo"),
                  is = ifelse(npos==nneg,0,(npos - nneg)/(npos + nneg))
               ) %>% 
               select(Grupos,week,is) %>%
               arrange(Grupos,week,is) %>% unique()
            dfGruposData1$grpnumber <- unlist(strsplit(unlist(strsplit(dfGruposData1$Grupos,"-"))[seq(from=1,to=2*length(dfGruposData1$Grupos),by=2)]," "))[seq(from=2,to=2*length(dfGruposData1$Grupos),by=2)]
            
            week_labels <- seq(from=as.Date(dmy_hm("01/08/2017 00:07")), to = as.Date(max(dmy_hm(dfGrupos1$Data))), by="weeks")
            week_breaks <- 1:length(week_labels)
            
            i <- as.character(input$grupo)
            dfGruposData1I <- dfGruposData1 %>% filter(grpnumber ==i)
            
            ggplot(dfGruposData1I) + 
               geom_point(stat="identity",aes(x=as.numeric(week), y=as.numeric(is)), size=2.5) +
               geom_line(color="blue",stat="identity",aes(x=as.numeric(week), y=as.numeric(is), group=1), size=0.25) +
               geom_smooth(aes(x=as.numeric(week), y=as.numeric(is)), method="lm", se = FALSE) + 
               facet_wrap(~ Grupos) + 
               ylab("IS") + 
               ylim(-1,1) + 
               scale_x_continuous(name="Semanas", breaks=week_breaks, limits=range(week_breaks), labels=week_labels) +
               theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1))
           }
   }
   
   output$tendenciasTS = downloadHandler(
      filename = function() {
         paste("tendencias_",input$mes,".png", sep = "")
      },
      content = function(file) {
         device <- function(..., width, height) {
            grDevices::png(..., width = 3200, height = 1800,
                           res = 300)
         }
         ggsave(file, plot = plotTendencias(), device = device)
      }   
      )    

  output$tendencias <- renderPlot({

     filepath <- input$file$datapath
     if(length(filepath) > 0){
        relNI1 <- read_xlsx(filepath)
        dfGrupos1 <- limpaPlanilha(relNI1)
        dfGrupos1 <- dfGrupos1[which(dfGrupos1$Grupos != "TESTE Região 11 Caetité"),]
        uniqueGrupos1 <- dfGrupos1 %>% 
           group_by(Grupos) %>% 
           summarise(count = n()) %>% 
           arrange(count) %>% tail(20) 
        
        cat("dfGruposDataA1",sep="\n")        
        dfGruposDataA1 <- dfGrupos1 %>% mutate(
           day = dmy_hm(Data) %>% 
              as.Date() %>%
              format("%d"), 
           month = dmy_hm(Data) %>% 
              as.Date() %>%
              format("%m"), 
           year = dmy_hm(Data) %>% 
              as.Date() %>% 
              format("%Y"), 
           hour = dmy_hm(Data) %>% 
              as.Date() %>%
              format("%H"), 
           min = dmy_hm(Data) %>% 
              as.Date() %>%
              format("%M"),
           week = (as.numeric(difftime(dmy_hm(dfGrupos1$Data),dmy_hm("01/08/2017 00:07"),units="days")) %/% 7) + 1)  %>%
           group_by(Grupos)

        cat("dfGruposData1",sep="\n")        
        dfGruposData1 <- dfGruposDataA1 %>% group_by(Grupos, week) %>%
           mutate(
              npos = sum(Polaridade=="Positivo"), 
              nneg = sum(Polaridade=="Negativo"),
              is = ifelse(npos==nneg,0,(npos - nneg)/(npos + nneg))
           ) %>% 
           select(Grupos,week,is) %>%
           arrange(Grupos,week,is) %>% unique()
        dfGruposData1$grpnumber <- unlist(strsplit(unlist(strsplit(dfGruposData1$Grupos,"-"))[seq(from=1,to=2*length(dfGruposData1$Grupos),by=2)]," "))[seq(from=2,to=2*length(dfGruposData1$Grupos),by=2)]
        
        cat("weeks",sep="\n")
        week_labels <- seq(from=as.Date(dmy_hm("01/08/2017 00:07")), to = as.Date(max(dmy_hm(dfGrupos1$Data))), by="weeks")
        week_breaks <- 1:length(week_labels)
        i <- as.character(input$grupo)
        dfGruposData1I <- dfGruposData1 %>% filter(grpnumber == i)
        ggplot(dfGruposData1I) + 
           geom_point(stat="identity",aes(x=as.numeric(week), y=as.numeric(is)), size=2.5) +
           geom_line(color="blue",stat="identity",aes(x=as.numeric(week), y=as.numeric(is), group=1), size=0.25) +
           geom_smooth(aes(x=as.numeric(week), y=as.numeric(is)), method="lm", se = FALSE) + 
           facet_wrap(~ Grupos) + 
           ylab("IS") + 
           ylim(-1,1) + 
           scale_x_continuous(name="Semanas", breaks=week_breaks, limits=range(week_breaks), labels=week_labels) +
           theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1))
     }
  })

}
