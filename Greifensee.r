#--------------------------------------------------------------------------
#Pegel des Greifensees
#
#
#
#K. Rosin, November 2021
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
#Pfade, Packages, Funktionen
#--------------------------------------------------------------------------

#Bisherige Objekte entfernen    
    rm(list=ls())   

#Packages
    #install.packages("tidyverse")
    #install.packages("readxl") 
    #install.packages("lubridate")
    #install.packages("here")
   # install.packages("styler")
   # install.packages("lintr")  
    library(tidyverse)
    library(readxl)
    library(lubridate)
    library(here)
    library(styler)
    library(lintr)   

#Hauptpfad
    hauptPfad <- here()
   
#Funktion: Neutrales Design fuer ggplot
    neutral <- theme_bw() + theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="grey85"),
        panel.border = element_rect(colour = "grey85"))
    
#Funktionen (u.a. trotz fehlender Werte ausfuehren)
    meanNA <- function(x){mean(x, na.rm = TRUE)}
    minNA <- function(x){min(x, na.rm = TRUE)}
    maxNA <- function(x){max(x, na.rm = TRUE)}       
    sumMissing <- function(x){sum(is.na(x))}     

    
#--------------------------------------------------------------------------
#Daten importieren
#--------------------------------------------------------------------------

#Greifensee-Pegelstand importieren und aufbereiten
    importPfad <- paste0(hauptPfad, "/Greifensee.csv") 
    
    greif <- read_csv(importPfad) %>% 
            mutate(Tage = parse_date(Datum, format = "%d.%m.%Y"),
                Jahre = year(Tage)) %>%
            select(Tage, Jahre, Pegel)      
     

#--------------------------------------------------------------------------
#Tests: Fehlende Werte?
#--------------------------------------------------------------------------
    
#Fehlende Werte (NA)?    
    summarise_all(greif, sumMissing) #keine fehlenden Werte
    
#Fehlende Tage?
    fehlend <- mutate(greif, Differenz = c(1, diff(Tage))) %>%  
        summarise_at("Differenz", list(minNA=minNA, maxNA=maxNA)) #keine fehlenden Tage
    
    
#--------------------------------------------------------------------------
#Grafik: Pegel pro Tag
#--------------------------------------------------------------------------

#Grafik    
    g1 <- ggplot() + 
        geom_line(data = greif, aes(x = Tage, y = Pegel)) +
        labs (x = "", y = "Pegel [m ü.M.]") +
        neutral
    
    ggsave(paste0(hauptPfad, "/Grafiken/1_Pegel_Tage.pdf"), g1,  
        width = 12, height = 8, units = "cm")















#--------------------------------------------------------------------------
#Aggregieren: Mittlerer Pegel pro Jahr
#--------------------------------------------------------------------------

#Aggregieren    
    greifJahr <- select(greif, Tage, Jahre, Pegel) %>% 
        group_by(Jahre) %>%             
            summarise(Pegel = meanNA(Pegel)) %>%       
        ungroup()
    
    
#Grafik
    pdf(paste0(hauptPfad, "3_Resultate/22_Tidyverse_Pegel_Jahre.pdf"), width = 9, height = 5) 
    
        ggplot() + neutral + geom_line(data = greifJahr, aes(x = Jahre, y = Pegel)) +
            labs (x = "", y = "Pegel [m ü.M.]")
        
    dev.off()
    

#--------------------------------------------------------------------------
#Facetplot: Mittlerer Pegel pro Monat und Jahr
#--------------------------------------------------------------------------
   
#Daten aufbereiten
    MonatJahr <- mutate(greif, Monate = month(Tage, label = TRUE, abbr = TRUE)) %>% 
        select(Monate, Jahre, Pegel) %>% 
        group_by(Monate, Jahre) %>%             
            summarise(Pegel = meanNA(Pegel)) %>%       
        ungroup() %>% 
        select(Jahre, Monate, Pegel) %>% 
        arrange(Jahre, Monate)
    

#Grafik
    pdf(paste0(hauptPfad, "3_Resultate/23_Tidyverse_Pegel_Monate-Jahre.pdf"), width = 10, height = 8) 
        
        ggplot() + neutral + 
            geom_line(data = MonatJahr, aes(x = Jahre, y = Pegel)) +
            facet_wrap(~ Monate, nrow = 3) +
            labs (x = "", y = "Pegel [m ü.M.]")    

    dev.off()    
        
    
    
#--------------------------------------------------------------------------
#Zusaetzlich: Nach Monat und Jahr die einzelnen Tagespegel
#--------------------------------------------------------------------------
   
#Daten aufbereiten
    MonatJahrTag <- mutate(greif, Monate = month(Tage, label = TRUE, abbr = TRUE),
        Jahre = as.factor(year(Tage)),
        TageOhneJahr = day(Tage))
    
        #Alternative, falls man die Sprache umstellen will    
        #   MonateZahl = month(Datum) 
        #   Monate = ordered(MonatZahl, levels = 1:12, labels = date_names_lang("de")$mon)
       
#Farben
    nJahre <- length(unique(MonatJahrTag$Jahre))
    farben <- colorRampPalette(c("grey80", "red", "black"))(nJahre)  
 
#Grafik
    pdf(paste0(hauptPfad, "3_Resultate/33_Zusatz_Pegel_Tage_Monate-Jahre.pdf"), 
        width = 13, height = 7) 
    
        ggplot() + neutral + 
            geom_line(data = MonatJahrTag, aes(x = TageOhneJahr, y = Pegel, color = Jahre)) +
            facet_wrap(~ Monate, nrow = 3) +
            scale_colour_manual(values = farben) +
            labs (x = "Tag im Monat", y = "Pegel [m ü.M.]", color = "Jahre")  
    
    dev.off()
    
        
#--------------------------------------------------------------------------
#Zusaetzlich: Pegel mit Hoch- und Niedrigwassergrenzen (und etwas schoener)
#--------------------------------------------------------------------------
  
#Grafik-Parameter
    xBeginn <- as_date("1998-1-1")
    xEnde <- as_date("2022-1-1")
    xSeqStriche <- seq(xBeginn, xEnde, by = "years") 
    xSeqText <- year(xSeqStriche)
    xSeqText[length(xSeqStriche)] <- ""
    
    yBeginn <- 434.5
    yEnde <- 437 
    ySeq <- seq(yBeginn, yEnde, by = 0.5)
    yHoch <- 435.5
    yNiedrig <- 435.15    
    
    poly <- tibble(x=c(xBeginn, xEnde, xEnde, xBeginn),
        y = c(yNiedrig, yNiedrig, yHoch, yHoch)) 
    
    
#Grafik
    pdf(paste0(hauptPfad, "3_Resultate/34_Zusatz_Pegel_Tage_mit-Hoch-Niedrigwassergrenzen.pdf"), 
        width = 9, height = 5)            
            
        ggplot() + neutral +
            geom_polygon(data=poly, mapping=aes(x=x, y=y), fill = "grey90") +           
            geom_vline(aes(xintercept = xSeqStriche), color = "grey90") +
            geom_line(data = greif, aes(x = Tage, y = Pegel), color = "blue4") + 
            labs (x = "", y = "Pegel [m ü.M.]") +
            scale_x_date(limits = c(xBeginn, xEnde), breaks = xSeqStriche, 
                labels = xSeqText, expand = c(0, 0)) +
            theme(axis.text.x = element_text(hjust = -0.2)) +
            scale_y_continuous(limits = c(yBeginn, yEnde), breaks = ySeq)    
        
    dev.off()   
    

    
    
# library(ggplot2) 
# nz <- map_data("nz")
# head(nz)
# 
# ggplot(nz, aes(long, lat, group = group)) +
#   geom_polygon(fill = "white", colour = "black")
# 
# ggplot(nz, aes(long, lat, group = group)) +
#   geom_polygon(fill = "white", colour = "black") +
#   coord_quickmap()

 
    
