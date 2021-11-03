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
    library(tidyverse)
    library(readxl)
    library(lubridate)
    library(here)

#Hauptpfad
    haupt_pfad <- here()
   
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
    importPfad <- paste0(haupt_pfad, "/Greifensee.csv") 
    
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
        labs(x = "", y = "Pegel [m ü.M.]") +
        neutral
    
    ggsave(paste0(haupt_pfad, "/Grafiken/1_Pegel_Tage.pdf"), g1,  
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
    g2 <- ggplot() + 
        geom_line(data = greifJahr, aes(x = Jahre, y = Pegel)) +
        labs(x = "", y = "Pegel [m ü.M.]") + neutral 
    
    ggsave(paste0(haupt_pfad, "/Grafiken/2_Pegel_Jahre.pdf"), g2,  
        width = 12, height = 8, units = "cm")


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
    g3 <- ggplot() + 
        geom_line(data = MonatJahr, aes(x = Jahre, y = Pegel)) +
        facet_wrap(~ Monate, nrow = 3) +
        labs(x = "", y = "Pegel [m ü.M.]") +
        neutral   
    
    ggsave(paste0(haupt_pfad, "/Grafiken/3_Pegel_Monate-Jahre.pdf"), g3,  
        width = 20, height = 15, units = "cm")

   
#--------------------------------------------------------------------------
#Zusaetzlich: Nach Monat und Jahr die einzelnen Tagespegel
#--------------------------------------------------------------------------

#Daten aufbereiten
    MonatJahrTag <- mutate(greif, 
        Monate = month(Tage, label = TRUE, abbr = TRUE),
        TageOhneJahr = day(Tage))
    
#Alternative, falls die Sprache umgestellt werden muss    
#   MonateZahl = month(Datum) 
#   Monate = ordered(MonatZahl, levels = 1:12, labels = date_names_lang("de")$mon)
 
#Aktuelles Jahr
    aktuell <- filter(MonatJahrTag, Jahre == max(Jahre))
    
#Vorjahre
    vorher <- filter(MonatJahrTag, Jahre < max(Jahre)) %>% 
        group_by(Monate, TageOhneJahr) %>% 
            summarize(minPegel = min(Pegel), 
                      maxPegel = max(Pegel)) %>% 
        ungroup() 
    
#Legenden-Labels
    leg <- c(paste(min(greif$Jahre), "bis", max(greif$Jahre) - 1), 
                   max(greif$Jahre))
    
#Grafik 
    # https://stackoverflow.com/questions/28714492/legend-with-geom-line-and-geom-ribbon    
    # analog: bar, line    
    # https://community.rstudio.com/t/adding-a-legend-to-an-overlay-bar-and-line-plot/69331/6
        
    g4 <- ggplot() + 
        geom_ribbon(data = vorher, 
                    aes(x = TageOhneJahr, ymin=minPegel, ymax=maxPegel, fill = leg[1]), 
                    alpha = 0.5) + 
        geom_line(data = aktuell, aes(x = TageOhneJahr, y = Pegel, color = leg[2])) +
        facet_wrap(~Monate, nrow = 3) + 
        scale_fill_manual("", values = "grey50") +
        scale_colour_manual("", values = "red") +
        labs(x = "Tag im Monat", y = "Pegel [m ü.M.]") +      
        neutral 

    ggsave(paste0(hauptPfad, "/Grafiken/4_Pegel_Tage_Monate-Jahre.pdf"), g4,
        width = 20, height = 12, units = "cm")


#--------------------------------------------------------------------------
#Zusaetzlich: Quantile und einzelne Tagespegel
#--------------------------------------------------------------------------

#der Code baut auf der vorangehenden Grafik auf
#neu: Quantile anstatt graue Flaeche
    
#Quantile waehlen
    quant <- seq(0, 1, by = 0.2)
    
#Lookup-Tabelle
    look_quant <- tibble(Min = quant[-length(quant)],
                         Max = quant[-1],
                         Ribbon = paste(Min * 100, "bis", Max * 100),
                         Farben = colorRampPalette(c("red", "grey50", "blue"))(length(quant)-1))
    
#Vorjahre: Quantile
    vorher_qua <- filter(MonatJahrTag, Jahre < max(Jahre)) %>% 
        group_by(Monate, TageOhneJahr) %>%         
            summarize(Pegel = quantile(Pegel, quant), 
                      Quantil = quant) %>% 
        ungroup()
    
#Pro Ribbon: unterer Wert
    vorher_unten <- left_join(vorher_qua, 
                              select(look_quant, Min, Ribbon), 
                              by = c("Quantil" = "Min")) %>% 
        rename(Pegel_unten = Pegel) %>% 
        filter(Quantil < quant[length(quant)]) %>% 
        select(Monate, TageOhneJahr, Ribbon, Pegel_unten)
    
#Pro Ribbon: oberer Wert
    vorher_oben <- left_join(vorher_qua, 
                              select(look_quant, Max, Ribbon), 
                              by = c("Quantil" = "Max")) %>% 
        rename(Pegel_oben = Pegel) %>% 
        filter(Quantil > quant[1]) %>%    
        select(Monate, TageOhneJahr, Ribbon, Pegel_oben) 

    
#Ribbon (mit oberem und unterem Wert)
    vorher_uo <- left_join(vorher_unten, vorher_oben,     
               by = c("Monate", "TageOhneJahr", "Ribbon")) %>% 
        mutate(RibbonFaktor = factor(Ribbon, levels = look_quant$Ribbon))
    
    
#Grafik
    g5 <- ggplot() + 
        geom_ribbon(data = vorher_uo, 
                    aes(x = TageOhneJahr, ymin=Pegel_unten, ymax=Pegel_oben, 
                        fill = RibbonFaktor), 
                    alpha = 0.5) + 
        geom_line(data = aktuell, aes(x = TageOhneJahr, y = Pegel, color = leg[2])) +
        scale_fill_manual(paste(leg[1], "(Quantile)"), values = look_quant$Farben) + 
        scale_colour_manual("", values = "black") +        
        facet_wrap(~Monate, nrow = 3) +
        labs(x = "Tag im Monat", y = "Pegel [m ü.M.]") +      
        neutral 

    ggsave(paste0(hauptPfad, "/Grafiken/5_Pegel_Quantile.pdf"), g5,
        width = 20, height = 12, units = "cm")
    
    
    
    
        

    
    
    
    
    
    
    
    
    
    
    
