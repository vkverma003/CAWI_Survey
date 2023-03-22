rm(list = ls())

library("readxl")
library("dplyr")
library("ggplot2")
library("scales")
library("tidyverse")

cat("\014")
graphics.off()


#A. Reading CAWI data from excel file ====

#1. Response sheet
file <- "D:/4. IITR/3. Manuscripts & Chapters/5. TIPCE2022/Responses_CAWI.xlsx"
data <- read_excel(file, sheet = "Combined", col_names = TRUE)
data <- tibble(data)

data <- na_if(data, -3)
data <- na_if(data, -1)


unique <- sort(unique(data$Gender))
data$Gender[data$Gender %in% unique[1]] <- "Male"
data$Gender[data$Gender %in% unique[2]] <- "Female"

unique <- sort(unique(data$`Age Group`))
data$`Age Group`[data$`Age Group` %in% unique[1]] <- "15-25"
data$`Age Group`[data$`Age Group` %in% unique[2]] <- "26-45"
data$`Age Group`[data$`Age Group` %in% unique[3]] <- "46-60"

unique <- sort(unique(data$Education))
data$Education[data$Education %in% unique[1]] <- "Up to 12th"
data$Education[data$Education %in% unique[2]] <- "Graduate"
data$Education[data$Education %in% unique[3]] <- "PG or Higher"

unique <- sort(unique(data$Occupation))
data$Occupation[data$Occupation %in% unique[1]] <- "Unemployed"
data$Occupation[data$Occupation %in% unique[2]] <- "Retired"
data$Occupation[data$Occupation %in% unique[3]] <- "Student"
data$Occupation[data$Occupation %in% unique[4]] <- "Supporting Staff"
data$Occupation[data$Occupation %in% unique[5]] <- "Middle Level"
data$Occupation[data$Occupation %in% unique[6]] <- "Higher Level"

unique <- sort(unique(data$Income))
data$Income[data$Income %in% unique[1]] <- "Nil"
data$Income[data$Income %in% unique[2]] <- "10,001- 25,000"
data$Income[data$Income %in% unique[3]] <- "25,001 - 50,000"
data$Income[data$Income %in% unique[4]] <- "50,001 - 1 Lakh"
data$Income[data$Income %in% unique[5]] <- "1 - 1.5 Lakh "
data$Income[data$Income %in% unique[6]] <- "1.5 Lakh or above"

unique <- sort(unique(data$Ownership))
data$Ownership[data$Ownership %in% unique[1]] <- "Car"
data$Ownership[data$Ownership %in% unique[2]] <- "2 Wheeler"
data$Ownership[data$Ownership %in% unique[3]] <- "Both"
data$Ownership[data$Ownership %in% unique[4]] <- "None"

unique <- sort(unique(data$`Driving License`))
data$`Driving License`[data$`Driving License` %in% unique[1]] <- "Car"
data$`Driving License`[data$`Driving License` %in% unique[2]] <- "2 Wheeler"
data$`Driving License`[data$`Driving License` %in% unique[3]] <- "Both"
data$`Driving License`[data$`Driving License` %in% unique[4]] <- "None"

unique <- sort(unique(data$TripFreqChangeCOVID))

data$TripFreqChangeCOVID[data$TripFreqChangeCOVID %in% unique[1] ] <- "Decreased"
data$TripFreqChangeCOVID[data$TripFreqChangeCOVID %in% unique[2] ] <- "Increased"
data$TripFreqChangeCOVID[data$TripFreqChangeCOVID %in% unique[3] ] <- "Not Changed"
data$TripFreqChangeCOVID[data$TripFreqChangeCOVID %in% unique[4] ] <- "At Home Completely"

#2. Responses Completeness====
#2.1 Overall Completeness ====
data %>% 
    select(Method, Completeness) %>% 
    group_by(Method) %>% 
    table() %>% 
    prop.table()

#2.2 Part Wise Completeness ====

for (name in unique(data$Method)) {
        
        a <- data %>% 
            filter(Method == name) %>% 
            select(-c(Date, Transit, Completeness, Method)) %>% 
            select(1:36)
        
        b <- colSums(is.na(a))/nrow(a)
        b <- round(b, digits = 2)

        assign(name, b)
    }
part1 <- rbind(Snowball, `Crowd Source`, Whatsapp)
rownames(part1) <- c("S", "OL", "TM")

par(mar=c(15, 5, 3, 1))
plot1 <- part1 %>%
    barplot(horiz = FALSE, las= 2, cex.names=0.9, legend = TRUE, ylab="No Responses (In Percent)", ylim = c(0, 2))


for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(39:69)
    
    b <- colSums(is.na(a))/nrow(a)
    b <- round(b, digits = 2)
    
    assign(name, b)
}
part2 <- rbind(Snowball, `Crowd Source`, Whatsapp)
rownames(part2) <- c("S", "OL", "TM")

par(mar=c(15, 5, 3, 1))
plot2 <- part2 %>%
    barplot(horiz = FALSE, las= 2, cex.names=0.9, ylab="No Responses (In Percent)", ylim = c(0, 2) )


for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(70:86)
    
    b <- colSums(is.na(a))/nrow(a)
    b <- round(b, digits = 2)
    
    assign(name, b)
}
part3 <- rbind(Snowball, `Crowd Source`, Whatsapp)
rownames(part3) <- c("S", "OL", "TM")

par(mar=c(15, 5, 3, 1))
plot3 <- part3 %>%
    barplot(horiz = FALSE, las= 2, cex.names=0.9, legend = TRUE, ylab="No Responses (In Percent)", ylim = c(0, 2))



#2.3 Complete Dataset ====

for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method))
    
    b <- colSums(is.na(a))/nrow(a)
    b <- round(b, digits = 2)
    
    assign(name, b)
}
part <- rbind(Snowball, `Crowd Source`, Whatsapp)

part <- t(part) %>% 
    data.frame() %>% 
    rownames_to_column()

colnames(part) <- c("Features", "Snowball", "CrowdSource", "Whatsapp")

part1 <- part[1:38, ]
mean(part1$Snowball)
mean(part1$CrowdSource)
mean(part1$Whatsapp)

part2 <- part[39:69, ]
mean(part2$Snowball)
mean(part2$CrowdSource)
mean(part2$Whatsapp)


part3 <- part[70:79, ]
mean(part3$Snowball)
mean(part3$CrowdSource)
mean(part3$Whatsapp)


part4 <- part[80:86, ]
mean(part4$Snowball)
mean(part4$CrowdSource)
mean(part4$Whatsapp)


part[1:36, ] %>%
    pivot_longer(Snowball:Whatsapp, names_to = "Method", values_to = "Incomplete") %>% 
    ggplot(aes(x=factor(Features), y=Incomplete, fill=Method)) +
    geom_col(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#2.4 Part Wise Completeness Percent ====

for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(1:38)
    
    b <- colSums(is.na(a))
    c <- dim(a)
    
    b <- sum(b)/(c[1]*c[2])
    
    assign(name, b)
}
part1 <- rbind(Snowball, `Crowd Source`, Whatsapp)

for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(39:69)
    
    b <- colSums(is.na(a))
    c <- dim(a)
    
    b <- sum(b)/(c[1]*c[2])
    
    assign(name, b)
}
part2 <- rbind(Snowball, `Crowd Source`, Whatsapp)

for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(70:79)
    
    b <- colSums(is.na(a))
    c <- dim(a)
    
    b <- sum(b)/(c[1]*c[2])
    
    assign(name, b)
}
part3 <- rbind(Snowball, `Crowd Source`, Whatsapp)


for (name in unique(data$Method)) {
    
    a <- data %>% 
        filter(Method == name) %>% 
        select(-c(Date, Transit, Completeness, Method)) %>% 
        select(80:86)
    
    b <- colSums(is.na(a))
    c <- dim(a)
    
    b <- sum(b)/(c[1]*c[2])
    
    assign(name, b)
}
part4 <- rbind(Snowball, `Crowd Source`, Whatsapp)

#3. CAWI Method Distribution====
data %>% 
    select(Method, Completeness) %>% 
    group_by(Method) %>% 
    table() %>% 
    prop.table()



#4. Sample Characteristics ====
socio_data <- data[ ,c(2, 82:88, 90)]

for (var in colnames(socio_data[ ,1:7])) {
    
    for(name in unique(socio_data$Method)) {
        
        a <- socio_data %>%
            select(var, Method) %>% 
            filter(Method == name) %>% 
            table() %>% 
            prop.table()
        
        print(a)
    }

}

Gender <- socio_data %>%
    select(Gender, Method, Transit) %>% 
    table() %>%
    data.frame() %>% 
    group_by(Method) %>% 
    mutate(Percent = Freq/sum(Freq)) %>%
    mutate(Method = case_when(Method == "Whatsapp" ~ "TM",
                              Method == "Snowball" ~ "S",
                              Method == "Crowd Source" ~ "OL")) %>% 
    ggplot(aes(y=Gender, x=Percent)) +
    geom_col() +
    facet_grid(~Method)

AgeGroup <- socio_data %>%
    select(`Age Group`, Method, Transit) %>% 
    table() %>%
    data.frame() %>% 
    group_by(Method) %>% 
    mutate(Percent = Freq/sum(Freq)) %>%
    mutate(Method = case_when(Method == "Whatsapp" ~ "TM",
                              Method == "Snowball" ~ "S",
                              Method == "Crowd Source" ~ "OL")) %>%
    ggplot(aes(y=Age.Group, x=Percent)) +
    geom_col() +
    facet_grid(~Method)



Education <- socio_data %>%
    select(Education, Method, Transit) %>% 
    table() %>%
    data.frame() %>% 
    group_by(Method) %>% 
    mutate(Percent = Freq/sum(Freq)) %>%
    mutate(Method = case_when(Method == "Whatsapp" ~ "TM",
                              Method == "Snowball" ~ "S",
                              Method == "Crowd Source" ~ "OL")) %>%
    ggplot(aes(y=factor(Education, levels = c("Up to 12th", "Graduate", "PG or Higher")), x=Percent)) +
    geom_col() +
    labs(y = "Education")+
    facet_grid(~Method)

Occupation <- socio_data %>%
    select(Occupation, Method, Transit) %>% 
    table() %>%
    data.frame() %>% 
    group_by(Method) %>% 
    mutate(Percent = Freq/sum(Freq)) %>%
    mutate(Method = case_when(Method == "Whatsapp" ~ "TM",
                              Method == "Snowball" ~ "S",
                              Method == "Crowd Source" ~ "OL")) %>%
    ggplot(aes(y=Occupation, x=Percent)) +
    geom_col() +
    facet_grid(~Method)

Ownership <- socio_data %>%
    select(Ownership, Method, Transit) %>% 
    table() %>%
    data.frame() %>% 
    group_by(Method) %>% 
    mutate(Percent = Freq/sum(Freq)) %>%
    mutate(Method = case_when(Method == "Whatsapp" ~ "TM",
                              Method == "Snowball" ~ "S",
                              Method == "Crowd Source" ~ "OL")) %>%
    ggplot(aes(x=factor(Ownership, levels = c("2 Wheeler", "Car", "Both", "None")), y=Percent)) +
    geom_col() +
    labs(x= "Ownership") +
    facet_grid(~Method) +
    coord_flip()


library(ggpubr)
ggarrange(Gender, AgeGroup, Education, Occupation, Ownership, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)


#5. Travel changes due to COVID19 ====

data %>%
    select(TripFreqChangeCOVID, Transit) %>%
    table() %>% 
    data.frame() %>% 
    mutate(Percent = Freq/sum(Freq)) %>% 
    ggplot(aes(y=TripFreqChangeCOVID, x= Percent)) +
    geom_col()+
    labs(y= "Trip Frequency Change due to COVID-19")+
    theme_classic()


#B. Reading No-Responses Causes Excel sheet ====
file <- "D:/4. IITR/5. Data Collection/10. Online Survey/SendInBlue-Compile/SendInBlue.xlsx"
NoResp <- read_xlsx(file, sheet = 1, col_names = TRUE, skip= 1022403)
colnames(NoResp) <- c("Email", "Name", "Mobile", "Ignore1", "Gender", "Position", "Education", "Salary", "Email", "Whatsapp", "NameCorrection", "RequestSent")

NoResp <- NoResp %>% 
    select("Gender", "Position", "Education", "Salary", "Email", "Whatsapp")

unique <- sort(unique(NoResp$Whatsapp))
NoResp$Whatsapp[NoResp$Whatsapp %in% unique[c(5:7, 13:14)]] <- "Invalid ID"
NoResp$Whatsapp[NoResp$Whatsapp %in% unique[c(4,9:10)]] <- "Not Interested"


NoResp %>% 
    filter(Whatsapp %in% c("Language Barrier", "Not Interested", "Not Targeted")) %>% 
    ggplot(aes(x=Whatsapp)) +
    geom_bar(aes(y = ..prop.., group = 1)) + 
    theme_classic() + 
    coord_flip() +
    labs(y = "Percent", x = "Reason of No Responses")


#test







