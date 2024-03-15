setwd("/Users/Downloads/covid/0data")
############################################################################################
############Convert week in data to date(Convert x-axis units)##############################
############################################################################################
install.packages("lubridate")
library(lubridate)

#Set start date
start_date <- as.Date("2022-01-01")

date_df <- data.frame(Date = character())

#Generate dates for the first day of each week up to week 65 and store into a dataframe
for (i in 1:65) {
  current_week_first_day <- start_date + weeks(i - 1)
  formatted_date <- format(current_week_first_day, "%B %d, %Y")
  date_df <- rbind(date_df, data.frame(Date = formatted_date))
}

write.csv(date_df, file = "weekly_dates.csv", row.names = FALSE)

############################################################################################
##############Overall statistical correlation of tweets numbers and real new cases##########
################Kendall's tau coefficient & P value#########################################
############################################################################################
CR <- read.csv(file = "weekly_rate_cough.csv", stringsAsFactors = TRUE)
DR <- read.csv(file = "weekly_rate_diarrhea.csv", stringsAsFactors = TRUE)
FR <- read.csv(file = "weekly_rate_fever.csv", stringsAsFactors = TRUE)
TR <- read.csv(file = "weekly_rate_throat.csv", stringsAsFactors = TRUE)
HR <- read.csv(file = "weekly_rate_headache.csv", stringsAsFactors = TRUE)
VR <- read.csv(file = "weekly_rate_vomit.csv", stringsAsFactors = TRUE)

tau_fever <- cor.test(FR$sum_count, FR$WHO_count, method = "kendall")
tau_cough <- cor.test(CR$sum_count, CR$WHO_count, method = "kendall")
tau_diarrhea <- cor.test(DR$sum_count, DR$WHO_count, method = "kendall")
tau_headache <- cor.test(HR$sum_count, HR$WHO_count, method = "kendall")
tau_vomit <- cor.test(VR$sum_count, VR$WHO_count, method = "kendall")
tau_throat <- cor.test(TR$sum_count, TR$WHO_count, method = "kendall")

########################################################################################
################Weekly Negative Rate(line chart in 6 different symptoms)################
########################################################################################
library(ggplot2)
max_value <- max(VR$weekly_rate)
print(max_value)

a <- ggplot() +
  geom_line(data = CR, aes(x = week, y = weekly_rate, colour = "Cough"), size = 0.9) +
  geom_line(data = DR, aes(x = week, y = weekly_rate, colour = "Diarrar"), size = 0.9) + 
  geom_line(data = FR, aes(x = week, y = weekly_rate, colour = "Fever"), size = 0.9) + 
  geom_line(data = TR, aes(x = week, y = weekly_rate, colour = "Throat"), size = 0.9) +
  geom_line(data = HR, aes(x = week, y = weekly_rate, colour = "Headache"), size = 0.9) +
  geom_line(data = VR, aes(x = week, y = weekly_rate, colour = "Vomit"), size = 0.9) +
  scale_colour_manual("Symptom", values = c("Cough" = "#6f99adff", "Diarrar" = "#AFDAA6", "Fever" = "#BC3C29CC", "Throat" = "#E18727CC","Headache"="#20854ECC","Vomit"="#cfb790")) +
  xlab("Date") +
  ylab("Weekly Rate") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),  
    legend.box.background = element_rect(color = "black")#Legend border color #The code in the theme is used to remove grid lines and retain axis borders
  ) +
  ggtitle("Weekly Negative Rate") +
  scale_x_continuous(breaks = seq(0, max(TR$week, na.rm = TRUE), by = 4), expand = c(0, 0)) +  # Set the x-axis range to start from 1, the tick interval to 4, and the expand parameter to c(0, 0)
  scale_y_continuous(breaks = seq(0, 2, by = 0.2), limits = c(0, 0.9), expand = c(0, 0))  # Set the y-axis range from 0 to 1.0, the tick interval to 0.2, and the expand parameter to c(0, 0)
a

ggsave(filename = "new_6all_weekly_rate.pdf", plot = a, width = 10, height = 5, dpi = 500, bg='white')

###########################################################################
##############COVID-19 Variants percentage deposit area chart##############
###########################################################################
library(ggplot2)
prop <- read.csv(file = "variants_prop_all_100.csv", stringsAsFactors = TRUE)
head(prop)
library(tidyr)

#Convert data frame to long format
long <- gather(prop, key = "variable", value = "value", -week)

#Generate gradient color
colors <- colorRampPalette(c("#403990","white","#CF3D3E"))(8)
colors

#Draw a stacked area chart
p <- ggplot(long, aes(x = week, y = value, fill = variable)) +
  geom_area(size = 0.3, color = "darkgray") +
  scale_fill_manual(values = c("#403990","#7671AF","#ADAACF","#E3E2EF","#F8E3E3","#EAABAC","#DC7475","#CF3D3E")) +
  xlab("Date") +
  ylab("Proportion(%)") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),  #Title is centered
    legend.box.background = element_rect(color = "black")
  ) +
  ggtitle("Stacked Area Chart") +
  scale_x_continuous(breaks = seq(0, max(prop$week, na.rm = TRUE), by = 4), expand = c(0, 0))+#Set the x-axis range to start from 1, the tick interval to 4, and the expand parameter to c(0, 0)
  scale_y_continuous(breaks = seq(0, 100, by = 25), expand = c(0, 0)) 
p
ggsave(filename = "prop_all_varients.png", plot = p, width = 10, height = 5, dpi = 500, bg='white')

########################################################
############## 4 line chart into 1 picture##############
########################################################
##import the ggplot’s arrangement drawing package
install.packages("ggpubr")
library(ggpubr)
values = c("Cough" = "#9BAACA","Diarrar" = "#AFDAA6","Fever"="#DE8B96","Throat"="#E9C69B")

p1 <-ggplot()+geom_line(data = CR,aes(x = week,y = weekly_rate),colour = "#9BAACA",size=0.7)+
  ylim(0,.4)+
  xlab("Date")+ylab("Weekly Rate")+
  theme_bw()+ #Remove background gray
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position="none")+ #Legend border color #The code here is used to remove grid lines and retain coordinate axis borders.
  ggtitle("Cough")
p1

p2 <- ggplot() +
  geom_line(data = DR, aes(x = week, y = weekly_rate), color = "#AFDAA6", size = 0.7) +
  ylim(0.1, 0.7) +
  xlab("Date") +
  ylab("Weekly Rate") +
  theme_bw() + 
  theme(
    panel.grid.major = element_line(colour = NA),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  ggtitle("Diarrhea")

p2


p3 <-ggplot()+geom_line(data = FR,aes(x = week,y = weekly_rate),colour = "#DE8B96",size=0.7)+
  ylim(.1,.6)+
  xlab("Date")+ylab("Weekly Rate")+
  theme_bw()+ 
  theme(panel.grid.major=element_line(colour=NA),plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position="none")+ 
  ggtitle("Fever")
p3

p4 <-ggplot()+geom_line(data = TR,aes(x = week,y = weekly_rate),colour = "#E9C69B",size=0.7)+
  ylim(.3,.6)+
  xlab("Date")+ylab("Weekly Rate")+
  theme_bw()+ 
  theme(panel.grid.major=element_line(colour=NA),plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.position="none")+ 
  ggtitle("Throat")
p4

#Combine 4 pictures into 1 picture of 2*2
p_all <- ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,labels=c("(A)","(B)","(C)","(D)"))
p_all
ggsave(filename = "new_seperate_weekly_rate.png", plot = p_all, width = 8, height = 6, dpi = 400, bg='white')
