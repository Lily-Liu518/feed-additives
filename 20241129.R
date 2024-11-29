#--------------growth---------------------------------
Effect_size_cal_growth <- read_excel("Effect_size_cal_growth.xlsx")
View(Effect_size_cal_growth)
datag<-Effect_size_cal_growth
datag1<-escalc(measure = "SMD", n1i = treat_n, sd1i = treat_sd, m1i = treat_mean,n2i = control_n, sd2i = control_sd, m2i = control_mean, var.names = c("SMD", "vSMD"), data = datag)
datag1_MA <- rma.mv(yi = SMD , V = vSMD, random = list(~1 | Author, ~1 | ID),data = datag1)
summary(datag1_MA)
model_results <- orchaRd::mod_results(datag1_MA, mod = "1", at = NULL, group = "Author")
model_results
orchaRd::orchard_plot(datag1_MA, mod = "1", group = "Author", xlab = "Standardised mean difference",transfm = "none", twig.size = 0.5, trunk.size = 1)
# use i2_sn function to obtain the total I^2
I2 <- orchaRd::i2_ml(datag1_MA)
I2
setwd("C:/Users/v1lliu34/Downloads")
pp<-orchaRd::orchard_plot(model_results, mod = "1", xlab = "Standardised mean difference") +annotate(geom = "text", x = 0.8, y = 1.25, label = paste0("italic(I)^{2} == ", round(I2[1],2), "*\"%\""), color = "black", parse = TRUE, size = 5) + scale_fill_manual(values = "#d40d8c") +
  scale_colour_manual(values = "#d40d8c")
pp
ggsave('growth_overall.pdf',width = 15,height = 12,pp)
or
pdf(file = "growth_overall", height=10, width=15)
plot(test) 
dev.off()