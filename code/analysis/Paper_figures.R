##############################
# Setup
#############################

root.path <- '/Github/ALMLGR.github.io'

required_packages <- c("ggplot2", "lmtest", "sandwich", "ggpubr", "data.table", "readstata13")         
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    # Determine missing packages
if(length(not_installed)) install.packages(not_installed)                                           # Install missing packages

suppressWarnings(lapply(required_packages, require, character.only = TRUE))

source(paste0(root.path, '/code/build/clean_data_export_stata.R'))

##############################
# Make Figures for paper
#############################

all.city[, color := ifelse(`Days of NPI` < median(`Days of NPI`, na.rm = T), "More lenient NPIs", "Stricter NPIs")]
all.city$PopGrowth1017 <- all.city$Pop1917/all.city$Pop1910
all.city[, ManuGrowth1419 := CityManuEmp1919/CityManuEmp1914]
all.city[! is.na(`Days of NPI`), residualsManuGrowth := lm(log(ManuGrowth1419) ~ log(PopGrowth1017), data = all.city[! is.na(`Days of NPI`), ])$residuals]
all.city[! is.na(`Days of NPI`), residualsMortality := lm(`Mortality 1918` ~ log(PopGrowth1017), data = all.city[! is.na(`Days of NPI`), ])$residuals]
all.city[! is.na(`Days of NPI`), residualsDaysNPIPopGrowth := lm(`Days of NPI` ~ log(PopGrowth1017), data = all.city[! is.na(`Days of NPI`), ])$residuals]
all.city[! is.na(`Days of NPI`), residualsSpeedNPIPopGrowth := lm(`Speed of NPI` ~ log(PopGrowth1017), data = all.city[! is.na(`Days of NPI`), ])$residuals]
all.city[! is.na(`Days of NPI`), residualsManuEmplPopGrowth := lm(log(ManuGrowth1419) ~ log(PopGrowth1017), data = all.city[! is.na(`Days of NPI`), ])$residuals]

# ggplot parameters
baseSize <- 20
lineSize <- 1.2
pointSize <- 3
positionDodge <- 1

####################################################################################
# Figure 1: relationship of population growth and manufacturing employment growth
####################################################################################

ggplot(all.city[! is.na(`Days of NPI`), ], aes(log(PopGrowth1017), log(ManuGrowth1419), label=City)) + 
    geom_point(aes(color = color), size = 2.5) + 
    geom_smooth(method =  "lm",data =  all.city[! is.na(`Days of NPI`), ], color = "gray", formula = 'y ~ x') + 
    scale_color_manual(values = c("Red", "Dark green")) +
    geom_text(color= "Black", nudge_y = .03) + 
    theme_bw(base_size = baseSize) +
    theme(legend.title = element_blank(), legend.position = "right") +
    xlab("Log population growth 1910-1917") +
    ylab("Log growth manuf. employment 1914-1919") 
ggsave(paste0(root.path, "/results/figure1.pdf"), width = 10.5, height = 7.5)

# Regression estimate
lmfit <- lm(log(ManuGrowth1419) ~ log(PopGrowth1017), all.city)
coeftest(lmfit, vcov = vcovHC(lmfit))

################################################################################
# Figure 2: replication of their figure 7 panel A and residualized side by side
################################################################################
replicationPlotFig2 <- ggplot(all.city[! is.na(color)], aes(`Days of NPI`, log(ManuGrowth1419), label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(color)], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .03) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.box.background = element_rect(colour = "black")) +
  xlab("Days of NPI") +
  ylab("Log growth manuf. employment 1914-1919")+ 
  ylim(-.35, 1.25) + 
  xlim(0,200)
 
residualizedPlotFig2 <- ggplot(all.city[! is.na(color)], aes(residualsDaysNPIPopGrowth, residualsManuEmplPopGrowth, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(color)], color = "gray", formula = 'y ~ x') + 
  scale_color_manual(values = c("Red", "Dark green")) +
  geom_text(color= "Black", nudge_y = .03) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.box.background = element_rect(colour = "black")) +
  xlab("Residual days of NPI") +
  ylab("Residual log manuf. employment growth 1914-1919") +
  ylim(-.35, 1.25)  + 
  xlim(-100,100)

replicationPlotSpeedFig2 <- ggplot(all.city[! is.na(color)], aes(`Speed of NPI`, log(ManuGrowth1419), label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(color)], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .03) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.box.background = element_rect(colour = "black")) +
  xlab("Speed of NPI") +
  ylab("Log growth manuf. employment 1914-1919")+ 
  ylim(-.35, 1.25) + 
  xlim(-40,20)

residualizedPlotSpeedFig2 <- ggplot(all.city[! is.na(color)], aes(residualsSpeedNPIPopGrowth, residualsManuEmplPopGrowth, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(color)], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .03) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.box.background = element_rect(colour = "black")) +
  xlab("Residual speed of NPI") +
  ylab("Residual log manuf. employment growth 1914-1919") +
  ylim(-.35, 1.25) + 
  xlim(-40,20)

ggarrange(replicationPlotFig2, residualizedPlotFig2, replicationPlotSpeedFig2, residualizedPlotSpeedFig2, ncol=2, nrow = 2)
ggsave(paste0(root.path, "/results/figure2.pdf"), width = 15, height = 15)

################################################################################
# Figure 3: Spurious correlation with pre pandemic growth
################################################################################
all.city[, preManuGrowth := log(CityManuEmp1914/CityManuEmp1899)]

lmfit <- lm(preManuGrowth ~ `Days of NPI`, all.city[! is.na(color), ])
coeftest(lmfit, vcov = vcovHC(lmfit))

lmfit <- lm(preManuGrowth ~ `Speed of NPI`, all.city[! is.na(color), ])
coeftest(lmfit, vcov = vcovHC(lmfit))

leftPlotDays <- ggplot(all.city[! is.na(`Days of NPI`), ], aes(`Days of NPI`, ManuGrowth1419, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(`Days of NPI`), ], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .05) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  xlab("Days of NPI") + 
  ylab("Log growth manuf. employment 1914-1919")

rightPlotDays <- ggplot(all.city[! is.na(`Days of NPI`), ], aes(`Days of NPI`, preManuGrowth, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(`Days of NPI`), ], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .05) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  xlab("Days of NPI") +
  ylab("Log growth manuf. employment 1899-1914") 

leftPlotSpeed <- ggplot(all.city[! is.na(`Days of NPI`), ], aes(`Speed of NPI`, ManuGrowth1419, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(`Days of NPI`), ], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .05) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  xlab("Speed of NPI") + 
  ylab("Log growth manuf. employment 1914-1919")

rightPlotSpeed <- ggplot(all.city[! is.na(`Days of NPI`), ], aes(`Speed of NPI`, preManuGrowth, label=City)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method =  "lm",data =  all.city[! is.na(`Days of NPI`), ], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .05) + 
  theme_bw(base_size = baseSize) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  xlab("Speed of NPI") +
  ylab("Log growth manuf. employment 1899-1914") 

ggarrange(leftPlotDays, rightPlotDays, leftPlotSpeed, rightPlotSpeed, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
ggsave(paste0(root.path, "/results/figure3.pdf"), width = 15, height = 15)

################################################################################
# Figure 4 and 5: NPI dynamic diff in diff by city
################################################################################

# Effect of days NPI on employment
stata.estim.correia.npi <- fread(file = paste0(root.path,"/results/correiaEventStudyEmpNPI.txt"), sep = "\t", skip = 1)
stata.estim.correia.npi[, specification := "Correia, Luck, and Verner (2020)"]
stata.estim.control.npi <- fread(file = paste0(root.path,"/results/controlEventStudyEmpNPI.txt"), sep = "\t", skip = 1)
stata.estim.control.npi[, specification := "Controlling for linear pretrend"]
eventStudyPlot.npi <- rbind(stata.estim.control.npi, stata.estim.correia.npi)
eventStudyPlot.npi[, Year := substr(as.character(V1), 1,4)]
eventStudyPlot.npi[, c("b", "min95", "max95") := list(100*b, 100*min95, 100*max95)]
# Add zeros for baseline year
eventStudyPlot.npi <- rbind(eventStudyPlot.npi, list(NA, 0, 0, 0, 0, "Controlling for linear pretrend", 1914))
eventStudyPlot.npi <- rbind(eventStudyPlot.npi, list(NA, 0, 0, 0, 0, "Correia, Luck, and Verner (2020)", 1914))
eventStudyPlot.npi[, Year := as.integer(Year)]

# Effect of days of NPI on output
stata.estim.correia.npi.output <- fread(file = paste0(root.path,"/results/correiaEventStudyOutputNPI.txt"), sep = "\t", skip = 1)
stata.estim.correia.npi.output[, specification := "Correia, Luck, and Verner (2020)"]
stata.estim.control.npi.output <- fread(file = paste0(root.path,"/results/controlEventStudyOutputNPI.txt"), sep = "\t", skip = 1)
stata.estim.control.npi.output[, specification := "Controlling for linear pretrend"]
eventStudyPlot.npi.output <- rbind(stata.estim.control.npi.output, stata.estim.correia.npi.output)
eventStudyPlot.npi.output[, Year := substr(as.character(V1), 1,4)]
# Add zeros for baseline year
eventStudyPlot.npi.output <- rbind(eventStudyPlot.npi.output, list(NA, 0, 0, 0, 0, "Controlling for linear pretrend", 1914))
eventStudyPlot.npi.output <- rbind(eventStudyPlot.npi.output, list(NA, 0, 0, 0, 0, "Correia, Luck, and Verner (2020)", 1914))
eventStudyPlot.npi.output[, c("b", "min95", "max95") := list(100*b, 100*min95, 100*max95)]
eventStudyPlot.npi.output[, Year := as.integer(Year)]

# Effect of speed of NPI on employment
stata.estim.correia.npiSpeed.employment <- fread(file = paste0(root.path,"/results/correiaEventStudyEmpNPISpeed.txt"), sep = "\t", skip = 1)
stata.estim.correia.npiSpeed.employment[, specification := "Correia, Luck, and Verner (2020)"]
stata.estim.control.npiSpeed.employment <- fread(file = paste0(root.path,"/results/controlEventStudyEmpNPISpeed.txt"), sep = "\t", skip = 1)
stata.estim.control.npiSpeed.employment[, specification := "Controlling for linear pretrend"]
eventStudyPlot.npiSpeed.employment <- rbind(stata.estim.control.npiSpeed.employment, stata.estim.correia.npiSpeed.employment)
eventStudyPlot.npiSpeed.employment[, Year := substr(as.character(V1), 1,4)]
# Add zeros for baseline year
eventStudyPlot.npiSpeed.employment <- rbind(eventStudyPlot.npiSpeed.employment, list(NA, 0, 0, 0, 0, "Controlling for linear pretrend", 1914))
eventStudyPlot.npiSpeed.employment <- rbind(eventStudyPlot.npiSpeed.employment, list(NA, 0, 0, 0, 0, "Correia, Luck, and Verner (2020)", 1914))
eventStudyPlot.npiSpeed.employment[, c("b", "min95", "max95") := list(100*b, 100*min95, 100*max95)]
eventStudyPlot.npiSpeed.employment[, Year := as.integer(Year)]

# Effect of speed of NPI on output 
stata.estim.correia.npiSpeed.output <- fread(file = paste0(root.path,"/results/correiaEventStudyOutputNPISpeed.txt"), sep = "\t", skip = 1)
stata.estim.correia.npiSpeed.output[, specification := "Correia, Luck, and Verner (2020)"]
stata.estim.control.npiSpeed.output <- fread(file = paste0(root.path,"/results/controlEventStudyOutputNPISpeed.txt"), sep = "\t", skip = 1)
stata.estim.control.npiSpeed.output[, specification := "Controlling for linear pretrend"]
eventStudyPlot.npiSpeed.output <- rbind(stata.estim.control.npiSpeed.output, stata.estim.correia.npiSpeed.output)
eventStudyPlot.npiSpeed.output[, Year := substr(as.character(V1), 1,4)]
# Add zeros for baseline year
eventStudyPlot.npiSpeed.output <- rbind(eventStudyPlot.npiSpeed.output, list(NA, 0, 0, 0, 0, "Controlling for linear pretrend", 1914))
eventStudyPlot.npiSpeed.output <- rbind(eventStudyPlot.npiSpeed.output, list(NA, 0, 0, 0, 0, "Correia, Luck, and Verner (2020)", 1914))
eventStudyPlot.npiSpeed.output[, c("b", "min95", "max95") := list(100*b, 100*min95, 100*max95)]
eventStudyPlot.npiSpeed.output[, Year := as.integer(Year)]

panelAFig4 <- ggplot(eventStudyPlot.npi, aes(Year, b, group = specification, color = specification)) + 
  geom_errorbar(aes(ymin = min95, ymax = max95), width=1, position=position_dodge(positionDodge), size = lineSize) +
  theme_bw(base_size = baseSize) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box="Horizontal", legend.margin=margin(t = -1, unit='cm')) + 
  guides(color=guide_legend(ncol=1,nrow=2,byrow=TRUE)) + # Make legend elements stacked veryically instead of horizontally
  geom_point(position=position_dodge(positionDodge),  size = pointSize) + 
  geom_line(position=position_dodge(positionDodge), size = lineSize) + 
  scale_colour_manual(values = c("dark green", "dark blue")) + 
  xlab("") + 
  ylab("Coefficient Estimate") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = c(1899, 1904, 1909, 1914, 1919, 1921, 1923, 1925, 1927), limits = c(1898, 1928)) +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Log manufacturing employment on Days of NPI") +  
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(legend.text=element_text(size=24), 
        legend.margin=margin(0,0,0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

panelBFig4 <- ggplot(eventStudyPlot.npi.output, aes(Year, b, group = specification, color = specification)) + 
  geom_errorbar(aes(ymin = min95, ymax = max95), width=1, position=position_dodge(positionDodge), size = lineSize) +
  theme_bw(base_size = baseSize) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box="Horizontal", legend.margin=margin(t = -1, unit='cm')) + 
  guides(color=guide_legend(ncol=1,nrow=2,byrow=TRUE)) + # Make legend elements stacked veryically instead of horizontally
  geom_point(position=position_dodge(positionDodge),  size = pointSize) + 
  geom_line(position=position_dodge(positionDodge), size = lineSize) + 
  scale_colour_manual(values = c("dark green", "dark blue")) + 
  xlab("") + 
  ylab("") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = c(1899, 1904, 1909, 1914, 1919, 1921, 1923, 1925, 1927), limits = c(1898, 1928)) +
  theme(panel.grid.major.x = element_blank()) + 
  ggtitle("Log manufacturing output on Days of NPI") +  
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(legend.text=element_text(size=24), 
        legend.margin=margin(0,0,0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

panelAFig5 <- ggplot(eventStudyPlot.npiSpeed.employment, aes(Year, b, group = specification, color = specification)) + 
  geom_errorbar(aes(ymin = min95, ymax = max95), width=1, position=position_dodge(positionDodge), size = lineSize) +
  theme_bw(base_size = baseSize) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box="Horizontal", legend.margin=margin(t = -1, unit='cm')) + 
  guides(color=guide_legend(ncol=1,nrow=2,byrow=TRUE)) + # Make legend elements stacked veryically instead of horizontally
  geom_point(position=position_dodge(positionDodge),  size = pointSize) + 
  geom_line(position=position_dodge(positionDodge), size = lineSize) + 
  scale_colour_manual(values = c("dark green", "dark blue")) + 
  xlab("") + 
  ylab("") + 
  ylab("Coefficient Estimate") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = c(1899, 1904, 1909, 1914, 1919, 1921, 1923, 1925, 1927), limits = c(1898, 1928)) +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Log manufacturing employment on Speed of NPI") +  
  theme(legend.text=element_text(size=24), 
        legend.margin=margin(0,0,0,0)) +
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

panelBFig5 <- ggplot(eventStudyPlot.npiSpeed.output, aes(Year, b, group = specification, color = specification)) + 
  geom_errorbar(aes(ymin = min95, ymax = max95), width=1, position=position_dodge(positionDodge), size = lineSize) +
  theme_bw(base_size = baseSize) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box="Horizontal", legend.margin=margin(t = -1, unit='cm')) + 
  guides(color=guide_legend(ncol=1,nrow=2,byrow=TRUE)) + # Make legend elements stacked veryically instead of horizontally
  geom_point(position=position_dodge(positionDodge),  size = pointSize) + 
  geom_line(position=position_dodge(positionDodge), size = lineSize) + 
  scale_colour_manual(values = c("dark green", "dark blue")) + 
  xlab("") + 
  ylab("") + 
  geom_hline(yintercept = 0) + 
  theme(panel.grid.major.x = element_blank()) + 
  ggtitle("Log manufacturing output on Speed of NPI") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1899, 1904, 1909, 1914, 1919, 1921, 1923, 1925, 1927), limits = c(1898, 1928)) +
  theme(legend.text=element_text(size=24), 
        legend.margin=margin(0,0,0,0)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(panelAFig4, panelBFig4,  ncol=2, common.legend = TRUE, legend="bottom")
ggsave(paste0(root.path, "/results/figure4.pdf"), width = 18, height = 12)

ggarrange(panelAFig5, panelBFig5,  ncol=2, common.legend = TRUE, legend="bottom")
ggsave(paste0(root.path, "/results/figure5.pdf"), width = 18, height = 12)
