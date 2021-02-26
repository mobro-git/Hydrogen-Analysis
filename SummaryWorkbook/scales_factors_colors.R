bottom1 <- theme(legend.position = "bottom")
bottom2 <- guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

yt <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9)) 

st <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

nolegend <- theme(legend.position='none')

noaxes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

zero <- geom_hline(yintercept = 0, linetype = "dashed", color = "red")

x_cont <- scale_x_continuous(breaks = seq(2010,2050, by = 5), expand = c(0,1))
x_disc <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2))
x_disc_l <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2),
                             labels = c("2010", "","2020", "", "2030", "", "2040", "", "2050"))

x_cont_scenario <- scale_x_continuous(breaks = seq(2010,2050, by = 5), expand = c(0,1))
x_disc_scenario <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2))

col_elc <- c(`Wind` = "#92CBF3", `Hydro` = "dodgerblue4", 
             `Solar` = "darkgoldenrod2", `Nuclear` = "darkorange3", 
             `Coal` = "gray9", `Natural Gas` = "darkslategray4", 
             `Coal CCS` = "gray", `Other` = "#F3C0D4")

elc_fill <- scale_fill_manual(values = col_elc)
elc_color <- scale_color_manual(values = col_elc)

techlabels <- c(
  "H2CCNGA" = "Cen Nat Gas",
  "H2CCNGACCS" = "Cen Nat Gas CCS",
  "H2CFNGA" = "Cen Future Nat Gas",  
  "H2CFNGACCS" = "Cen Future Nat Gas CCS", 
  "H2DCETH" = "Dist Ethanol.", 
  "H2DCNGA" = "Dist Nat Gas", 
  "H2DFEPEM" = "Dist Future PEM",
  "H2DFETH" = "Dist Future Ethanol",
  "H2DFNGA" = "Dist Future Nat Gas")

airlabels <- c(
  "CO[2]" = expression(CO[2]),
  "SO[2]" = expression(SO[2]),
  "NO[X]" = expression(NO[X]),
  "CH[4]" = expression(CH[4]),
  "PM[2.5]" = expression(PM[2.5])
)