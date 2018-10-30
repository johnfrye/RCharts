# Plotting ----------------------------------------------------------------

# helpful interactive view of par
listviewer::jsonedit(par())

# vector graphics

# pixels to inches conv
filename <- "SM Net GP Pct.pdf"
pix.width <-  1513
pix.height <-  932
pix.to.inches <-  0.010416667

ggsave(fp(gd, filename), g, width = pix.width * pix.to.inches, 
       height = pix.height * pix.to.inches)

# save all plots in display to one pdf
time <- time_stamp()
(pdfname <- paste0(title, " ", time, ".pdf"))
dev.copy(pdf, fp(gd, pdfname), width=11, height=8.5)
dev.off()

# save HTML
sink(fp(gd, "hypothesis_timeline.html"), append=FALSE)
hypo_graph
sink()

# bitmap graphical format
# without Cairo, Windows will produce non-anti-aliased graphics
# Cairo produces bitmap images with transparency, alpha levels, and anti-aliasing
png_name <- "test.png"
CairoPNG(file = fp(gd, "plotname.png"), width = 792, height = 612)
print(us.map.object)
dev.off()

# with ggplot objects
ggsave(plot = g, "Cairo ggsave.png", h = 9/3, w = 16/3, type = "cairo-png")

# color
pal <- choose_palette()
pal(4)
display.brewer.all()
colors <- brewer.pal(9, "Set1")

# To make a gif from plots:
#3 First make plots.
for(p in 1:20){
  png(paste0('img', p, '.png')  )
  plot(1:9, 1:9, pch = p)
  dev.off() 
}

# set graphics to plot 2 rows of two graphics each
par(mfrow=c(2,2))
