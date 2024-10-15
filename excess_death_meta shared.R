#3 China meta analysis
rm(list=ls())
setwd("/Users/hxiao2/Desktop/AAA_Research Projects/China excess deaths_meta/final analysis")
library(foreign)

### Meta
library(metafor)
library(meta)
df<-read.csv("primary_meta_excess_death.csv")

res <- rma(yi = log_RR, sei = log_SE,data=df[1:7,], method = "REML")
summary(res)

meta_obj <- metagen(
       TE = log_RR, 
      seTE =log_SE,
       data=df[1:7,],
       studlab = NULL,
       comb.fixed = FALSE,
       comb.random = TRUE,
       hakn = FALSE,
       prediction = FALSE,
       sm = "RR",
       method.tau = "DL",
       hakn.single = FALSE
   )

### leave one out sensitivity analysis
leave1out_results <- leave1out(res)
print(leave1out_results)

metafor::forest(leave1out_results)

leave1out_df <- data.frame(
  yi = leave1out_results$estimate,  # Effect sizes
  ci.lb = leave1out_results$ci.lb,  # Lower confidence intervals
  ci.ub = leave1out_results$ci.ub,  # Upper confidence intervals
  study = df[1:7,"author"]  # Study labels
)

# Create a forest plot for the leave-one-out sensitivity analysis
metafor::forest(data=leave1out_df,leave1out_df$yi, ci.lb = leave1out_df$ci.lb, ci.ub = leave1out_df$ci.ub,
       slab = leave1out_df$study, xlab = "Effect size",
       ticks_at = c(log(0.5), log(1), log(1.5), log(2), log(2.5)),
      xlim = c(log(0.45), log(2.75)),
       #ci_column = 2,
      # ref_line =1,
       refline = res$b,  # Overall effect estimate from the original meta-analysis
       header = TRUE, theme = tm,atransf = exp)

par(mar = c(5, 4, 4, 2) + 0.1)
p_s<-metafor::forest(data=leave1out_df,exp(leave1out_df$yi), 
                     ci.lb = exp(leave1out_df$ci.lb), ci.ub = exp(leave1out_df$ci.ub),
                slab = leave1out_df$study, 
                xlab = "Risk Ratio",
                at = c(1, 1.5, 2),
                xlim = c(0.5, 2.5),
                cex=1,
                #width = 1,
                #ci_column = 3,
                refline=exp(0.3815),# Overall effect estimate from the original meta-analysis
                theme = tm,
                header = F)
text(
  x = 0.50, # Adjust x position to fit within plot area
  y = nrow(leave1out_df) + 1.4,
  labels = "Study excluded",
  pos = 4,
  cex = 1,
  font = 2 # Bold font
)


text(
  x = 2.0, # Adjust x position to fit within plot area
  y = nrow(leave1out_df) + 1.9,
  labels = "  Risk ratio",
  pos = 4,
  cex = 1,
  font = 2 # Bold font
)


text(
  x = 2.0, # Adjust x position to fit within plot area
  y = nrow(leave1out_df) + 1.4,
  labels = "with 95% CI",
  pos = 4,
  cex = 1,
  font = 2 # Bold font
)



library(grid)
library(forestploter)
library(gtable)
library(gridtext)
library(grid)
library(metafor)

forest.rma(res, atransf = exp, xlab = "Relative Risk (RR)",slab=df[1:7,]$author,
           addfit = TRUE, showweights = TRUE,annotate=TRUE,addpred=F,header=T,
          xlim=c(log(0.5),log(3)),arrow_lab = c("Better","Worse")) # arrow_lab = c("Better","Worse"), after abandoning Zero-COVID

###https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-post.html
###https://www.metafor-project.org/doku.php/plots:forest_plot_revman

df<-read.csv("primary_meta_excess_death.csv")
df$cicol <- paste(rep(" ", 24), collapse = " ")
dt_fig <- df[,c("author","weight","rr_ci","cicol","rob_s",	"rob_c",	"rob_o")]
dt_fig$weight <- sprintf("%0.1f%%", dt_fig$weight)
dt_fig$weight[dt_fig$weight == "NA%"] <- ""
colnames(dt_fig) <- c("Study",  "Weight", " ", "",
                      LETTERS[c(19,3,15)])
dt_fig[is.na(dt_fig)] <- ""
#df[is.na(df)] <- ""

#pdf("plot.pdf",family ="Arial", width = 10, height = 8)
# Background to white and summary diamond to black
tm <- forest_theme(core = list(bg_params=list(fill = c("white"))),
                   summary_col = "black",
                   arrow_label_just = "end",
                   arrow_type = "closed",base_family ="Arial")

#par(mar = c(2, 1, 2, 1))
#par(mar=c(2.8,0,1.3,1.3), mgp=c(3,0.2,0), tcl=-0.2)
p <- forestploter::forest(dt_fig,
            est = df$est,
            lower = df$lb, 
            upper = df$ub,
            sizes = c(sqrt((df$weight[1:12]+3)/75)+0.1,0.90),
            is_summary = c(rep(F, nrow(df)-1), T),
            ci_column = 4,
            ref_line =1,
            x_trans = "log",
            arrow_lab = c("Lower mortality","Higher mortality"),
            xlim = c(0.5, 4),
            ticks_at = c(0.5, 1,  2, 4),
            theme = tm)

#p

# Change font face
g <- edit_plot(p, row = 9, 
               gp = gpar(fontface = "bold"))

# Change color
g <- edit_plot(g, col = 4, row = 9, which = "ci", 
               gp = gpar(col = "blue", fill = "blue"))

# Change the background of the total row
g <- edit_plot(g, col = 1:3, 
               row = 9, 
               which = "background", 
               gp = gpar(fill = "#f6eff7"))

# Align texts to center
g <- edit_plot(g, col = 5:7, 
               which = "text",
               # gp = gpar(),
               hjust = unit(0.5, "npc"),
               x = unit(0.5, "npc"))

### Add or insert some text to the header on top of CI columns
g <- add_text(g, text = "IV, Random, 95% CI",
              part = "header", 
              col = 3:4,
              gp = gpar(fontface = "bold"))

g <- insert_text(g, text = "Risk ratio (RR)",
                 part = "header", 
                 col = 3:4,
                 gp = gpar(fontface = "bold"))

g <- add_text(g, text = "Risk of Bias",
              part = "header", 
              row = 1,
              col = 5:7,
              gp = gpar(fontface = "bold"))

# Add or insert some text to the header
g <- add_border(g, 
                part = "header", 
                row = 1,
                col = 5:7,
                gp = gpar(lwd = .5))

g <- add_border(g, 
                part = "header", 
                row = 2,
                gp = gpar(lwd = 1))

## 
g <- add_grob(g, 
              row = 1:c(nrow(dt_fig) - 1), 
              col = 5:7,
              order = "background",
              gb_fn = roundrectGrob,
              r = unit(0.05, "snpc"),
              gp = gpar(lty = "dotted",
                        col = "#bdbdbd"))

# Draw a circle grob, you can also draw a `pointsGrob`
cols <- c("#eeee00", "#00cc00", "#cc0000")
symb <- c("?", "+", "-")
for(i in seq_along(symb)){
  pos <- which(dt_fig == symb[i], arr.ind=TRUE)
  for(j in 1:nrow(pos)){
    g <- add_grob(g, 
                  row = pos[j,1], 
                  col = pos[j,2],
                  order = "background",
                  gb_fn = circleGrob,
                  r = 0.4,
                  gp = gpar(fill = cols[i]))
  }
}

#g
# tau^2 (estimated amount of total heterogeneity): 0.0222 (SE = 0.0133)
# tau (square root of estimated tau^2 value):      0.1491
# I^2 (total heterogeneity / total variability):   93.95%
# H^2 (total variability / sampling variability):  16.53

# Test for Heterogeneity:
#   Q(df = 10) = 514.1802, p-val < .0001

txt <- bquote(atop(paste("Heterogeneity: ", tau^2, " = 0.007; ",
                         chi^2, " = 25.1, df = 6 (p =.0003); ",
                         I^2, " = 71.1%"),"Total for overall effect: Z = 8.04(p<.0001)"))

g2<-add_text(g, text = txt,
         col = 1:3,
         row = 10,
         just = "left",
         parse = TRUE,
         gp = gpar(fontsize = 8))


###
library(grid)
#install.packages("gridtext")
library("gridtext")
#txt <- "Heterogeneity: &tau;<sup>2</sup> = 0.02; &chi;<sup>2</sup> = 514.2,df=10 (P<.0001);I<sup>2</sup> = 94.0%<br><span style='color:black'>**Test for overall effect:**</span> Z = 8.03(P<.0001)"

#spaces <- str_pad("", width = 5, side = "right")
txt <- "<br>Heterogeneity: &tau;<sup>2</sup> = 0.007; &chi;<sup>2</sup> = 25.1, df=6 (P=.0003);
I<sup>2</sup> = 71.1%<br>
<span style='color:black'>Test for overall effect:</span> Z = 8.04 (P<.0001) <br>
<br>
**Risk of bias legend:**<br>
(S) Selection;  (C) Comparability;  (O) Outcome <br>
(+) Good . . .;  ( ?) Fair . . . . . . . .;  ( - ) Poor"

g2<-add_grob(g, 
         row = 10, 
         col = 1:2,
         order = "background",
         gb_fn = gridtext::richtext_grob,
         text = txt,
         gp = gpar(fontsize = 8),
         hjust = 0, vjust = 1, halign = 0, valign = 1,
         x = unit(0, "npc"), y = unit(1, "npc"))

loadfonts(device = "pdf")
pdf("plot.pdf", width = 10,family="Arial", fonts="Arial",height = 8)
g2
dev.off()



################################################################################
################################################################################
### sim/reg vs. sur
################################################################################
################################################################################
df<-read.csv("primary_meta_excess_death.csv")
df$method<-factor(df$method,levels=c("sim_reg","sur"))
metafor::forest(res, xlim=c(-6, 4.0), at=log(c(0.5, 1,2, 4)), atransf=exp,slab=df[1:7,]$author,
                cex=0.9, 
                rows=c(13:10,5:3),ylim=c(-1,17),
                order=method,
                mlab=mlabfun("RE Model for All Studies", res),
                cex.mlab=0.5,showweights = TRUE,digits=c(2L,1,1),
                psize=1, header=c("Study","Risk Ratio [95% CI]"),xlab="Risk Ratio",col="blue")  #ylim=c(-1, 27), rows=c(3:4,9:15,20:23),

text(-6, c(14,6), pos=4, c("Simulation / Regression Informed by Empirical Data",
                             # "Regression and / or Extrapolation",
                              "Survey / Surveillance Motality Data"), font=4,cex=0.9)

### fit random-effects model in the three subgroups
res.sim <- rma(yi = log_RR, sei = log_SE,data=df[1:7,],subset=(method=="sim_reg"))
res.sur <- rma(yi = log_RR, sei = log_SE,data=df[1:7,],subset=(method=="sur"))

### add summary polygons for the three subgroups
addpoly(res.sim, row=8.5, mlab=mlabfun("RE Model for Subgroup", res.sim),cex=0.9)
addpoly(res.sur, row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.sur),cex=0.9)


### fit meta-regression model to test for subgroup differences
res_method <- rma(yi = log_RR, sei = log_SE,data=df[1:7,], mods = ~ method)

### add text for the test of subgroup differences
text(-6, -1.5, pos=4, cex=0.9, bquote(paste("Test for Subgroup Differences: ",
                                            Q[M], " = ", .(fmtx(res_method$QM, digits=2)),
                                            ", df = ", .(res_method$p - 1), ", ",
                                            .(fmtp(res_method$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))

