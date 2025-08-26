library(knitr)
setwd("C:\\Users\\sterg\\Code\\R\\R Projects\\Clash of Clans Stats")
playerdata              <-  read.csv("data.csv", header = TRUE)

# ------------------------------------------------------------------------------

player_activity         <- playerdata$attackWins > 0
player_activity         <- ifelse(player_activity, "active", "inactive")

# Create the main dataframe 'mydata' containing all clan members.
mydata                  <-  data.frame(Name        = playerdata$name,
                                       Level       = playerdata$expLevel,
                                       Trophies    = playerdata$trophies,
                                       `War Stars` = playerdata$warStars,
                                       Activity    = player_activity)

# Create the 'mydata_active' dataframe,
# filtering for players active in the current season.
x                       <- mydata$Activity == "active"
mydata_active           <- mydata[x, ]
mydata_active           <- mydata_active[order(mydata_active$War.Stars,
                                               decreasing = TRUE), ]
rownames(mydata_active) <- 1:nrow(mydata_active) # nolint

# Sort the main 'mydata' dataframe.
# This is done after creating the active players subset
# to ensure the original dataframe's row order
# is not disturbed during the filtering process.
mydata                  <-  mydata[order(mydata$War.Stars,
                                         decreasing = TRUE), ]
rownames(mydata)        <- 1:nrow(mydata)        # nolint

# ------------------------------------------------------------------------------

# Output the summary data to a text file named "Clan Info.txt".
sink("Clan Info.txt")

cat("The following table has data of every member of your clan")
print(kable(mydata,
            align   = "ccccc",
            format  = "simple"))
cat("\n")

if (nrow(mydata_active) == nrow(mydata)) {
  cat("All clan members are active in the current season.")
  cat("\n")

} else {
  cat("The following table has data of active members of your clan")
  print(kable(mydata_active,
              format  = "simple",
              align   = "ccccc"))
}
cat("\n")
cat("#########################################################################")
cat("\n\n")
cat("Statistical analysis")
cat("\n\n")

# Perform and print summary statistics.
cat("Player Level")
cat("\n")
print(summary(mydata$Level))
cat("\n")
cat("War Stars")
cat("\n")
print(summary(mydata$War.Stars))
sink()

# ------------------------------------------------------------------------------

png1         <- "Level Anlalysis.png"
png(png1, width = 4000, height = 4000, res = 600)

par(mfrow    = c(2, 2))
qqnorm(mydata$Level,
       main  = "Level vs Normal Distribution",
       ylab  = "Level",
       xlab  = "Level Range")
qqline(mydata$Level,
       col   = "Red")
hist(mydata$Level,
     main    = "Player Level Frequency",
     col     = "Light Blue",
     ylab    = "Frequency",
     xlab    = "Level")
boxplot(mydata$Level,
        main = "Player Level Distribution",
        col  = "Light Green",
        ylab = "Level")
plot(density(mydata$Level),
     type    = "h",
     col     = "Light Blue",
     main    = "Player Level Density",
     ylab    = "Density",
     xlab    = "Level")

dev.off()

# ------------------------------------------------------------------------------

png2         <-  "War Stars Analysis.png"
png(png2, width = 4000, height = 4000, res = 600)

par(mfrow    = c(2, 2))
qqnorm(mydata$War.Stars,
       main  = "War Stars vs Normal Distribution",
       xlab  = "Theoretical Quantiles",
       ylab  = "War Stars")
qqline(mydata$War.Stars,
       col   = "Red")
hist(mydata$War.Stars,
     main    = "War Stars Frequency",
     col     = "Light Blue",
     xlab    = "Number of Stars",
     ylab    = "Frequency")
boxplot(mydata$War.Stars,
        main = "War Star Distribution",
        col  = "Light Green",
        ylab = "War Stars")
plot(density(mydata$War.Stars),
     type    = "h",
     col     = "Light Blue",
     main    = "Player War Star Density",
     ylab    = "Density",
     xlab    = "War Stars")

dev.off()