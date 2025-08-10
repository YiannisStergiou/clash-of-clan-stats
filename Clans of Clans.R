library(knitr)
setwd("C:\\Users\\sterg\\Code\\R\\R Projects\\Clash of Clans Stats")
playerdata              <-  read.csv("clania.csv", header = TRUE)

# ------------------------------------------------------------------------------

player_activity         <- playerdata$attackWins > 0
player_activity         <- ifelse(player_activity, "active", "inactive")

# Δημηουργίας πίνακα mydata ο οποίος περιέχει δεδομένα
# για όλους τους παίκτες στο clan
mydata                  <-  data.frame(Name        = playerdata$name,
                                       Level       = playerdata$expLevel,
                                       Trophies    = playerdata$trophies,
                                       `War Stars` = playerdata$warStars,
                                       Activity    = player_activity)

# Δημηουργία του  mydata_active πίνακα ο οποίος περιέχει δεδομένα
# για τους παίκτες που είναι ενεργοί το τελευταίο sesson
x                       <- mydata$Activity == "active"
mydata_active           <- mydata[x, ]
mydata_active           <- mydata_active[order(mydata_active$War.Stars,
                                               decreasing = TRUE), ]
rownames(mydata_active) <- 1:nrow(mydata_active) # nolint

# Ταξινόμηση πινάκων ο οποίος είναι απαραίτητο να γίνει μετά την δημιουργεία του
# mydata_active γιατί κάνει mess up με τα δεδομένα του πίνακα
mydata                  <-  mydata[order(mydata$War.Stars,
                                         decreasing = TRUE), ]
rownames(mydata)        <- 1:nrow(mydata)        # nolint

# ------------------------------------------------------------------------------

# Εδώ καταγράφουμε τα δεδομένα σε ένα αρχείο "Στατιστικά του Clan.txt"
sink("Clan Info.txt")

cat("The following table has data of every member of your clan")
print(kable(mydata,
            align   = "ccccc",
            format  = "simple"))
cat("\n")

if (nrow(mydata_active) == nrow(mydata)) {
  cat("Όλοι οι παίκτες μέσα στο clan είναι active")
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

# Στατιστική Ανάλυση των δεδομένων
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
png(png1, width = 6000, height = 6000, res = 600)

par(mfrow    = c(2, 2))
qqnorm(mydata$Level,
       main  = "Level vs Normal Distribution",
       ylab  = "Level",
       xlab  = "Level Range")
qqline(mydata$Level,
       col   = "Red")
hist(mydata$Level,
     main    = "Συχνότητα στα Levels των παικτών",
     col     = "Light Blue",
     ylab    = "Συχνότητα",
     xlab    = "Level")
boxplot(mydata$Level,
        main = "Κατανομή των Level Παικτών",
        col  = "Light Green",
        ylab = "Level")
plot(density(mydata$Level),
     type    = "h",
     col     = "Light Blue",
     main    = "Πυκνότητα στα Level των παικτών",
     ylab    = "Πυκνότητα",
     xlab    = "Level")

dev.off()

# ------------------------------------------------------------------------------

png2         <-  "War Stars Analysis.png"
png(png2, width = 6000, height = 6000, res = 600)

par(mfrow    = c(2, 2))
qqnorm(mydata$War.Stars,
       main  = "War Stars vs Normal Distribution",
       xlab  = "Κατανομή των Αστεριών",
       ylab  = "War Stars")
qqline(mydata$War.Stars,
       col   = "Red")
hist(mydata$War.Stars,
     main    = "War Stars Frequency",
     col     = "Light Blue",
     xlab    = "Αριθμός Αστεριώn",
     ylab    = "Συχνότητα")
boxplot(mydata$War.Stars,
        main = "Κατανομή των Αστεριών",
        col  = "Light Green",
        ylab = "War Stars")
plot(density(mydata$War.Stars),
     type    = "h",
     col     = "Light Blue",
     main    = "Πυκνότητα των Αστεριών των Παικτών",
     ylab    = "Πυκνότητα",
     xlab    = "War Stars")

dev.off()