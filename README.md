# cost-of-living_AST
Analysis of the cost of living for Astronomy peer programs

# Analysis of rent data for 1-bedroom apartments in Austin and areas proximal 
# institutions that compete with Astronomy for students.

# These areas/institutions include Ann Arbor (Michigan), Columbus (Ohio State),
# New Haven (Yale), Harrisburg (Penn State), Charlottsville (Virgina), and Baltimore 
# (Johns Hopkins)

# UT Color pallette
UTpalette <- c( "darkorange4",
                "white",
                "grey2",
                "orange2",
                "gold",
                "darkolivegreen1",
                "green4",
                "darkturquoise",
                "steelblue4",
                "lightsteelblue3",
                "seashell2")

# Create data frame "rent" from CSV file w/headers
rent <- read.csv("Rent.csv", header = T)

# Examine the structure of Rent.csv
summary(rent)

# Make a histogram of rent
hist(rent$Rent,
     col = "seashell2",
     main = "Mean monthly rent for 1 BR apartments in Austin\nand areas near peer institutions between 2014-2017",
     xlab = "Monthly rent ($)")

# Make a box plot of rent
boxplot(rent$Rent,
        col = "seashell2",
        notch = T,
        horizontal = T,
        main = "Monthly rent for 1 BR apartments in Austin\nand areas near peer institutions between 2014-2017",
        xlab = "Monthly rent ($)")

# How does rent vary by location?
# Split the rent data based on location:
rent.city <- split(rent$Rent, rent$Location)

# Draw boxplots of rent by location
boxplot(Rent ~ Location, data = rent,
        col = "lightsteelblue3",
        notch = T,
        xlab = "Location",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartments by city\n2014-2018")

# Draw boxplots of rent by year
boxplot(Rent ~ Year, data = rent,
        col = "lightsteelblue3",
        notch = T,
        xlab = "Year",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartment by year\nin Austin and cities with AST program peer programs")

# Recreate data set without boston
buckfoston <- read.csv("Rent_noBoston.csv", header = T)

# Make a histogram of buckfoston
hist(buckfoston$Rent,
     col = "seashell2",
     main = "Mean monthly rent for 1 BR apartments in Austin\nand areas near peer institutions between 2014-2017",
     xlab = "Monthly rent ($)")

# Make a box plot of buckfoston
boxplot(buckfoston$Rent,
        col = "lightsteelblue3",
        notch = T,
        horizontal = F,
        main = "Monthly rent for 1 BR apartments in Austin\nand areas near peer institutions between 2014-2017",
        xlab = "Monthly rent ($)")

# Draw boxplots of rent by location, Boston excepted
boxplot(Rent ~ Location, data = buckfoston,
        col = "lightsteelblue3",
        notch = F,
        xlab = "Location",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartments by city\n2014-2018")

# Draw boxplots of rent by year, Boston excepted
boxplot(Rent ~ Year, data = buckfoston,
        col = "lightsteelblue3",
        notch = F,
        xlab = "Year",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartment by year\nin Austin and cities with AST peer programs")

# Pull out Austin for changes over time
austin.rent <- subset(rent, Location == "Austin")

# Draw boxplots of rent by year in Auston
boxplot(Rent ~ Year, data = austin.rent,
        col = "lightsteelblue3",
        notch = F,
        xlab = "Year",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartment by year in Austin")

# Create data frame "food" from CSV file w/headers
food <- read.csv("Monthly_food_costs.csv", header = T)

# Make a barplot of monthly food costs using ggplot
food.allw <- ggplot(food, aes(x=Location, y=Monthly)) + 
  geom_bar(stat = "identity",
           col = "lightsteelblue3",
           fill=rgb(0.1,0.4,0.5,0.7)) 

# Change the labels
food.allw + labs(y = "Min recommended ($)")

# Adjust formatting
food.allw + geom_hline (yintercept=321.14, 
        linetype="dashed", 
        color = "darkturquoise", 
        size=1) + 
        labs(y = "Min recommended ($)") +
        theme(panel.background = element_rect(fill = F, 
        colour = 'Black'),
        panel.grid.major.y = element_line(size=0.1, colour = "black"),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 0))
