# Assignment 2

# 1. Import dataset
Work <- read.csv("./DATA/Raw/bagmati_raw_inventory.csv")


#  Display first six observations
head(Work)

# 3. Check number of rows and columns
dim(Work)

# 4. Check variable names
names(Work)

# 5. Check data types
str(Work)

# 6. Generate statistical summary
summary(Work)

# 7. Check missing values #check how many missing values (NA) are present in each column of the dataset
colSums(is.na(Work))



#. Identify the number of unique forest stands, plots, and tree observations
# Count unique forest stands
length(unique(Work$Stand_ID))

# Count unique plots
length(unique(Work$Plot_ID))

Count tree species

length(unique(Work$Species))
unique(Work$Species) # different name of shorea robusta, schima wallichi 

#cleaning species data diff name od same species available, so now fixing taht
# Standardize Shorea robusta
# Make a copy of original data
Work_clean <- Work

# Replace different species names

# Correct Shorea robusta names
Work$Species[Work$Species %in% c("S. robusta", "Shorea rubusta")] <- "Shorea robusta"

# Correct Schima wallichii names
Work$Species[Work$Species == "Schima wallichii"] <- "Schima wallichi"

Work$Species[Work$Species == "C. indica"] <- "Castanopsis indica"

# Check corrected names
unique(Work$Species)# now cleaned


table(Work_clean$Species)


# Count unique tree IDs (if Tree_ID exists)
length(unique(Work$Tree_ID))
unique(Work$Tree_ID)


# Check variable names
names(Work)

# Check data types of all data head
sapply(Work, class)

# Check for how many negative numerical values
sapply(Work[sapply(Work, is.numeric)], function(x) sum(x < 0, na.rm = TRUE))

# Frequency of observations by forest stand
table(Work$Stand_ID)

# Frequency of observations by plot
table(Work$Plot_ID)


#NA CHECKING

# Count missing values in each column
colSums(is.na(Work))  # 3 na in dbh
# missing value of individual column
sum(is.na(Work$Stand_ID)) #o missing
sum(is.na(Work$Plot_ID)) #0 missing
sum(is.na(Work$Tree_ID)) #0 missing

# Removing rows containing NA
Work_clean <- na.omit(Work) # omiting na 

colSums(is.na(Work_clean)) # now zero na

#Checking neg value
sapply(Work[sapply(Work, is.numeric)],
       function(x) sum(x < 0, na.rm = TRUE))


#checking biological measurement
#summary of individual
# Check DBH values
summary(Work$DBH_cm)

# Check tree height
summary(Work$Height_m)

# Check wood density
summary(Work$Wood_density)

# Check above-ground biomass
summary(Work$Biomass_kg)

# Check plot area
summary(Work$Plot_area_ha)



# MAKING FUNCTION 

#Individual tree carbon stock (kg)
calc_tree_carbon <- function(biomass_kg) {
    carbon_stock <- biomass_kg * 0.47
    return(carbon_stock)
}
 calc_tree_carbon(30)

#Convert carbon stock from kg to Mg
calc_carbon_Mg <- function(carbon_kg) {
    carbon_Mg <- carbon_kg / 1000
    return(carbon_Mg)
}
calc_carbon_Mg(14.1)

#Calculate carbon density (Mg/ha)
calc_carbon_density <- function(biomass_kg, sampled_area_ha) {

    # Call first function
    carbon_kg <- calc_tree_carbon(biomass_kg)

    # Call second function
    carbon_Mg <- calc_carbon_Mg(carbon_kg)

    # Calculate total carbon
    total_carbon_Mg <- sum(carbon_Mg)

    # Calculate carbon density
    carbon_density <- total_carbon_Mg / sampled_area_ha

    return(carbon_density)
}

calc_carbon_density (30, 14.1)


#single formula to cnvrt biomass in carbonstock

calc_carbon_density_stand <- function(biomass_kg, sampled_area_ha) {

    # A. Calculate individual tree carbon (kg C)
    carbon_kg <- biomass_kg * 0.47

    # B. Convert individual tree carbon to Mg C
    carbon_Mg <- carbon_kg / 1000

    # C. Calculate total carbon stock (Mg C)
    total_carbon_Mg <- sum(carbon_Mg)

    # Calculate carbon density (Mg C/ha)
    carbon_density <- total_carbon_Mg / sampled_area_ha

    return(carbon_density)
}

#checking function
calc_carbon_density_stand(
    biomass_kg = c(30, 40, 50),
    sampled_area_ha = 0.1
)


calc_carbon_density_stand(30, 14.1)



# calc_carbon_density(20,20)

calc_carbon_density(Work$Biomass_kg, sum(Work$Plot_area_ha))
