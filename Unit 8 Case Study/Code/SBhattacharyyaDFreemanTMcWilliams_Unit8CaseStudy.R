## Bring the data files in 
femalefilenames = paste("/Users/timmcwilliams/Documents/DataScience/QTW/Case Study Unit 8/", 1999:2012, ".txt", sep = "")
femaleFiles = lapply(femalefilenames, readLines)
names(femaleFiles) = 1999:2012

## Create a function to find the columns 
findColLocs = function(spacerRow) {
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

## Create a function to select the columns 
selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

## Create a function that uses the two function above 
## to extract the variables from each years txt file
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
{
       # Find the index of the row with =s
  eqIndex = grep("^===", file)
       # Extract the two key rows and the data
  spacerRow = file[eqIndex] 
  headerRow = tolower(file[ eqIndex - 1 ])
  body = file[ -(1 : eqIndex) ]
       
       # Obtain the starting and ending positions of variables
  searchLocs = findColLocs(spacerRow)
  locCols = selectCols(varNames, headerRow, searchLocs)

  Values = mapply(substr, list(body), start = locCols[1, ], 
                  stop = locCols[2, ])
  colnames(Values) = varNames
  
  invisible(Values)
}

## Call the function 
# This function call errors out because of the years 2000 and 2001. 
# They do not have the ==== at the begning of the file. But the mens 
# files do. So we will take the == from a mens file and slap it into 
# the females 2000 and 2001 fiels.
femaleResMat = lapply(femaleFiles, extractVariables)
length(femaleResMat)

## Lets explore those years 
femaleFiles[['2000']][1:15]
femaleFiles[['2001']][1:15]

## The years above are missing the header with the =====
men2001 = readLines("/Users/timmcwilliams/Documents/DataScience/QTW/Case Study Unit 8/men2001.txt")
men2001[1:15]

## Assign the === to the female files
femaleFiles[['2000']][9:10] = men2001[12:13]
femaleFiles[['2001']][9:10] = men2001[12:13]

## Now call the extraction function again
femaleResMat = lapply(femaleFiles, extractVariables)
length(femaleResMat)

## Check the data for each year 
sapply(femaleResMat, nrow)

## Lets check the data by looking at age 
age = as.numeric(femaleResMat[['2012']][ , 'ag'])
age = sapply(femaleResMat,
             function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")


## Check for NAs
sapply(age,  function(x) sum(is.na(x)))

## Convert time 
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
                      if (length(x) == 2) x[1] + x[2]/60
                      else 60*x[1] + x[2] + x[3]/60
                      })
}

## Create a function to convert the data from a matrix to a df
createDF =
function(Res, year, sex)
{
       # Determine which time to use
  useTime = if( !is.na(Res[1, "net"]) )
              Res[ , "net"]
            else if( !is.na(Res[1, "gun"]) )
               Res[ , "gun"]
            else
               Res[ , "time"]
  runTime = convertTime(useTime)
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , "name"],
                       home = Res[ , "home"],
                       age = as.numeric(Res[, "ag"]),
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

## Call the function 
femaleDF = mapply(createDF, femaleResMat, year = 1999:2012,
                   sex = rep("W", 14), SIMPLIFY = FALSE)



# There are a large number of NAs in 2007, 2009, and 2010, and it appears 
# that all of the run time values for 2006 are NA. Let’s begin by examining 
# a few of the records in 2007, 2009, and 2010 that have an NA in run time. 
# We find that these are caused by runners who completed half the race but 
# have no final times and by runners who have a footnote after their time. 
# We have to modify our createDF() fucnton to eliminate the footnote 
# smybols(# and *) .

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

## Call the function 
femaleDF = mapply(createDF, femaleResMat, year = 1999:2012,
                   sex = rep("W", 14), SIMPLIFY = FALSE)

sapply(femaleDF, function(x) sum(is.na(x$runTime)))

## Now we have fixed the NAs 


# Finally, we combine the race results for all years and females into 
# one data frame using the do.call() function to call rbind() with the list 
# of data frames as input. The do.call() function is very convenient when 
# we have the individual arguments to a function as elements of a list.
cbFemale = do.call(rbind, femaleDF)
save(cbFemale, file = "cbFemale.rda")

dim(cbFemale)
head(cbFemale)
str(cbFemale)
summary(cbFemale)






