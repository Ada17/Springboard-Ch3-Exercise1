#library(data.table)
refine_original <- read.csv("C:/Users/aalutu/Desktop/work/pers/data science/refine_original.csv")

dtoy <- refine_original[1:6,1]

#nPhil <- c("philip","philip","philip","philip","philip","philip")
#setNames(dtoy=nPhil)

#dtoy <- nPhil  #'philip'
  
 # dtoy

 # transform(refine_original, refine_original[1:6,1] = "philip")
  #transforefine_original[refine_original=="Phillips"] <- "philip"
  
 pcompany <- gsub(pattern="P.*s$|f.*s$", replacement = "philips", x = refine_original$company, ignore.case = TRUE)
  #gsub(ignore.case = TRUE)

 akcompany <- gsub(pattern="A.*o$|a.*0$", replacement = "akzo", x = pcompany, ignore.case = TRUE)
  #vector or dataframe used here is pcompany, to include the changes from there.
  ##akcompany
 
 vhcompany <- gsub(pattern="V.*\\ H.*n$", replacement = "van houten", x = akcompany, ignore.case = TRUE)
 #vhcompany
 
 allcompany <- gsub(pattern = "u.*r$", replacement = "unilever", x = vhcompany, ignore.case = TRUE)
# allcompany
 
 refine_original$company <- allcompany
 
 #refine_original$company #Print Question 1
 
 #Exercise 1 question 2
 refine_original$Product.code...number <- as.character(refine_original$Product.code...number)
 prodcode <- strsplit(refine_original$Product.code...number, split = "-")
 prodcode #print second column without the hyphen
 
 productcode <- 0 # vector to store product code
 productid <- 0 #vector to store product id
 productcat <- 0 #vector to store product categories
 fulladdr <- 0 #vector to store concatenated address from 3 fields.
 
 
 
 
 #for loop for all the looping for Exercise 1
 for(i in 1:length(prodcode)){
   
   #print(prodcode[i])
  
 # print(substring(prodcode[i], 4, 4)) #store the product codes
  productcode[i] <- substring(prodcode[i], 4, 4) #store the product codes
    if (productcode[i] == "p"){
      productcat[i] = "Smartphone"
    }else if (productcode[i] == "v")
    {
    productcat[i] = "TV"
    }else if (productcode[i] == "x"){
      productcat[i] = "Laptop"
    }else{ #must be q
      
      productcat[i] = "Tablet" 
    }
  
  productid[i] <- gsub(pattern ="\"", replacement= "", x = substring(prodcode[i], 9, 10))
  
  #use for loop position index to concat the addresses
  
  fulladdr[i] <- paste(refine_original$address[i],",", refine_original$city[i],",", refine_original$country[i], sep = " ")
  
  } #end for_loop
 
# productcode 
# productid
 #productcat
 #fulladdr #to print concatenated addresses from the for loop.
 
 
 #create dummy variables containing binary 0 or 1 for each of the company columns.
 #dummy variables for each company and product
 #generating random numbers between 0 and 1. floor() returns integer instead of decimals
 compphil <- floor(runif(25)) # c(1, 0, 0, 1,0,0,0,0, 1, 0, 0, 1,0,0,0,0,1, 0, 0, 1,0,0,0,0,1)
 compazko <- floor(runif(25, min = 0, max = 2))
 compvanh <- floor(runif(25, min = 0, max = 2))
 compuni <- floor(runif(25, min = 0, max = 2))
 prodphone <- floor(runif(25, min = 0, max = 2))
 prodtv <- floor(runif(25, min = 0, max = 2))
 prodlaptop <- floor(runif(25, min = 0, max = 2))
 prodtablet <- floor(runif(25, min = 0, max = 2))
 
 refineclean <- mutate(refine_original, product_code = productcode, product_number = productid, product_category = productcat, full_address = fulladdr, 
        company_philips = compphil, company_azko = compazko, company_van_houten = compvanh, 
        company_unilever = compuni, product_smartphone = prodphone, product_tv = prodtv, 
        product_laptop = prodlaptop, product_tablet = prodtablet)
 
 #write to a new csv file
 #refineclean
 #refineclean.csv will be created by R in the directory provided.
 write.csv(refineclean, file = "C:/users/aalutu/Desktop/work/pers/data science/refine_clean.csv")