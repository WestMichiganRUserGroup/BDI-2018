
###############################################################
# Title: Create Unique Reports for Vehicle Owners
# Author: Aaron Clark
# Created: 09/16/2018
# For: Big Data Ignite 2018, Grand Rapids, MI
###############################################################


suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(rmarkdown))
suppressMessages(library(knitr))
suppressMessages(library(kableExtra))
suppressMessages(library(randomNames))
suppressMessages(library(RColorBrewer))
suppressMessages(library(scales))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(pander))
suppressMessages(library(mailR))


rm(list=ls()) # clear the environment (if needed)


###############################################################
# Read in Data Set
###############################################################

data(mtcars)


#############################################################################
# OR, If you you have a SQL data base, this is where you'd write your query!
#############################################################################

# suppressMessages(library(RODBC))

#CLOSE ANY OPEN DATABASE CONNECTIONS CREATE CONNECTION TO DB
# odbcCloseAll()
# my_db <- odbcDriverConnect('driver={SQL Server};server=[servername];database=[dbname];trusted_connection=true')

# Submit Query and return table as R data.frame
# mtcars<- sqlQuery(my_db,paste0("
#   select *
#   from whatever_table
# "))


###############################################################
# Manipulate the data
################################################################

# Create a few new columns
mtcars<-mtcars%>%
  mutate(mpg_cat = case_when(mpg <= 15 ~ "15 mpg or Less"
                              ,mpg <= 20  ~ "15 to 20 mpg"
                              ,mpg > 20  ~ "20 mpg or More"
                              )
         ,hp_cat = case_when(hp <= 100 ~ "100 hp or Less"
                              ,hp <= 175  ~ "100 to 175 hp"
                              ,hp <= 230  ~ "175 to 230 hp"
                              ,hp > 230  ~ "230 hp or More"
         )
         ,vehicle = row.names(mtcars)
  )%>%
  rowwise()%>%
  mutate(owner = gsub(",","",randomNames(1,which.names = "both"
                            ,name.order = "first.last")))%>%
  ungroup()
  

# how many times have these cars raced?
x<-5

# Create some fake Quater Mile Times data
for (i in 1:nrow(mtcars)){
  if(i==1){
    vehicle<-rep(mtcars$vehicle[i],x)
    run<-i:x
    latest_qsec<-rnorm(n=x, mean = mtcars$qsec[i] + runif(1,1,3) , sd = 2.5)
    races<-data.frame(run,vehicle,latest_qsec)
  }
  else{
    run<-1:x
    vehicle<-rep(mtcars$vehicle[i],x)
    latest_qsec<-rnorm(n=x, mean = mtcars$qsec[i] + runif(1,1,3) , sd = 2.5)
    races<-rbind(races,data.frame(run,vehicle,latest_qsec))
  }
}

# Append race data to meta data about each vehicle
cars<-
  mtcars%>%
  left_join(races)%>%
  mutate(beat_pr = case_when(qsec>latest_qsec~"Yes"
                   ,qsec<=latest_qsec~"No"
                     )
         )


###############################################################
# Generate the reports
################################################################
myRmd_dir<-"C:/folderOne/folderTwo/BDI 2018" # Important: Enter in the directory of your RMD file here!
setwd(myRmd_dir)

# Create subfolder for PDFs
out_dir<-paste0("./Reports")
dir.create(out_dir, showWarnings = F)

# Create Timestamp to put on file name. Good way of keeping track of what "batch" a report belongs to
time<-gsub("-",".",gsub(":","-",substr(gsub(" ","_",Sys.time()),1,16)))


for(i in 1:nrow(mtcars)){

    # Subset a few data frames built above unique to this vehicle/owner
    this_car<-
      mtcars%>%
      subset(vehicle==mtcars$vehicle[i])
    
    my_mpg_class<-
      cars%>%
      subset(mpg_cat == this_car$mpg_cat)
      
    my_hp_class<-
      cars%>%
      subset(hp_cat == this_car$hp_cat)
      
    these_runs<-
      cars%>%
      subset(vehicle==this_car$vehicle)
    
    # Concatenate a consistent file name
    fileName <- paste0("Yesterdays_Quarter_Mile_Times_",gsub(" ","_",this_car$vehicle),"_",time,'.pdf')

    # Render the PDF!
    render('Quarter Mile Report.Rmd',
           output_format= "pdf_document",
           output_file = fileName,
           output_dir = out_dir
    )
    
    ###############################################################
    # Email the reports, one at a time!
    ################################################################
    # Conidition
    beat<-any(which(these_runs$beat_pr=="Yes"))

    # Create a conditional email body based on what happened at the race track!
    htmlbody <- paste0("<html><body>",trimws(this_car$owner,which = "both"),",<br><br>Our results show that you ",ifelse(beat,"","did not")," beat your longstanding PR yesterday.",ifelse(beat," Congrats! As a token of celebration, please accept a free quarter mile run on us!"," Better luck next time."),"<br><br><b>Please be aware that the attached times are final.</b> If you have any questions or concerns regarding the timing equipment or it's calibration please contact Aaron at aclark@realemailaddress.com.<br><br>Thank your for your patronage and we'll see you on the track,<br><br>Dave, CEO<br>My Fake Raceway LLC</body></html>")

  # Assemble components of email
  send.mail(from = "NoReply@realemailaddress.com" # who is the email from?
            ,to = "deliverTo@realemailaddress.com" #your email here
            ,subject = paste0(this_car$vehicle," Qtr Mile Track Report")
            ,body = htmlbody
            ,html = TRUE
            ,attach.files = paste0(out_dir,"/",fileName)
            ,smtp = list(host.name = "webmail.hostname.com") # Enter in your email client's host name here!
            ,authenticate = F
            ,send = TRUE
            )

  # print something to the R console to help you keep track of where you are in the "for loop"
  cat(paste0("\nAuto Email Yesterday's Quarter Mile Track Times Report for ",trimws(this_car$owner,which = "both")," sent @ ",Sys.time()))
  
}











