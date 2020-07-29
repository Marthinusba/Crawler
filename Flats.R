library(rvest)
library(tidyverse)
library(stringr)
flat_lib<-read_html("https://www.gumtree.co.za/s-houses-flats-for-rent/cape-town/v1c9078l3100006p1")
#flat_pages<- flat_lib%>% html_nodes(".next , .current , .after a") %>% html_attr("href")
#print(car_pages)

flats_linked<-c()
for(i in 1:50){
  flats_pages <- paste("https://www.gumtree.co.za/s-houses-flats-for-rent/cape-town/page-",i,"/v1c9078l3100006p",i,sep="")
  flats_linked[i]<-(flats_pages)
  
}

#cars_linked<-as.tibble(cars_linked)

flats_links <- c()
#i<-1
for(i in flats_linked){
  flats_i <- html_session(i)
  flat_i_links <- flats_i %>% html_nodes("a") %>% html_attr("href") 
  
  flats_links_i <- paste(("https://www.gumtree.co.za"),str_subset(flat_i_links,"(/a-house-rentals)."),sep="")
  flats_links <- c(flats_links, flats_links_i)
}
# remove any duplicates and reorder
flats_links <- sample(unique(flats_links))

flats_data <- data.frame()
counter<-0
for(i in flats_links){   # more than 3 and you get blocked
  tryCatch({
    
    flat <- read_html(i)
    link<-flat
    
    ad <- flat %>% html_nodes(css = "h1") %>% html_text(trim = T)
    
    price <- flat %>% html_nodes(css = ".vip-summary .ad-price") %>% html_text(trim = TRUE)
    rooms <- flat %>% html_nodes(css = ".attribute:nth-child(4) .value") %>% html_text(trim = TRUE)
    size <- flat %>% html_nodes(css = ".attribute:nth-child(7) .value") %>% html_text(trim = TRUE) 

    # if couldn't find data on webpage, replace with NA
    price <- ifelse(length(price) >0, price, NA)
   rooms <- ifelse(length(rooms) >0, rooms, NA)
    size <- ifelse(length(size) > 0, size, NA)

    
    # store results
    
    counter<-counter+1
    print(counter)
    this_flats <- data.frame(link=i,ad=ad,price = price, rooms = rooms, size = size)
    flats_data <- rbind.data.frame(flats_data,this_flats)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # random wait avoids excessive requesting
  Sys.sleep(sample(seq(2, 3, by=1), 1))
  
}
write.csv(flats_data,file = "flats.csv")