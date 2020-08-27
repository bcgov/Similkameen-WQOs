## to calucate the TSS WQG/WQO, need to input the upstream station that [] will be compared to, and
# whether it is clear or turbid flows
## this works on a dataframe with the following columns: data=dataframe, upstn=EMS_ID, 
## clearmgl/turbidpercent=input WQO for clear/turbid flows, condn=the [TSS] that defines 
# clear/turbid flows

## Example: 10 mg/L max when U/S station is <= 100mg/L, 
#  10% max when U/S station > 100 mg/L

TSS_WQO <- function(data, upstn, clearmgl, turbidpercent, condn){
  
  #for testing part
 #  data<-SR_sites
 # 
 #  upstn<-"0500075"
 # 
 #  value<-data %>%
 #   dplyr::filter(EMS_ID==upstn & Variable=="tss") %>%
 #    dplyr::pull(Value)
 # 
 # condn<-100
 # clearmgl<-10
 # turbidpercent<-10
 # 
 # 
 # # calculate tubid flow = tubid%>value (for example, if turbid%=10, turbid flow = value*1.1)
 # #written this way to add flexibility to change turbid%
 # turbidFlow<-value*(1+turbidpercent/100)
 # 
 # output<-dplyr::if_else(value<=condn, clearmgl,turbidFlow)
 # 
 # tibble(value, clearmgl, turbidFlow, output) %>% View(.)
  
 #end testing
  
  value<-data %>%
    dplyr::filter(EMS_ID==upstn & Variable=="tss") %>%
    dplyr::pull(Value)
  
  turbidFlow<-value*(1+turbidpercent/100)
  
  # output<-dplyr::if_else(value<=condn, 
  #                        clearlimit,
  #                        turbidFlow)
  
  data %>%
    dplyr::filter(Variable=="tss", EMS_ID==upstn) %>%
    dplyr::mutate(Threshold=dplyr::if_else(value<=condn, 
                                          value + clearmgl,
                                           turbidFlow))
}

## to run function

source("TSS_WQO.R")

(tsslimits<-TSS_WQO(SR_sites, "0500075", 10, 10, 100))


