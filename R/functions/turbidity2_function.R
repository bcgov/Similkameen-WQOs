## to calucate the Turbidity WQO/WQGs when there are two conditions, 
# need to input the upstream station that [] will be compared to, and
# whether it is clear or turbid flows
## this works on a dataframe with the following columns: data=dataframe, 
# upstn=EMS_ID, clearmgl/turbidpercent=input WQG/WQO for clear/turbid flows,
# condn=the [turbidity] that defines clear/turbid flows
# 

## Example: 10 mg/L max when U/S station is <= 100mg/L, 
## 10% max when U/S station > 100 mg/L

turbidity2_WQO <- function(data, upstn, clearmgl, turbidpercent, condn){
  
  value<-data %>%
    dplyr::filter(EMS_ID==upstn & Variable=="turbidity") %>%
    dplyr::pull(Value)
  
  turbidFlow<-value*(1+turbidpercent/100)

  data %>%
    dplyr::filter(Variable=="turbidity", EMS_ID==upstn) %>%
    dplyr::mutate(Threshold=dplyr::if_else(value<=condn, 
                                           value + clearmgl,
                                           turbidFlow))
}

## to run function

source("tubidity2_WQO.R")

(ntulimits<-turbidity2_WQO(SR_sites, "0500075", 10, 10, 100))


