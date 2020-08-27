## to calucate the turbidity WQG/WQO with three conditions, need to input the upstream station that [] will be compared to, and
# whether it is clear or turbid flows

## Example 1 NTU max when U/S station is <= 5 NTU, 5 NTU when U/S station > 5NTU <=50,
## 10% max when U/S station > 50 NTU

# data = dataframe
# upstn = upstream or reference station to derive WQO
# clearntu = max turbidity value in clear flows (in this case 1 NTU)
# turbidntu = max turbidity value in turbid flows (in this case 5 NTU)
# turbidpercent = max percent increase in turbid flows (in this case 10%)
# turbidlower = lower limit of case 2
# turbidupper = upper limit of case 2 and condition of case 3

turbidity_WQO <- function(data, upstn, clearntu, turbidntu, turbidpercent, turbidlower, turbidupper){
  
  # #for testing part
  # data<-SR_sites
  # 
  # upstn<-"0500075"
  # 
  # # value<-data %>%
  # #   dplyr::filter(EMS_ID==upstn & Variable=="turbidity") %>%
  # #   dplyr::pull(Value)
  # 
  # turbidntu<-5
  # clearntu<-1
  # turbidpercent<-10
  # turbidlower = 5
  # turbidupper = 50
  # 
  # 
  # # calculate tubid flow = tubid%>value (for example, if turbid%=10, turbid flow = value*1.1)
  # #written this way to add flexibility to change turbid%
  # # turbidFlow<-value*(1+turbidpercent/100)
  # 
  # 
  # ## the Similkameen WQO: 1 NTU max when U/S station is <= 5 NTU, 5 NTU when U/S station > 5NTU <=50,
  # ## 10% max when U/S station > 50 NTU
  # data %>% dplyr::filter(EMS_ID==upstn & Variable=="turbidity") %>%
  # dplyr::mutate(Threshold = 
  #   dplyr::case_when(
  #   #case 1
  #   Value <= 5 ~ Value + clearntu,
  #   Value > turbidlower & Value <= turbidupper ~ Value + turbidntu,
  #   Value > turbidupper ~ Value * (1+turbidpercent/100)
  # )) %>% View(.)
  
  #end testing
  
  data %>% dplyr::filter(EMS_ID==upstn & Variable=="turbidity") %>%
    dplyr::mutate(Threshold =
      dplyr::case_when(
      #case 1
      Value <= 5 ~ Value + clearntu,
      #case 2
      Value > turbidlower & Value <= turbidupper ~ Value + turbidntu,
      #case 3
      Value > turbidupper ~ Value * (1+turbidpercent/100)
    )) 
}

## to run function

source("turbidity_WQO.R")

tubidity_WQO <- turbidity_WQO(SR_sites, "0500075", 1, 5, 10, 5, 50)



