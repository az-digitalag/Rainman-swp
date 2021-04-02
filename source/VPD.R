####Function to calculate SVP and VPD from RH and T
####source: Emanual 1994, "Atmospheric Convection"
#inputs are RH and T in Celsius
#returns SVP/VPD in kPa
SVP<-function(t){ifelse(t>0, exp(53.67957 -(6743.769/(t+273.15))-4.8451*log(t+273.15))/10, 
                        exp(23.33086 - (6111.72784/(t+273.15))+0.15215*log(t+273.15))/10)}


VPD<-function(rh, t){ifelse(t>0, exp(53.67957 -(6743.769/(t+273.15))-4.8451*log(t+273.15))/10*(1-rh), 
                            exp(23.33086 - (6111.72784/(t+273.15))+0.15215*log(t+273.15))/10*(1-rh))}

