#Normalize
RawData_18months_final$order.revenue.Carpets.Norm<- RawData_18months_final$order.revenue_Carpets_textile..CZK / mean(RawData_18months_final$order.revenue_Carpets_textile..CZK)
RawData_18months_final$order.revenue.Decoration.Norm<- RawData_18months_final$order.revenue_Decoration..CZK / mean(RawData_18months_final$order.revenue_Decoration..CZK)
RawData_18months_final$order.revenue.Fashion.Norm<- RawData_18months_final$order.revenue_Fashion..CZK / mean(RawData_18months_final$order.revenue_Fashion..CZK)
RawData_18months_final$order.revenue.Furniture.Norm<- RawData_18months_final$order.revenue_Furniture..CZK. / mean(RawData_18months_final$order.revenue_Furniture..CZK.)
RawData_18months_final$order.revenue.Kitchen.Norm<- RawData_18months_final$order.revenue_Kitchen_and_dining..CZK. / mean(RawData_18months_final$order.revenue_Kitchen_and_dining..CZK.)
RawData_18months_final$order.revenue.Lights.Norm<- RawData_18months_final$order.revenue_Lights..CZK. / mean(RawData_18months_final$order.revenue_Lights..CZK.)
RawData_18months_final$order.revenue.Living_out.Norm<- RawData_18months_final$order.revenue_Living_outdoors..CZK. / mean(RawData_18months_final$order.revenue_Living_outdoors..CZK.)
RawData_18months_final$order.revenue.other.Norm<- RawData_18months_final$order.revenue_other..CZK. / mean(RawData_18months_final$order.revenue_other..CZK.)
