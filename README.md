# NYC-Green-Cab-Analysis

## METHODOLOGY

This analysis is performed in R by using various data visualization and statistical analysis methods. Using SQL scripts in R, the dataset was combined with NYC Taxi & Limousine Commission Information on green cab ridership and National Centers for Environmental Information on various weather indexes from February 1 to February 14, 2016. The core objective of this analysis is focusing on addressing key trends about customer behaviors including demand and payment based on time, location and weather condition, which are highly related with taxi driver’s revenue potential. From a taxi driver’s perspective, the key questions addressed here are what is a better distance range for a driver’s revenue, when are better time to drive, where are better areas to drive, and how weather conditions influence demand for green taxi.   

The green cab ridership dataset was cleaned by using SQL to filter out some unrealistic fares, toll fees, tips, trip distance, or GPS coordinates. For example, tips or fares are negative numbers. The range of different variables in this taxi dataset are showed in Figure. 1 including original data and data after the cleaning procedure. Weather data in different locations are aggregated to daily base with indexes, such as average temperature, average wind speed, average snow depth, etc. Two datasets are combined based on date (2/1/2016 – 2/14/2016). Multiple data frames were created with SQL in R to serve for different analysis needs. Most of the charts are trying to use blind friendly colors.

## RECOMMENDATIONS

This analysis showed, as a green taxi driver, driving on right time, right areas and understanding consumer payment behaviors are very important to gain more revenue. Some recommendations to drivers are listed below:

•	Tip and revenue per mile will be reduced along with the increasing trip distance. So it is better to drive short trips than long ones. Rule of thumb is less than 7 miles.

•	It is better to drive in rush hours (7 am to 10 am and 5 pm to 8 pm) during weekdays, especially for morning rush hours, to gain more trips and revenues. During weekends, a whole day time driving is preferred. Furthermore, Saturday is a very important day for boosting revenue. 

•	Although overnight driving seems have higher fare amount, after considering the lower demand at night, better revenue should be gain to drive during daily rush hours. 

•	Upper Manhattan, Brooklyn Heights and Long Island City are the most popular areas for customers to hail green taxies. The Long Island City area is a better choice during weekends comparing with other two areas. 

•	Multiple weather indexes are strong indicators for customers to hail a taxi. Generally, the worse the weather condition is, the better for taxi drivers to gain revenue. 

All in all, key words for green cab drivers to boost their revenues are short trip, rush hours, Saturday, Long Island City, worse weather. 


