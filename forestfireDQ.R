forestfires <- forestfires %>% mutate(month = factor(month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")))
forestfires <- forestfires %>% mutate(day = factor(day,levels = c("sun","mon","tue","wed","thu","fri","sat")))
View(forestfires)

fires_month <- forestfires %>% group_by(month) %>% summarise(fires = n())
fires_day <- forestfires %>% group_by(day) %>% summarise(fires = n())

ggplot(data = fires_month) + aes(x = month,y = fires) + geom_bar(stat = "identity") + theme(panel.background = element_rect(fill = "white")) + labs(title = "Fires by Month")
ggplot(data = fires_day) + aes(x = day,y = fires) + geom_bar(stat = "identity") + theme(panel.background = element_rect(fill = "white")) + labs(title = "Fires by Day")
#box plots for month and day variables
box_plot_month <- function(x,y){
ggplot(data = forestfires) + 
    aes_string(x = x,y = y) + 
    geom_boxplot() 
} 

x_var <- names(forestfires)[3]
y_var <- names(forestfires)[5:12]
  
map2(x_var,y_var,box_plot_month)

box_plot_day <- function(a,b){
  ggplot(data = forestfires) + aes_string(x = a,y = b) + geom_boxplot() 
} 

a_var <- names(forestfires)[4]
b_var <- names(forestfires)[5:12]
map2(a_var,b_var,box_plot_day)
 

#scatter plots for area variable
scatter_plot_area <- function(c,d){
  ggplot(data = forestfires) + 
    aes_string(x = c,y = d) + 
    geom_point(alpha = 0.3) 
} 

c_var <- names(forestfires)[5:12]
d_var <- names(forestfires)[13]

map2(c_var,d_var,scatter_plot_area)

#histogram of area variable
ggplot(data = forestfires) + aes(x = area) + geom_histogram(bins = 30)
