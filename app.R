
# Juliana Brandt, Madison Polley

library(shiny)


data = read.csv("bodyimagerawdata.csv")
data = data %>% na_if("")
workingdata = data[ , c(9:45, 50, 51, 53, 55:57)]
workingdata = workingdata[ , c(1:4, 6:8, 11, 12, 15:22, 24:43)]


for (i in 1:(ncol(workingdata))) {
  workingdata[, i] = as.character(workingdata[, i])
}

workingdata[ , ncol(workingdata)] = as.numeric(workingdata[, ncol(workingdata)])


for (i in 1:(ncol(workingdata))) {
  
  if("A little healthy" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("Not at all healthy", "A little healthy", "Somewhat healthy", "Very healthy", "Extremely healthy", "")))
  }
  
  if("Always" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c( "Not applicable", "Never", "Rarely", "Sometimes", "Very often", "Always")))
  }
  
  if("Not at all" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c( "Not at all", "Slightly", "Somewhat", "Very", "Extremely")))
  }
  
  if("A few" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c( "None", "A few", "Some", "Most", "All")))
  }
  
  if("Extremely often" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("Not applicable", "Never", "Rarely", "Sometimes", "Very often", "Extremely often")))
  }
  
  if("Yes" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("No", "Yes")))
  }
  
  if("A great deal" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("Not at all", "A little", "Some", "Quite a bit", "A great deal")))
  }
  
  if("Once a week" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("Less than once a month", "Once or twice a month", "Once a week", "Once a day", "More than once a day")))
  }
  
  if("Worsened" %in% workingdata[ ,i]) {
    workingdata[, i] = as.numeric(factor(workingdata[, i], ordered= T, levels = c("Worsened", "Stayed about the same", "Improved")))
  }
  
}
indx = apply(workingdata, 1, function(x) any(is.na(x)))

workingdata$imputed = 0
workingdata$imputed[indx] = 1

workingdata = workingdata %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

data.year.gender = workingdata[3:nrow(workingdata),35:36]
workingdata = lapply(workingdata[3:nrow(workingdata), c(1:34,37:38)], as.numeric)
workingdata = data.frame(workingdata)

inferencedata = cbind(workingdata,data.year.gender)

inferencedata = inferencedata %>%
  mutate(personal = (select(., c(Q1,Q2,Q3,Q4,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q33,Q35,Q42,Q44)) %>%  rowSums(na.rm = TRUE))/81) 

inferencedata = inferencedata %>% 
  mutate(social = (select(., c(Q6,Q7,Q11,Q12)) %>%  rowSums(na.rm = TRUE))/24)

inferencedata = inferencedata %>%
  mutate(weight = (select(., c(Q24,Q25,Q26,Q27)) %>%  rowSums(na.rm = TRUE))/23)

inferencedata = inferencedata %>%
  mutate(eating = (select(., c(Q29,Q31,Q32,Q34,Q41,Q44)) %>%  rowSums(na.rm = TRUE))/51)

cleandata = inferencedata[,36:ncol(inferencedata)]

cleandatafull = cleandata[complete.cases(cleandata), ]
cleandatafull$Q46= revalue(cleandatafull$Q46, c("...first year"="First Year", "...second year"="Second Year", "...third year"="Third Year", "...fourth year"="Fourth Year", "...fifth year or more"="Fifth Year+", "...graduate student pursuing a Masters or PhD"="Graduate"))



# ---------------------------------------UI------------------------------------------------------
ui<-(pageWithSidebar(
  # title
  headerPanel("Density Graphs for Body Image Scores"),
  
  sidebarPanel
  ("Select Options for the following:", selectInput("gender", "Gender:",
                                                    choices = c("Female", 
                                                                "Male",
                                                                "Nonbinary", 
                                                                "Do not care to say"),
                                                    selected = "Female"),
    selectInput("school", "Year in School:",
                choices = c("First Year", 
                            "Second Year",
                            "Third Year",
                            "Fourth Year"),
                selected = "First Year"),
    selectInput("score", "Body Image Score Category:",
                choices = c("personal", 
                            "social",
                            "weight",
                            "eating"))),
  
  mainPanel(h2("Body Image and Disordered Eating Scores Among UW-Madison Students"), h5("Density Plot"), plotOutput("densityplot"))
)
)


# --------------------------------------------SERVER---------------------------------------------
server<-function(input, output){
  
  genders = c("Female", "Male", "Nonbinary", "Do not care to say")
  years = c("First Year", "Second Year", "Third Year", "Fourth Year")
  scores = c("personal", "social", "weight", "eating")
  
  
  dataset = reactive({
    for (i in genders) {
      for (j in years) {
        for (k in scores) {
          if (input$gender == i) {
            if (input$school == j) {
              if (input$score == k) {
                gender.data = cleandatafull[cleandatafull$Q47 == i,]
                specified = gender.data[gender.data$Q46 == j,]
                return(specified)
              } } } } } }
  })
  all.dataset.gender = reactive({
    for (i in genders) {
      for (j in years) {
        for (k in scores) {
          if (input$gender == i) {
            if (input$school == j) {
              if (input$score == k) {
                gender.data = cleandatafull[cleandatafull$Q46 == j,]
                return(gender.data)
              } } } } } }
  })
  
  fill.color = reactive({
    if (input$gender == "Female") {
      return("red")
    }
    if (input$gender == "Male") {
      return("blue")
    }
    if (input$gender == "Nonbinary") {
      return("yellow")
    }  
    if (input$gender == "Do not care to say") {
      return("black")
    }
    else {
      return(NULL)
    }
  })
  
  line.color = reactive({
    if (input$school == "First Year") {
      return("blueviolet")
    }
    if (input$school == "Second Year") {
      return("dodgerblue4")
    }
    if (input$school == "Third Year") {
      return("darkgoldenrod1")
    }  
    if (input$school == "Fourth Year") {
      return("brown3")
    }
    else {
      return(NULL)
    }
  })
  
  fill.color.all = reactive({
    if (input$gender == "All") {
      return(data$Q47)
    }
    else {
      return(NULL)
    }
  })
  
  finddata = reactive({
    # if (input$school == "All") {
    #       (all.dataset.year())}
    if (input$gender == "All") {
      (all.dataset.gender())}
    if (input$school %in% c("First Year", "Second Year", "Third Year", "Fourth Year")){
      (dataset())}
    if (input$gender %in% c("Female", "Male", "Nonbinary", "Do not care to say")){
      (dataset())}
  })
  
  
  
  output$densityplot<-renderPlot({
    
    
    data = finddata()
    
    x.axis.value = reactive({
      if (input$score == "personal") {
        return(data$personal)}
      if (input$score == "social") {
        return(data$social)}
      if (input$score == "weight") {
        return(data$weight)}
      if (input$score == "eating") {
        return(data$eating)}
    })
    
    a = ggplot(data, aes_string(x=x.axis.value())) + geom_density(adjust=.75, fill = fill.color(), color = line.color(), alpha = 0.25, aes(fill = (fill.color.all()), color = line.color())) + theme(text = element_text(family = 'Gill Sans', color = "#444444"),plot.title = element_text(size = 24),axis.title = element_text(size = 18, color = '#555555'),axis.title.y = element_text(vjust = .5, angle = 0) ,axis.title.x = element_text(hjust = .5))+ coord_cartesian(xlim = c(0, 1)) + labs(title = paste0(input$gender, ", ", input$school, ", ", input$score)) 
    
    print(a)
    
  })}

shinyApp(ui, server)


