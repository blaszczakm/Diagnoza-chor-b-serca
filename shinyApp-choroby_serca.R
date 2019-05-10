### Autor: Marek Błaszczak ###

#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("DT")
library(DT)


header<-dashboardHeader(title = "App menu")

menu<-dashboardSidebar(sidebarMenu(
  
  menuItem("Informations",
           menuSubItem("Heart diseases", tabName = "wyjasnienie"),
           menuSubItem("Dataset", tabName = "surowe_dane"),
           menuSubItem("About application", tabName = "about")),
  
  menuItem("Features importance", tabName = "waznosc"),
  
  menuItem("Choose algorithm",
           selectInput("model_wybor", "", 
                       choices = c("SVM",
                                   "KNN",
                                   "BAYES",
                                   "ctree2",
                                   "glm"),
                       selected = NULL),
           
           conditionalPanel("input.model_wybor=='SVM'",
                            selectInput("svm_kernel","Kernel",
                                        choices = c("Linear" = "svmLinear",
                                                    "Polynomial" = "svmPoly",
                                                    "Radial" = "svmRadial"),
                                        selected = "Linear")),
           
           conditionalPanel("input.model_wybor=='KNN'",
                            sliderInput("knn_k", "Num of neibours",
                                        min = 2, max=10, value = 5)),
           
           conditionalPanel("input.model_wybor=='BAYES'",
                            checkboxInput("bayes_kernel", "Kernel", T)),
           conditionalPanel("input.model_wybor=='BAYES'",
                            sliderInput("bayes_fL", "Laplace Corections:",
                                        min = 0, max = 5, step=1, value = 2)),
           conditionalPanel("input.model_wybor=='BAYES'",
                            sliderInput("bayes_adjust", "Adjust parameter:",
                                        min = 0, max=5 , step = 1, value = 2)),
           
           conditionalPanel("input.model_wybor=='ctree2'",
                            sliderInput("ctree2_d", "Tree depth:",
                                        min = 3, max = 15, value = 5)),
           conditionalPanel("input.model_wybor=='ctree2'",
                            sliderInput("ctree2_m", "Trust level:",
                                        min = 0, max = 1, value = 0.05)),
           
           
           sliderInput("cvrepeat", "Repeats of k-valadation:",
                       min = 1, max = 10, value = 5),
           
           menuSubItem("App author: Marek Błaszczak")),
  
  menuItem("Model fitting", tabName = "dopasowanie"),
  
  menuItem("Diagnose yourself",
           menuSubItem("Questions", "pytania"),
                       
           menuSubItem("Prediction (first answer the Q)", "diagnoza"))
))

body<-dashboardBody(tabItems(
  
  tabItem("about", verbatimTextOutput("about")),
  
  tabItem("surowe_dane", DTOutput("surowe_dane")),
  
  tabItem("wyjasnienie", verbatimTextOutput("wyjasnienie")),
  
  tabItem("waznosc",
          textOutput("info_istotnosc"),
          tabBox(title = "",
                 tabPanel("Correlation plot", plotOutput("wykres_kor")),
                 tabPanel("Correlation table", DTOutput("tabela_kor")),
                 tabPanel("Test Chi2", DTOutput("tabela_chi")),
                 tabPanel("Algorithm LVQ", plotOutput("wykres_lvq")),
                 tabPanel("Decision tree", plotOutput("wykres_ctree2")))
          ),
  
  tabItem("dopasowanie",
          tabBox(title = "",
                 tabPanel("Info",
                    textOutput("wybor"),
                    textOutput("dopasowanie_tren"),
                    textOutput("dopasowanie_test")
                 ),
                 br(),
          tabPanel("Accuracy plot",plotOutput("dopasowanie_wykres"))
          )),
  
  tabItem("pytania",
          
          fluidRow(
            
            # column max widt = 12 i rozlokowujesz column tak by
            # width sumowalo sie do 12
            column(width = 4,
                   uiOutput("my_age_ui"),
                   uiOutput("my_sex_ui"),
                   uiOutput("my_cp_ui"),
                   uiOutput("my_trestbps_ui")
            ),
            
            column(width = 4,
                   uiOutput("my_chol_ui"),
                   uiOutput("my_thalach_ui"),
                   uiOutput("my_fbs_ui"),
                   uiOutput("my_restecg_ui"),
                   uiOutput("my_exang_ui")
            ),
            
            column(width = 4,
                   uiOutput("my_oldpeak_ui"),
                   uiOutput("my_slope_ui"),
                   uiOutput("my_ca_ui"),
                   uiOutput("my_thal_ui")
            )
          ) #zamyka fulid row
  ),
  
   tabItem("diagnoza",
           DTOutput("dane_odp"),
           br(),
         textOutput("model_odp"),
         tableOutput("dane_odp_tab"))
  
)


)




ui<-dashboardPage(header, menu, body)


server<-function(input, output, session){
  
  dane<-read.csv("https://raw.githubusercontent.com/blaszczakm/Machine_learning-choroby_serca/master/heart.csv", sep = ",")
  
  info<-"Author: Marek Błaszczak"
  output$about<-renderText(paste(
    info, "Dataset from https://www.kaggle.com/ronitf/heart-disease-uci",
    "Technologies: R 3.5.3 (shiny, shinydashboard, DT, ggplot2)",
    sep = "\n"
  ))
  
  output$wyjasnienie <- renderText({
    paste("age - age in years","sex (1=male, 0=female)", "cp - chest pain type",
      "trestbps - resting blood pressure (in mm Hg on admission to the hospital)",
      "chol - serum cholestoral in mg/dl", "fbs - (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)",
      "restecg - resting electrocardiographic results", "thalach - maximum heart rate achieved",
      "exang - exercise induced angina (1 = yes; 0 = no)", 
      "oldpeak - ST depression induced by exercise relative to rest ", 
      "slope - the slope of the peak exercise ST segment", 
      "ca - number of major vessels (0-3) colored by flourosopy",
      "thal (1 = normal; 2 = fixed defect; 3 = reversable defect)",
      "target - heart disease (1 - yes, 0 -no)", sep="\n")
  })
    
 
    library(ggplot2)
    library(caret)
    
    str(dane)
    colnames(dane)
    
    #surowe dane
    output$surowe_dane<-renderDT(dane, options = list(scrollX=T, scrollY=T))
    #sprawdzenie brakow
    
    sapply(dane, function(x) sum(is.na(x))) #brak
    sapply(dane, function(x) length(which(x=="")))
    sapply(dane, function(x) length(which(x==" ")))
    sapply(dane, function(x) length(which(x=="?")))
    #dane nie zawieraja brakow
    
    #faktoryzacja
    summary(dane)
    str(dane)
    
    dane$age<-as.numeric(dane$age)
    dane$sex<-as.factor(dane$sex)
    dane$cp<-as.factor(dane$cp)
    dane$trestbps<-as.numeric(dane$trestbps)
    dane$chol<-as.numeric(dane$chol)
    dane$fbs<-as.factor(dane$fbs)
    dane$restecg<-as.factor(dane$restecg)
    dane$thalach<-as.numeric(dane$thalach)
    dane$exang<-as.factor(dane$exang)
    dane$oldpeak<-as.numeric(dane$oldpeak)
    dane$slope<-as.factor(dane$slope)
    dane$ca<-as.factor(dane$ca)
    dane$thal<-as.factor(dane$thal)
    dane$target<-as.factor(dane$target)
    
    summary(dane)
    
    
    ### renderowanie pytan ###
    
    output$my_age_ui <- renderUI(sliderInput("my_age", "Your age in years:",
                                             min=1, max = 120 , value = 28))
    
    output$my_sex_ui <-renderUI(sliderInput("my_sex", "Sex (0-female, 1-male)",
                min = 0, max = 1, value = 1, step = 1))
    
    output$my_cp_ui <- renderUI(sliderInput("my_cp", "Your cp:",
                   min = min(as.numeric(as.character(dane$cp))),
                   max = max(as.numeric(as.character(dane$cp))), step = 1, 
                   value = min(as.numeric(as.character(dane$cp))))
    )

     output$my_trestbps_ui <- renderUI(sliderInput("my_trestbps", "Your trestbps:",
                 min = min(dane$trestbps),
                 max = max(dane$trestbps), step = 1,
               value =median(dane$trestbps)))
    
    output$my_chol_ui <- renderUI(sliderInput("my_chol","Your choresterol:",
                        min = min(dane$chol),
                        max = max(dane$chol), step = 1,
                        value =median(dane$chol)))
      
     output$my_thalach_ui <- renderUI(sliderInput("my_thalach", "Your thalach:",
                        min = min(dane$thalach),
                        max = max(dane$thalach),
                        value =median(dane$thalach)))
      
      output$my_fbs_ui <- renderUI(sliderInput("my_fbs", "Fasting blood sugar >120",
                       min = min(as.numeric(as.character(dane$fbs))),
                        max = max(as.numeric(as.character(dane$fbs))),
                       step = 1, value = 0))
      
      output$my_restecg_ui <- renderUI(sliderInput("my_restecg", "Your rest ECG:",
                        min = min(as.numeric(as.character(dane$restecg))),
                        max = max(as.numeric(as.character(dane$restecg))),
                        step = 1, value = 0))
      
      output$my_exang_ui <- renderUI(sliderInput("my_exang", "Exang (Yes -1 No-0)",
                        min = min(as.numeric(as.character(dane$exang))),
                        max = max(as.numeric(as.character(dane$exang))), 
                        value = 1, step = 1))
      
      output$my_oldpeak_ui <- renderUI(sliderInput("my_oldpeak", "Your oldpeak: ",
                        min = min(dane$oldpeak),
                        max = max(dane$oldpeak),
                        value =median(dane$oldpeak)))
      
      output$my_slope_ui <- renderUI(sliderInput("my_slope", "Your slope type:",
                        min = min(as.numeric(as.character(dane$slope))),
                        max = max(as.numeric(as.character(dane$slope))),
                        value =median(as.numeric(as.character(dane$slope))),
                        step = 1))
      
      output$my_ca_ui <- renderUI(sliderInput("my_ca", "Your ca value:",
                          min = min(as.numeric(as.character(dane$ca))),
                        max = max(as.numeric(as.character(dane$ca))),
                          step = 1,
                          value =median(as.numeric(as.character(dane$ca)))
        ))
      
        output$my_thal_ui <- renderUI(sliderInput("my_thal", "Your thal value",
                           min = min(as.numeric(as.character(dane$thal))),
                           max = max(as.numeric(as.character(dane$thal))),
                           step = 1,
                           value =min(as.numeric(as.character(dane$thal)))
        ))
        
        #pomieszanie danych
        set.seed(667)
        dane<-dane[sample(nrow(dane)),]
        
        #info o zaleznosci zmiennych
        
        text<-"The following shows how significant the individual variables 
        have on the variable target."
        
        output$info_istotnosc<-renderText(text)
        
        ### sprawdzenie korelacji metoda Spearmana ###
        
        dane1<-dummyVars(target~.,dane)
        dane_m<-predict(dane1,newdata=dane)
        y<-as.numeric(as.character(dane$target))
        s<-data.frame(dane_m)
        s$target<-y
        str(s)
        
        #install.packages("reshape2")
        library(reshape2)
        
        c<-abs(cor(s))
        
        melted_cormat <- melt(c)
        wykres_kor<-ggplot(melted_cormat)+
          geom_tile(aes(x=Var1, y=Var2, fill=value), color = "white")+
          scale_fill_gradient2(low = "white", high = "red",
                               limit = c(0,1), space = "Lab",
                               name="Pearson\nCorrelation") +
          labs(x="",y="")+
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, vjust = 1,
                                           size = 9, hjust = 1))+
          coord_fixed()
        
        output$wykres_kor<-renderPlot(wykres_kor)
        
        ### sposob nr2 korelacja - tabelka ###
        
        kor_num<- t(data.frame(abs(cor(s, s$target))))
        rownames(kor_num)<-"coefficient"
        
        output$tabela_kor<-renderDT(kor_num, options=list(scrollX=T))
        
        ### sposob 3 test chi kwadrat dla danych kategorycznych
        # bez zmiany na numeryczne
        
        #poazanie zmiennych jakosciowych
        sapply(dane, function(x) is.factor(x))
        i<-as.vector(sapply(dane, function(x) is.factor(x)))
        i<-colnames(dane[,which(i==T)])
        i<-i[-which(i=="target")]
        i
        
        #zastosowanie testu chi-kwadrat dla kazdej zmiennej celem
        #obliczenia jej wplywu na zmienna target
        
        chi1<-chisq.test(table(dane[,c("sex","target")]))$p.value
        chi2<-chisq.test(table(dane[,c("cp","target")]))$p.value
        chi3<-chisq.test(table(dane[,c("fbs","target")]))$p.value
        chi4<-chisq.test(table(dane[,c("restecg","target")]))$p.value
        chi5<-chisq.test(table(dane[,c("exang","target")]))$p.value
        chi6<-chisq.test(table(dane[,c("slope","target")]))$p.value
        chi7<-chisq.test(table(dane[,c("ca","target")]))$p.value
        chi8<-chisq.test(table(dane[,c("thal","target")]))$p.value
        
        wek_chi<-c(chi1,chi2,chi3,chi4,chi5,chi6,chi7,chi8)
        wek_chi
        
        chi_table<-data.frame("p.value"=wek_chi)
        rownames(chi_table)<-i
        chi_table<-t(chi_table)
        chi_table
        
        output$tabela_chi<-renderDT(chi_table, options = list(scrollX=T))
        
        
        ### sprawdzenie istotnosci zmiennych modelem LVQ ###
        control_lvp <- trainControl(method="repeatedcv", number=10, repeats=3)
        
        ist_lvq <- train(target~., data=dane, 
                         method="lvq", preProcess="scale",
                         trControl=control_lvp)
        
        importance_lvq <- varImp(ist_lvq, scale=FALSE)
        
        output$wykres_lvq<-renderPlot(plot(importance_lvq))
        
        ### sprawdzenie istotnosci zmiennych modelem drzewa decyzyjnego ###
        
        ist_tree<-train(target~., data=dane,
                        method = "ctree2", trControl = control_lvp)
        
        importance_tree<-varImp(ist_tree, scale=F)
        
        output$wykres_ctree2<-renderPlot(plot(importance_tree))
        
        # metoda 4 budowanie modeli random forest dla kazdego podzbioru zbioru danych
        # celem znalezienia podzbioru dajacego najlepsze wyniki
        
        # control_rfe <- rfeControl(functions=rfFuncs, method="cv", number=10)
        # # run the RFE algorithm
        # results <- rfe(target~.,data = dane,rfeControl=control_rfe)
        # 
        # plot(results, type=c("g", "o"))
        # results$results
        
        #Istotnosc zmiennych mozna sprawdzic tez poprzez
        #budowe modelu np RF a pozniej plot(VarImp(model))
        
        ### rozpoczecie sekcji zaleznej od inputow
        
        observe({
        
    ### BUDOWANIE MODELI ###
    
    ## podzial danych one hot encoding
    # dla modeli glm i knn
    
    #rezygnacja ze zmiennej fbs 
    #z powodu na mala istotnosc zmiennej
    o<-which(colnames(dane)=="fbs")
    dane<- dane[,-o]
    y<-dane$target
    dane_dummy<-dummyVars(target~.,dane)
    dane01<-predict(dane_dummy,newdata=dane)
    dane01<-data.frame(dane01)
    dane01$target<-y
    str(dane01)
    
    ### Podzial na zbiory treningowe i testowe
    
    set.seed(100)
    wiersze_trening <- createDataPartition(dane$target, p=0.8, list = F)
    
    dane_trening <- dane[wiersze_trening,]
    dane_test <- dane[-wiersze_trening,]
    str(dane_test)
    dim(dane_trening)
    dim(dane_test)
    
    dane01_trening <- dane01[wiersze_trening,]
    dane01_test <- dane01[-wiersze_trening,]
    str(dane01_test)
    str(dane01_trening)
    
    control <- trainControl(method="repeatedcv", number=10, 
                            repeats = input$cvrepeat)
    

      if(!is.null(input$model_wybor)){
        
        output$wybor<-renderText({paste("Your's classification model: ", input$model_wybor)})
        
        if(input$model_wybor=="SVM"){
          metoda<- as.character(input$svm_kernel)
          model <-train(target~., data = dane_trening,
                                        method = metoda,
                                        preProcess = c("range","pca"),
                                    trControl = control)
        }
        
         if(input$model_wybor=="KNN"){
         
               modelLookup("knn") #podglad parametrow metody
               grid <- data.frame(k = input$knn_k)
               model<-train(target~., data = dane01_trening,
                            method = "knn",
                            preProcess = c("center","scale"),
                            trControl = control,
                            tuneGrid = grid)
         }
        
        if(input$model_wybor=="glm"){
        
          modelLookup("glm") #podglad parametrow
                 model<-train(target~., data = dane01_trening,
                              method = "glm",
                              preProcess = c("center","scale"),
                              trControl = control)
           
        }
        
        if(input$model_wybor=="ctree2"){
         
               modelLookup("ctree2") #podglad parametrow
               grid <- data.frame(maxdepth = as.integer(input$ctree2_d),
                                  mincriterion = 1 - as.double(input$ctree2_m))
               model<-train(target~., data = dane_trening,
                            method = "ctree2",
                            preProcess = c("range","pca"),
                            trControl = control,
                            tuneGrid = grid)
        
        
        }
        
        if(input$model_wybor=="BAYES"){
                 
                 modelLookup("nb") #podglad parametrow
                 grid <- data.frame(usekernel = input$bayes_kernel,
                                     fL = input$bayes_fL,
                                     adjust = input$bayes_adjust)
                 model<-train(target~., data = dane_trening,
                              method = "nb",
                              preProcess = c("range","pca"),
                              trControl = control,
                              tuneGrid = grid)
                 
        }
        
        accuracy_tren<-max(model$results[,"Accuracy"])
        output$dopasowanie_tren<-renderText(paste("Accuracy on train dataset:",
                                                  round(accuracy_tren*100,2),"%"))
        
        if(input$model_wybor=="KNN" || input$model_wybor=="glm"){
          a <- length(dane01_test$target)
          p <- predict(model, dane01_test)
          accuracy_test<-sum(dane01_test$target==p)/a
          #do wykresu ramka
          wyniki <- data.frame("Rzeczywista" = dane01_test$target, 
                               "Przewidziana" = predict(model, dane01_test))
        }
        
        else{
          a <- length(dane_test$target)
          p <- predict(model, dane_test)
          accuracy_test<-sum(dane_test$target==p)/a
          #ramka do wykresu
          wyniki <- data.frame("Rzeczywista" = dane_test$target, 
                               "Przewidziana" = predict(model, dane_test))
          
        }
          
        output$dopasowanie_test<-renderText(paste("Accuracy on test dataset:",
                                                  round(accuracy_test*100,2),"%"))
        
         # wykres dopasowania modelu
        
         cols<-c("c1"="black", "c2"="red")
         shapes<-c("s1"=3, "s2"=4)
         wykres_dop<-ggplot(wyniki,aes(x=seq(length(Rzeczywista))))+
           geom_point(aes(y=Rzeczywista),shape = 3, color="black")+
           geom_point(aes(y=Przewidziana),shape=4, color="red")+
           labs(x="Observation", y="target")+
           scale_fill_manual(name = "lol",
                             breaks = c("p1","p2"),
                             
                             labels = c("tak","nie"))
         
         output$dopasowanie_wykres<-renderPlot(wykres_dop)
        
        ### prewidywanie choroby serca na podstawie
        ### odpowiedzi uzytkownika
         
         q <- c(input$my_age, input$my_sex, input$my_cp, input$my_trestbps,
                input$my_chol, input$my_restecg, input$my_thalach,input$my_exang,
                input$my_oldpeak, input$my_slope, input$my_ca, input$my_thal)
         
         if(length(q)==12){
         # 12 bo 12 pytan
           
           dane_odp<-dane[1,-which(colnames(dane)=="target")]
           
           age<-as.numeric(input$my_age)
           sex<-(input$my_sex)
           cp<-(input$my_cp)
           trestbps<-as.numeric(input$my_trestbps)
           chol<-as.numeric(input$my_chol)
           #fbs<-(input$my_fbs) #nie bierzemy pod uwage fbs przy modelowaniu
           restecg<-as.numeric(input$my_restecg)
           thalach<-as.numeric(input$my_thalach)
           exang<-(input$my_exang)
           oldpeak<-as.numeric(input$my_oldpeak)
           slope<-(input$my_slope)
           ca<-(input$my_ca)
           thal<-(input$my_thal)
           
           ### tworzenie ramki danych odpowiedzi 
           
           odp<-c(age,sex,cp,trestbps,chol,restecg,thalach,exang,
                  oldpeak,slope,ca,thal)
           dane_odp[1,]<-odp
           str(dane_odp)
           dane_odp$age<-as.numeric(dane_odp$age)
           dane_odp$trestbps<-as.numeric(dane_odp$trestbps)
           dane_odp$chol<-as.numeric(dane_odp$chol)
           dane_odp$thalach<-as.numeric(dane_odp$thalach)
           dane_odp$oldpeak<-as.numeric(dane_odp$oldpeak)
           
           dane_odp_correct<-dane_odp #ramka danych do wyswietlenia
           rownames(dane_odp_correct)<-"Your answer"
           output$dane_odp<-renderDT(dane_odp_correct,options = list(scrollX=T, 
                                                                     scrollY=F))
        
        if(input$model_wybor=="KNN" || input$model_wybor=="glm"){
          d<-dummyVars(~.,dane_odp)
           dane01_odp<-predict(d,newdata=dane_odp)
           dane01_odp<-as.data.frame(dane01_odp)
           model_odp <- predict(model, dane01_odp)
           answer <- ifelse(model_odp==1,"YES","NO")
          output$model_odp <- renderText({paste("Will you have heart disease? : ",
                                                  answer)})
        }
           
        else{
          model_odp <- predict(model, dane_odp)
          answer <- ifelse(model_odp==1,"YES","NO")
           output$model_odp <- renderText({paste("Will you have heart disease? : ",
                                                 answer)})
        }
        
        
      } #zamyka if dla pytan
    
    
    } #zamyka if dla wyboru modelu
    
  }) #zamyka observe
  
} #zamyka server


shinyApp(ui=ui, server=server)
