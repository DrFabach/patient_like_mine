extraction_context<- function(entities, text,path_fastContext=NULL, path_IAMsystemFastContext=NULL,sentence_tokenizer=NULL,
                      suppression_partie_atcd = T){
  
  
  if(suppression_partie_atcd){
    text<- suppression_partie_atcd(text)
    
  }
  require(rJava)
  if(is.null(path_fastContext)) path_fastContext<-"./sources/french_context_latest.txt"
  if(is.null(sentence_tokenizer))  sentence_tokenizer<-"(?<=(\\.|\\?|!))\\s|\n"
  if(is.null(path_IAMsystemFastContext)) path_IAMsystemFastContext<-"./sources/IAMsystemFastContext-0.0.1-jar-with-dependencies.jar"
  
  rJava::.jinit(force.init = T)
  rJava::.jaddClassPath(path_IAMsystemFastContext)
  
  termDetector<- rJava::.jnew(class = "fr.erias.IAMsystem.detect.TermDetector")
  
  
  df_mot<- data.frame(term =entities,code = "PR")
  apply(df_mot, 1, function(x){
    termDetector$addTerm(term = x[1], code=x[2])
    return(NULL)
  })
  
  fastContext<- rJava::.jnew(class = "edu.utah.bmi.nlp.fastcontext.FastContext",path_fastContext)
  
  
  list_res<- vector("list", length(text))
  for( i in 1:length(text)){
    
    
    phrases<-unlist(strsplit(text[i], sentence_tokenizer, perl=T))
    phrases<- phrases[nchar(phrases)>3]
    
    
    context_j<- vector("list", length("phrases"))
    for(j in seq_along(phrases)){
      ctcodes <- .jcall(termDetector,"Lfr/erias/IAMsystem/detect/DetectOutput;",method = "detect", phrases[j])
      
      
      if(.jcall(ctcodes, "S","toString")=="0 terms detected.\n"){
        context_j[[j]]<- data.frame(dictLabel="NA", phrase=j)
      }else{
        detectOutputcontext<- rJava::.jnew("fr.erias.IAMsystemFastContext.DetectOutputContext",ctcodes,fastContext)
        # lres <- ♣.jcall(dummyObj, "Ljava/lang/String;", "SayMyName")
        
        context_j[[j]] <-data.frame(
          (
            .jcall(
              .jcall(
                detectOutputcontext,
                "Lorg/json/JSONObject;",
                method = "getJSONObject",
                evalString = T
              ),
              "S",
              method = "toString",
              evalString = T
            ) %>%
              jsonlite::fromJSON(flatten = T)
          )$ct,phrase=j)
        
      }
    }
    
    list_res[[i]]<-data.frame(id= i ,do.call(plyr::rbind.fill, context_j))
    
  }
  
  res<-(do.call(plyr::rbind.fill, list_res))
  return(res)
}

suppression_partie_atcd<- function(texte, string_atcd = NULL, string_autre_partie = NULL){
  require(stringr)
  atcd<-c("antecedent",  "atcd")
  if(!is.null(string_atcd)) atcd<- tolower(unique(atcd,string_atcd ))
  autre_titre<-c("\n- osteoporose ", "\n- prothese ", "\n\t aide a ", "\n\talimentation ", 
                 "\n\tdate du poids ", "\n\tdegre ", "\n\tetat cutane ", "\n\timc ", 
                 "\n\tlever possible ", "\n\thauteur ", "\n\tpersonne ", "\n\tperte ", 
                 "\n\tpoids ", "\n\ttaille ", "\n\tvariation ", "\n\tvit avec ", 
                 "\n\tvit seul(e) ", "\nalimentation ", "\nallergie", "\nappareillage ", 
                 "\nautre prothese ", "\nbilan et ", "\ndouleur a ", "\nfacteurs de ", 
                 "\nisolement ", "\nlunettes ", "\nmateriels ", "\nmedication ", 
                 "\nmode de ", "\nnombre ", "\npersonnes a ", "\npoids / taille ", 
                 "\nprofession ", "\nprofession / ", "\nprothese ", "\nprovenance ", 
                 "\nsi allergie, ", "\nsituation ", "\nsur le plan ", "\ntexture ", 
                 "\ntraitements ", "\ntransit ", "\ntransport ", "\nvit seul(e) : ", 
                 "\r\n\r\n", "motif ")
  if(!is.null(string_autre_partie)) string_autre_partie<- tolower(unique(string_autre_partie,string_autre_partie ))
  
  
  
  
  
  texte<-str_replace_all(tolower(texte),
                         paste0("(^|\n|\r|- )(",
                                paste(atcd,collapse = "|"),
                                ")(.|\n|\r)*?(",
                                paste(autre_titre,collapse = "|"),
                                ")"),
                         "\\4")
}

term_PR<-c("poliarthrite rhumatoide", 
           "polyarthrite  rhumatoide", "polyarthrite rhumat", "polyarthrite rhumathide", 
           "polyarthrite rhumathoide", "polyarthrite rhumatiode", "polyarthrite rhumatise", 
           "polyarthrite rhumatismal", "polyarthrite rhumatismale", "polyarthrite rhumatismale", 
           "polyarthrite rhumato", "polyarthrite rhumatode", "polyarthrite rhumatodie", 
           "polyarthrite rhumatoid", "polyarthrite rhumatoide", 
           "polyarthrite rhumatoide",
           "polyarthrite rhumatoider", "polyarthrite rhumatoides", "polyarthrite rhumatoidesous", 
           "polyarthrite rhumatoidie", "polyarthrite rhumatoiide", "polyarthrite rhumatoiode", 
           "polyarthrite rhumatoire", "polyarthrite rhumatoise", "polyarthrite rhumatoisignes", 
           "polyarthrite rhumatologique", "polyarthrite rhumatotoide", "polyarthrite rhumatpide", 
           "polyarthrite rhumatpode", "polyarthrite rhumatroide", "polyarthriterhumatoide", 
           "polyartrite rhumat", "polyartrite rhumatdoide", "polyartrite rhumathoide", 
           "polyartrite rhumatiode", "polyartrite rhumatisme", "polyartrite rhumato", 
           "polyartrite rhumatoide", "polyartrite rhumatoides", "polyartrite rhumatoidie"
)

a<-extraction_context(entities = term_PR,text = c("Le patient est atteint de polyarthrite rhumatoide. ec",
                                                  "Le patient ne présente pas de signe de polyartrite rhumatoide"))

exploration_regex<- function(texte, pattern){
  texte<- tolower(texte)
  pattern<- tolower(pattern)
  texte<- texte[str_detect(texte, pattern)]
  
  retour<- sub(paste0("(.*[ ,.;:\\(\\{\r\n\\?\\)\\/\\-\\+]|^)(.*?",pattern,".*?)([ ,.;:\\(\\{\r\n\\?\\)\\/\\-\\+].*|$)"),"\\2",
               texte)
  
}
