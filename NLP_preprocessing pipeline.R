#NLP Pipeline 
# initial creation date: 03/11/2021
# version: 1.54
# current version date: 11/07/2024
# author: Carlo Santagiustina
# email: carlo.santagiustina@sciencespo.fr
# Please do not share this code with people outside the DMI 2024 Summer School group
options(encoding='UTF-8')

# Load libraries
#Rstudio should ask you to install it 
install.packages(c("devtools","tidyverse","quanteda","newsmap","ggplot2","stm","spacyr","visNetwork"))
devtools::install_github("hadley/emo")
devtools::install_github("richfitz/remoji")
require(quanteda)
require(newsmap)
require(ggplot2)
require(spacyr)
require(remoji)
require(emo)
require(tidyverse)
library(readr)
library(visNetwork)
#Import data using the readr library
# data=readr::read_csv(file = "[filename].csv",col_types = cols(id = col_character()
#)
#                                                                         
# )


data$language_descr=textcat::textcat(data$text)# infer language using textcat
table(data$language_descr) %>% sort(decreasing = T)
dict_emoji=remoji:::dat_core$emoji
names(dict_emoji)=remoji:::dat_core$description


data=as.data.frame(data)
#data=data[!is.na(data$text),] # remove  rows with empty text
rownames(data) =data$id

data=data[, !(colnames(data) %in% colnames(data)[duplicated(colnames(data))])]
data=data[is.na(data$retweeted_id),]# keep only original tweets

data_clean= data %>% #remove emojis
  dplyr::mutate(emoji = emo::ji_match_all(user_description)) %>% #remove all strange characters (emoji, etc)
  dplyr::mutate(clean_text = iconv(
    user_description,
    to = "latin1",
    from = "UTF-8" ,
    sub = ' '
  )) %>% #clean & character
  dplyr::mutate(clean_text = gsub(
    "&amp; ", #this is the & character in HTML
    "and ",
    clean_text,
    ignore.case = FALSE,
    perl = TRUE
  )) %>% #clean & character
  dplyr::mutate(clean_text = gsub(
    " &amp;",
    " and",
    clean_text,
    ignore.case = FALSE,
    perl = TRUE
  ))%>% #clean & character
  dplyr::mutate(clean_text = gsub(
    "&amp;",
    "&",
    clean_text,
    ignore.case = FALSE,
    perl = TRUE
  )) %>% #clean & character
  dplyr::mutate(clean_text = gsub(
    "Q&amp;A",
    "Q&A",
    clean_text,
    ignore.case = FALSE,
    perl = TRUE
  ))  %>% #extract urls
  dplyr::mutate(text_urls = regmatches(
    clean_text,
    gregexpr(
      "(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)",
      #"(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,}[^ [:punct:]])",
      clean_text,
      perl = TRUE
    )
  )) %>% #remove urls
  dplyr::mutate(
    clean_text = gsub(
      "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
      #"(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,}[^ [:punct:]])",
      " ",
      clean_text,
      perl = TRUE
    )
  ) %>%  #extract #tags
  dplyr::mutate(text_hashtags = regmatches(
    clean_text,
    gregexpr(
      "([#]{1}[a-zA-Z0-9_]{1,})",
      clean_text,
      perl = TRUE
    )
  )) %>%
  #remove # characters of from words
  dplyr::mutate(clean_text = sapply(
    regmatches(
      clean_text,
      gregexpr("([#]{1})(?=[a-zA-Z0-9_]{1,})", clean_text, perl = TRUE),
      invert = TRUE
    ),
    paste0,
    collapse = " "
  ))  %>%  #extract $tags
  dplyr::mutate(text_fintags = regmatches(
    clean_text,
    gregexpr(
      "([$]{1}[a-zA-Z]{2,8})",
      clean_text,
      perl = TRUE
    )
  ))  %>% #remove $ characters of finance tags
  dplyr::mutate(clean_text = sapply(
    regmatches(
      clean_text,
      gregexpr("([$]{1})(?=[A-Za-z]{2,8})", clean_text, perl = TRUE),
      invert = TRUE
    ),
    paste0,
    collapse = " "
  ))%>% #remove @ characters of user tags
  dplyr::mutate(clean_text = sapply(
    regmatches(
      clean_text,
      gregexpr("([@]{1})(?=[ ]?[A-Za-z0-9]{2,8})", clean_text, perl = TRUE),
      invert = TRUE
    ),
    paste0,
    collapse = " "
  ))%>% #extract  standard character emoticones
  dplyr::mutate(text_emoticones = regmatches(
    clean_text,
    gregexpr(pattern = "([>]{0,1}[:;=][']{0,1}[c\\-? ?\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-? ?\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)", clean_text, perl = TRUE)
  )) %>% #remove emoticones
  dplyr::mutate(
    clean_text = gsub(pattern = "([>]{0,1}[:;=][']{0,1}[c\\-? ?\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-? ?\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)", " ", clean_text, perl = TRUE)
  )

saveRDS(data_clean,file = "processed_data.RData")

data=readRDS("processed_data_tweets.RData")


#### NLP pipeline for NER and for extracting dep-rel ####
library(spacyr)# pacchetto utilizzato
reticulate::install_miniconda(path = "~/Documents/Miniconda", update = TRUE, force = FALSE)# run it only once, the first time you use the code
Sys.setenv(RETICULATE_MINICONDA_PATH="~/Documents/Miniconda")
reticulate::condaenv_exists()
reticulate::conda_create(
  envname = "spacyr",
  packages = "spacy",
  forge = TRUE,
  channel = "conda-forge")# run it only once
reticulate::condaenv_exists()
reticulate::use_miniconda(condaenv = "~/Documents/Miniconda/envs/spacyr/")
spacyr::spacy_download_langmodel(
   envname = "spacyr",model =  "fr_core_news_md"# download french model
 )# run it only once
spacy_initialize('fr_core_news_md',condaenv = "~/Documents/Miniconda/envs/spacyr/")
#spacy_install()
mySpacyrConsolidateEnt=function (x, concatenator = "_") 
{ 
  library(tidyverse)
  spacy_result <- data.table::as.data.table(x)
  lemma_space <- token_space <- NULL
  entity <- entity_type <- entity_count <- iob <- entity_id <- .N <- .SD <- `:=` <- token <- lemma <- pos <- tag <- new_token_id <- token_id <- sentence_id <- doc_id <- NULL
  if (!"entity" %in% names(spacy_result)) {
    stop("no entity in parsed object: rerun spacy_parse() with entity = TRUE")
  }
  spacy_result[, `:=`(entity_type, sub("_.+", "", entity))]
  spacy_result[, `:=`(iob, sub(".+_", "", entity))]
  extended_list <- c("DATE", "TIME", "PERCENT", "MONEY", "QUANTITY", 
                     "ORDINAL", "CARDINAL")
  spacy_result[, `:=`(entity_count, ifelse(iob == "B" | iob == 
                                             "", 1, 0))]
  spacy_result[, `:=`(entity_id, cumsum(entity_count)), 
               by = c("doc_id", "sentence_id")]
  spacy_result[, `:=`(token_space, token)]
  
  spacy_result[, `:=`(lemma_space, lemma)]
  
  spacy_result_modified = spacy_result %>% 
    group_by(doc_id, sentence_id, entity_id) %>% 
    mutate(token_entity= paste0(token_space, collapse = concatenator))
  spacy_result_modified = spacy_result_modified %>% 
    group_by(doc_id, sentence_id, entity_id) %>% 
    mutate(lemma_entity= paste0(lemma_space, collapse = concatenator))
  
  spacy_result_modified$ent_root=grepl(pattern = "B",x = spacy_result_modified$iob,perl = T)
  
  spacy_result_modified= spacy_result_modified[spacy_result_modified$ent_root==T | (spacy_result_modified$iob %in% c("")),]
  
  
  ret <- as.data.frame(spacy_result_modified)
  class(ret) <- c("spacyr_parsed", class(ret))
  ret
}

#spacyr::spacy_install(envname = "spacy_condaenv",version = "latest")#installa conda su pc
spacy_initialize('fr_core_news_md',condaenv =  "~/Documents/Miniconda/envs/spacyr/")

data_bkp=data_clean
data_clean$doc_id=paste0("text",1:nrow(data_clean))#check this!
#data$text=gsub("ndr"," ",x = data$text,ignore.case = T)
rownames(data_clean)=data_clean$doc_id
saveRDS(data_clean,file = "data_before_NLP_text.RDS")
NLP_data_raw=spacy_parse(data_clean$clean_text, dependency = TRUE, pos = TRUE,entity = T,multithread = T) #,"morph"
spacy_finalize()
saveRDS(NLP_data_raw,"raw_data_tweets.RDS")
NLP_data_entities=mySpacyrConsolidateEnt(NLP_data_raw,concatenator = " ")

saveRDS(NLP_data_entities,"NLP_data_entities_tweets.RDS")

NLP_data_entities=NLP_data_entities[!((NLP_data_entities$pos %in% c("PUNCT","SPACE","SYM","X"))),]#"AUX","CCONJ","SYM","PRON","DET","INTJ","PART","ADP","SCONJ","X"

NLP_data_entities=NLP_data_entities[grepl("[[:alnum:]]{1,}",x = NLP_data_entities$token_entity), ]

NLP_data_entities$token_entity=gsub(pattern = "_-_",replacement = "-",x = NLP_data_entities$token_entity,ignore.case = T,perl = T)
NLP_data_entities$token_entity=gsub(pattern = "_[[:space:]]{1,}_",replacement = "_",x = NLP_data_entities$token_entity,ignore.case = T,perl = T)

#NLP_data_entities=NLP_data_entities[nchar(NLP_data_entities$token_entity)>=3, ]#

NLP_data_entities$token_entity = tolower(NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("’",replacement = "'",x = NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("'_",replacement = "'",x = NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("^[[:punct:]]{1,}|[[:punct:]]{1,}$",replacement = "",x = NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("[[:space:]]{2,}",replacement = " ",x = NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("^[_]{1,}|[_]{1,}$",replacement = "",x = NLP_data_entities$token_entity)
NLP_data_entities$token_entity=gsub("&amp;",replacement = "&",x = NLP_data_entities$token_entity)


saveRDS(NLP_data_entities,paste0("NLP_data_entities_final.RDS"))
#NLP_data_entities=readRDS(paste0("NLP_data_entities_final_en_",Sys.Date(),".RDS"))

source_nodes= NLP_data_entities %>%
  dplyr::select(doc_id, 
                sentence_id, 
                token_0 = token_entity,
                token_0_id=token_id,
                pos_0=pos,
                dep_rel_01=dep_rel,
                token_1_id=head_token_id
  ) 

target_nodes= NLP_data_entities %>% dplyr::select(doc_id,
                                                  sentence_id, 
                                                  pos_1=pos,
                                                  token_1 = token_entity,
                                                  token_1_id=token_id,
                                                  #lemma1 = lemma_nounphrase,
                                                  dep_rel_12=dep_rel,
                                                  token_2_id=head_token_id
)

net_data=dplyr::left_join(
  source_nodes[source_nodes$dep_rel_01!="ROOT",] ,
  target_nodes,
  by = join_by(
    doc_id, 
    sentence_id, 
    token_1_id == token_1_id
  )
)


#net_data=net_data[net_data$dep_rel!="ROOT",]
net_data=net_data[!is.na(net_data$token_0) & !is.na(net_data$token_1),]

#net_data[,c("word_source","word_target")]
net_data=net_data[!(net_data$token_0 == net_data$token_1),]#remove loops

net_data$token_id=paste(net_data$token_0_id,"|",net_data$token_0_id)#paste("[",net_data$token_0_id,"|",net_data$token_0_id,"]")

net_data$id_up_01=net_data$token_0_id<net_data$token_1_id

net_data$edge_01=NA

net_data$edge_01[net_data$id_up_01]=paste(net_data$token_0[net_data$id_up_01],net_data$token_1[net_data$id_up_01],sep = "->")

net_data$edge_01[!net_data$id_up_01]=paste(net_data$token_1[!net_data$id_up_01],net_data$token_0[!net_data$id_up_01],sep = "->")

temp_data=left_join(net_data,data_clean[,c("doc_id","user_id","user_image")],by = join_by("doc_id"=="doc_id"))
write_csv(temp_data,paste0("deprel_dataedges_final.csv"))

saveRDS(net_data,paste0("net_data.RDS"))


NLP_data_entities$refdoc_id=NLP_data_entities$doc_id
NLP_data_entities$doc_id= paste(NLP_data_entities$doc_id, NLP_data_entities$sentence_id,sep = "_")
quanteda_unigrams=quanteda::as.tokens(NLP_data_entities %>% dplyr::select(doc_id, sentence_id, token_id, token = token_entity)) %>% quanteda::tokens_tolower()

net_data$refdoc_id=net_data$doc_id
net_data$doc_id= paste(net_data$doc_id, net_data$sentence_id,sep = "_")
quanteda_associations=quanteda::as.tokens(net_data  %>% dplyr::select(doc_id, sentence_id, token_id, token = edge_01)) %>% quanteda::tokens_tolower()

#BUILD DFMs

#unigrams_dfm =quanteda_unigrams[names(quanteda_associations)]%>% quanteda::dfm()
unigrams_dfm =quanteda_unigrams%>% quanteda::dfm()
unigrams_dfm = unigrams_dfm %>%
  dfm_group(.,
            groups = gsub("_[0-9]{1,}$",
                          "",
                          x = docnames(unigrams_dfm)))

####gsub("_[0-9]{1,}$","", x = docnames(unigrams_dfm)

associations_dfm= quanteda_associations %>% quanteda::dfm()
# colnames(associations_dfm)= gsub(pattern = "<-",
#                                  replacement ="->",x = colnames(associations_dfm))
#ncol(associations_dfm)
#associations_dfm =associations_dfm %>% quanteda::dfm_compress(.,margin = "features")
#ncol(associations_dfm)
associations_dfm= associations_dfm %>%
  dfm_group(.,
            groups = gsub("_[0-9]{1,}$",
                          "",
                          x = docnames(associations_dfm)))
nrow(unigrams_dfm)
nrow(associations_dfm)
saveRDS(unigrams_dfm,paste0("raw_unigrams_dfm_tweets.RDS"))
saveRDS(associations_dfm,paste0("raw_associations_dfm_tweets.RDS"))
table(colSums(associations_dfm)>=2)
table(colSums(unigrams_dfm)>=2)
# unigrams_dfm=unigrams_dfm_bkp
# associations_dfm=associations_dfm_bkp
unigrams_dfm_bkp=unigrams_dfm
associations_dfm_bkp=associations_dfm
# min_freq_uni=round((1/100000)*nrow(data))#once every ten thousand sentences
# min_freq_bi=round((1/100000)*nrow(data))#once every ten thousand sentences
# unigrams_dfm=dfm_trim(unigrams_dfm, min_termfreq = min_freq_uni, docfreq_type = "count") 
# associations_dfm=dfm_trim(associations_dfm, min_termfreq = min_freq_bi, docfreq_type = "count")#1 ogni 100 doc

stopwords=c(quanteda::stopwords(language = "fr"),"re","rt","na","si","i",c(1:9))

#unigram_counts=data.frame(term=featnames(unigrams_dfm), freq=colSums(unigrams_dfm))
#unigram_counts[!(unigram_counts$term %in% stopwords),]%>% View()

unigrams_dfm=quanteda::dfm_select(unigrams_dfm,
                                  selection = c("remove"),
                                  valuetype="fixed",
                                  pattern=stopwords)

stopwords_regex=paste(paste("->",stopwords,"$",sep = "",collapse = "|"),paste("<-",stopwords,"$",sep = "",collapse = "|"),paste("^",stopwords,"->",sep = "",collapse = "|"),paste("^",stopwords,"<-",sep = "",collapse = "|"),sep = "|")

associations_dfm=quanteda::dfm_select(associations_dfm,
                                      selection = c("remove"),
                                      valuetype="regex",
                                      pattern=stopwords_regex)

#colnames(associations_dfm)=gsub(pattern = "[<][-]","->",x = colnames(associations_dfm))
#associations_dfm=dfm_compress(associations_dfm,margin ="features")

rownames(data_clean)=data_clean$doc_id
  docvars(unigrams_dfm)=data_clean[docnames(unigrams_dfm),]
docvars(associations_dfm)=data_clean[docnames(associations_dfm),]

unigrams_df= tibble(name=featnames(unigrams_dfm), frequency=colSums(unigrams_dfm )) 
unigrams_df%>% wordcloud2::wordcloud2(.) #plot wordcloud of unigrams (words)

  
  associations_df= tibble(name=featnames(associations_dfm), frequency=colSums(associations_dfm ) ) 
associations_df%>% wordcloud2::wordcloud2(.) #plot wordcloud of dep.rel (word associations)


docnames(associations_dfm)[which(!(docnames(associations_dfm) %in% rownames(data_clean)))]

intersection_unigrams_dfm=unigrams_dfm[docnames(associations_dfm),]
nrow(intersection_unigrams_dfm)
ncol(intersection_unigrams_dfm)
identical(docnames(associations_dfm),docnames(intersection_unigrams_dfm))
dfm_intersection = cbind(intersection_unigrams_dfm, associations_dfm)
#docvars(dfm_intersection)
colnames(dfm_intersection)=gsub(pattern = "^[[:punct:]_]{1,}|[[:punct:]_]{1,}$",replacement = "",x =  featnames(dfm_intersection),perl = T)
dfm_intersection=dfm_compress(dfm_intersection,margin = "features")

docvars(dfm_intersection)=data_clean[docnames(dfm_intersection),]

saveRDS(dfm_intersection ,paste0("dfm_intersection.RDS"))

complement_unigrams_dfm=unigrams_dfm[!(docnames(unigrams_dfm) %in% docnames(associations_dfm)),]
nrow(complement_unigrams_dfm)
ncol(complement_unigrams_dfm)
complement_associations_dfm=Matrix::Matrix(data=0, nrow=nrow(complement_unigrams_dfm), ncol=ncol(associations_dfm), byrow=FALSE, dimnames=list(docnames(complement_unigrams_dfm),featnames(associations_dfm)), sparse = TRUE, doDiag = TRUE, forceCheck = T) %>% as.dfm()

nrow(complement_associations_dfm)
ncol(complement_associations_dfm)

identical(docnames(complement_associations_dfm),docnames(complement_unigrams_dfm))
dfm_complement= cbind(complement_unigrams_dfm, complement_associations_dfm)
docvars(dfm_complement)=docvars(complement_unigrams_dfm)


#### COMBINING DFMs ####

identical(featnames(dfm_intersection),featnames(dfm_complement))# TRUE is good !
intersect(docnames(dfm_intersection),docnames(dfm_complement)) #character(0) is good!

#dfm= rbind(dfm_intersection, dfm_complement[,featnames(dfm_intersection)])
dfm= rbind(dfm_intersection, dfm_complement)

#docvars(dfm)=data[docnames(dfm),]
docvars(dfm)=data_clean[gsub("_[0-9]{1,}$","", x = docnames(dfm)),]

saveRDS(dfm ,paste0("dfm_final.RDS"))

# Plot Dep.Rel network filtered by word or two words expression


#### Word egonetwork #####

### function to build dep rel network
egonet2=function(filter_term, filter_term2="",filter=0.5, save=TRUE, dfm_=dfm,max_edges=500,max_nodes=2000, ...){
  library(visNetwork)
  color.gradient_2 =   function(x,
                                colors = c("lightgray", "darkgray"),
                                colsteps = 1000) {
    return(colorRampPalette(colors) (colsteps) [findInterval(x, seq(0, max(bigrams_edges$weight), length.out =
                                                                      colsteps))])
  }
  
  #filter = 0.5
  #filter_term="macron"
  #filter_term2=""
  if(filter_term2!="") {
    keep = tolower(paste(
      paste(gsub(pattern = " ",replacement = "_",paste0("^",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|^"),
      paste(gsub(pattern = " ",replacement = "_",paste0("_",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|_"),
      paste(gsub(pattern = " ",replacement = "_",paste0("[-][>]",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|[-][>]"),sep = "|"))
    
    keep2 = tolower(paste(
      paste(gsub(pattern = " ",replacement = "_",paste0("^",filter_term2)),gsub(pattern = " ",replacement = "",filter_term2),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term2,"$")),sep = "$|^"),
      paste(gsub(pattern = " ",replacement = "_",paste0("_",filter_term2)),gsub(pattern = " ",replacement = "",filter_term2),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term2,"$")),sep = "$|_"),
      paste(gsub(pattern = " ",replacement = "_",paste0("[-][>]",filter_term2)),gsub(pattern = " ",replacement = "",filter_term2),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term2,"$")),sep = "$|[-][>]"),sep = "|"))
    
    keep_docs=   intersect(names(which(quanteda::dfm_select(dfm_,  selection = c("keep"),  valuetype="regex", pattern=keep) %>% rowSums()>=1)),
                           names(which(quanteda::dfm_select(dfm_,  selection = c("keep"),  valuetype="regex", pattern=keep2) %>% rowSums()>=1)))
    
    dfm_temp =dfm[keep_docs,]
    dfm_temp =dfm_temp[,colSums(dfm_temp)>=1]
    matched=featnames(dfm_temp)[grepl(pattern = keep,x = featnames(dfm_temp)) |grepl(pattern = keep2,x = featnames(dfm_temp)) ]
  }else{
    
    keep = tolower(paste(
      paste(gsub(pattern = " ",replacement = "_",paste0("^",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|^"),
      paste(gsub(pattern = " ",replacement = "_",paste0("_",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|_"),
      paste(gsub(pattern = " ",replacement = "_",paste0("[-][>]",filter_term)),gsub(pattern = " ",replacement = "",filter_term),gsub(pattern = " ",replacement = "[-][>]",paste0(filter_term,"$")),sep = "$|[-][>]"),sep = "|"))
    
    keep_docs=   names(which(quanteda::dfm_select(dfm_,  selection = c("keep"),  valuetype="regex", pattern=keep) %>% rowSums()>=1))
    dfm_temp =dfm_[keep_docs,]
    dfm_temp =dfm_temp[,colSums(dfm_temp)>=1]
    matched=featnames(dfm_temp)[grepl(pattern = keep,x = featnames(dfm_temp))]
  }
  
  #rownames(data)=data$textID
  #p_threshold=.00001
  
  unigrams = colnames(dfm_temp)[!grepl("^.{1,}->{1}.{1,}$", colnames(dfm_temp))]
  unigrams_probs = colSums(dfm_temp)[!grepl("^.{1,}->{1}.{1,}$", colnames(dfm_temp))]
  unigrams_probs = unigrams_probs / sum(unigrams_probs)
  names(unigrams_probs) = unigrams
  unigrams_probs=sort(unigrams_probs,decreasing = T)
  #filter=0.5
  p_threshold = quantile(unigrams_probs, probs = filter)
  bigrams_nodes = data.frame(id = names(unigrams_probs)[(unigrams_probs >= p_threshold) |
                                                          (names(unigrams_probs) %in% matched)],
                             label = names(unigrams_probs)[(unigrams_probs >= p_threshold) |
                                                             (names(unigrams_probs) %in% matched)],
                             stringsAsFactors = F)
  rownames(bigrams_nodes)=bigrams_nodes$label
  bigrams_nodes$value = unigrams_probs[rownames(bigrams_nodes)]
  
  bigrams = colnames(dfm_temp)[grepl("^.{1,}->{1}.{1,}$", colnames(dfm_temp))]
  bigrams_probs = colSums(dfm_temp)[grepl("^.{1,}->{1}.{1,}$", colnames(dfm_temp))]
  bigrams_probs = bigrams_probs / sum(bigrams_probs)
  p_threshold_edges = quantile(bigrams_probs, probs = filter)
  names(bigrams_probs) = bigrams
  bigrams_probs=sort(bigrams_probs,decreasing = T)
  bigrams_edges = data.frame(
    from = gsub("[-]{1}[>]{1}.{1,}$", "", x = names(bigrams_probs) ),
    to = gsub("^.{1,}[-]{1}[>]{1}", "", x = names(bigrams_probs) ),
    weight = bigrams_probs,
    edge_name=names(bigrams_probs),
    smooth.enabled = T,
    smooth.roundness = 0.50,
    stringsAsFactors = F
  )
  
  bigrams_edges = bigrams_edges[(bigrams_edges$from %in% bigrams_nodes$id) &
                                  (bigrams_edges$to %in% bigrams_nodes$id) &
                                  bigrams_edges$weight >= p_threshold_edges, ]
  
  bigrams_edges$title = paste(
    "From-To: '",
    bigrams_edges$from,
    "'-'",
    bigrams_edges$to,
    "', Prob: ",
    round(bigrams_edges$weight, 6),
    sep = ""
  )
  
  #bigrams_nodes = bigrams_nodes[bigrams_nodes$id %in% c(bigrams_edges$from, bigrams_edges$to),]  #remove isolated nodes
  bigrams_edges = bigrams_edges[!diag(sapply(bigrams_edges$from, `==`, bigrams_edges$to)), ] #remove loops
  
  if(nrow(bigrams_nodes)>max_nodes){
    bigrams_nodes=bigrams_nodes[(1:nrow(bigrams_nodes) %in% 1:max_nodes) | (bigrams_nodes$label %in% matched), ]
    bigrams_edges = bigrams_edges[(bigrams_edges$from %in% bigrams_nodes$id) &
                                    (bigrams_edges$to %in% bigrams_nodes$id), ]
    
  }
  if(nrow(bigrams_edges)>max_edges){
    bigrams_edges=bigrams_edges[(1:nrow(bigrams_edges) %in% 1:max_edges) | (bigrams_edges$from %in% matched) | (bigrams_edges$to %in% matched)|(bigrams_edges$edge_name %in% matched), ]
    bigrams_nodes = bigrams_nodes[bigrams_nodes$id %in% c(bigrams_edges$from, bigrams_edges$to,matched),]
  }
  
  #topic_bigrams_edges$weight
  
  bigrams_nodes$font.color= "black"
  bigrams_nodes$shape = "text"#dot
  bigrams_nodes$font.color[bigrams_nodes$id %in% matched]="darkred"
  bigrams_edges$color = color.gradient_2(bigrams_edges$weight)
  bigrams_nodes$title = paste(
    "Term: '",
    bigrams_nodes$label,
    "', Prob: ",
    as.character(round(bigrams_nodes$value,digits = 5)),
    sep = ""
  )
  bigrams_edges$width = (bigrams_edges$weight / max(bigrams_edges$weight)) *
    5
  Network = visNetwork::visNetwork(
    main =  list(text =paste('<b>Words association network for: ',filter_term,ifelse(filter_term2=="","", paste0(" & ",filter_term2)),"</b>",sep = ""),
                 style = "font-size:25px;text-align:center;"), 
    submain = list(text = paste('<b>(nodes & edges percentile filtering threshold  = ',filter,'</b>)<br><i>Network based on aggregated and filtered words dependency tree graphs of X bios/tweets containing the chosen term(s).<br>A total of ',nrow(dfm_temp),' documents matched the selected term(s).</i>',sep = ""),
                   style = "font-size:15px;text-align:center;"), 
    footer = paste('SoMe4Dem project - figure based on aggregated data from X - version:',Sys.Date()),
    height = "1400px",
    width = "100%",
    nodes = bigrams_nodes,
    edges =  bigrams_edges
  ) %>% visEdges(arrows = "to", smooth = T) %>%
    visNodes(scaling = list(label = list(min = 20, max = 100))) %>%
    visPhysics(solver = "forceAtlas2Based",stabilization= FALSE,
               forceAtlas2Based = list(gravitationalConstant = -50)) %>%
    visInteraction(multiselect = TRUE, navigationButtons = TRUE) %>%
    visOptions(
      manipulation = TRUE,
      nodesIdSelection = list(enabled = TRUE),
      highlightNearest = list(enabled = T, degree = 2, hover = T)
    )
  
  #%>%  visHierarchicalLayout(direction = "LR", levelSeparation = 300,edgeMinimization = F,treeSpacing = 300,blockShifting = T)
  #Network %>% 
  
  if(save==T){
    visSave(Network, paste0(filter_term,ifelse(filter_term2=="","_", paste0("_AND_",filter_term2,"_")),Sys.Date(),"_filter",filter,".html"), selfcontained = TRUE, background = "white")
  }
  Network
}

### egonet plot

egonet2(filter_term = "[query]",max_nodes = 200,max_edges = 1000,filter = 0)# filter is a percentile filter with a range from 0 to 1

