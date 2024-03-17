library(tuber)
library(magrittr) 
library(purrr)
library(dplyr)
library(tidyverse)
library(promises)
library(httpuv)
library(readr)
library(readxl)
library(stringi)
library(stringr)
library(tm)
library(pander)
library(RCurl)
library(wordcloud)
library(ROAuth)
library(ggplot2)
#library(funModeling) yuklenmiyor
library(lubridate)
library(tidytext)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(formattable)
library(psych)
library(GGally)
library(rstatix)
library(sentimentr)
library(webshot)
library(htmlwidgets)
library(skimr)
library(janitor)
library(openssl)
library(writexl)
library(stopwords)
library(devtools)
library(tibble)
library(rmarkdown)
library(ggstance)
library(pastecs)
library(kableExtra)
library(citation)
library(lubridate)
library(httr)
library(xlsx)


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   ########buraya kadar dokunma
get_all_comments(video_id = "qLc6krMg40Y")    ###### video id sini yapıştır= sonrası
comments1 <- get_all_comments(video_id = "qLc6krMg40Y")   #### vid id aynısı
write_xlsx(comments1, "C:\\Uzaktanegitim\\u1.xlsx")   ####bubilgklosoru-excel ac kaydet


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   
get_all_comments(video_id = "wcFSjDtBDnQ")    
comments2 <- get_all_comments(video_id = "wcFSjDtBDnQ")   
write_xlsx(comments2, "C:\\Uzaktanegitim\\u2.xlsx")  



clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   
get_all_comments(video_id = "RaJUq213tFQ")    
comments3 <- get_all_comments(video_id = "RaJUq213tFQ")   
write_xlsx(comments3, "C:\\Uzaktanegitim\\u3.xlsx")  


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   
get_all_comments(video_id = "KInvTBW3iXY")    
comments4 <- get_all_comments(video_id = "KInvTBW3iXY")   
write_xlsx(comments4, "C:\\Uzaktanegitim\\u4.xlsx") 


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   
get_all_comments(video_id = "1peHptShHV4")   
comments5 <- get_all_comments(video_id = "1peHptShHV4")  
write_xlsx(comments5, "C:\\Uzaktanegitim\\u5.xlsx") 


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')   
get_all_comments(video_id = "sj74trijmZ4")   
comments6 <- get_all_comments(video_id = "sj74trijmZ4")   
write_xlsx(comments6, "C:\\Uzaktanegitim\\u6.xlsx")


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')
get_all_comments(video_id = "lTDIrUk4h64")
comments7 <- get_all_comments(video_id = "lTDIrUk4h64")
write_xlsx(comments7, "C:\\Uzaktanegitim\\u7.xlsx")

clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')
get_all_comments(video_id = "Lv9NuOF8XwM")
comments8 <- get_all_comments(video_id = "Lv9NuOF8XwM")
write_xlsx(comments8, "C:\\Uzaktanegitim\\u8.xlsx")


clientid <- "558777448412-gl16r6j1u25ghfans45n68s9jkkc9pco.apps.googleusercontent.com"
client_secret <- "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"
yt_oauth(clientid, client_secret, token = '')
get_all_comments(video_id = "hbdhGuE1k4A")
comments9 <- get_all_comments(video_id = "hbdhGuE1k4A")
write_xlsx(comments9, "C:\\Uzaktanegitim\\u9.xlsx")


uzaktan_egitim <- read_xlsx(file.choose())

attach(uzaktan_egitim)

textOriginal<-str_replace_all(textOriginal, "#//S+"," ")
textOriginal<-str_replace_all(textOriginal, "@//S+"," ")
textOriginal<-str_replace_all(textOriginal, "[[:punct:][:blank:]]+"," ")
textOriginal<-str_to_lower(textOriginal)
textOriginal<-str_replace_all(textOriginal, "[<].*[>]"," ")
textOriginal<-gsub ("\uFFFD", "", textOriginal, fixed = TRUE)
textOriginal<-gsub ("\n", "", textOriginal, fixed = TRUE)
textOriginal<-str_replace_all (textOriginal, "[^[:alnum:]]", " ")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

stopwords::stopwords("tr",source="stopwords-iso")

liste <- stopwords::stopwords("tr",source="stopwords-iso")

liste<- c("adamakıllı","sesimizi","geliyor","yerine","yok","çocuk","yüksek","bölümü","evet","yani","ilgili",
          "tam","olmadan","ben","bu","ya","istiyorum","varsa","teşekkür","çok","şöyle","sana","yazık",
          "beni","canım","sakarya","sizce","birde","biz","şimdi","yüzden","size","evet","simdi","icin","yaptığı",
          "gun","yok","olmuş","ama","diyorlar","aynen","keşke","yeter","falan","cok","neden","biz","yani","boş",
          "zaten","diğer","üst","yandan","ama","böyle","olan","bende","tek","ile","önemli","nolur","ama","zaten",
          "bir","abi","şeyi","fatih","kısa","biz","geri","evet","çok","yok","olanlar","acaba","bak","kişi","adam",
          "den","durumu","son","eğer","fazla","varmı","kimse","siz","peki","madem","geçen","bilen","benim","bir",
          "yok","yer","birşey","bazı","ona","hepsi","bunu","gidip","gitmek","ikinci","bu","bir","ve","ne","ama",
          "ben","yok","olsun","çok","de","da","ya","var","için","daha","mi","olmalı","bence","şimdi","çok","olmayan",
          "yok","geçtim","iptal","yani","olmalı","dedi","boşa","kararı","bilgisi","ama","ayrı","sayın","bunun","inşallah",
          "merhaba","buna","inşallah","çok","devam","gore","varken","şimdi","ise","yani","zaten","lutfen","sakarya",
          "edin","önce","anda","diyorsunuz","biz","bende","valla","bir","bunlar","biliyorum","istediğim","hayır","sizin",
          "azından","tamamen","olucak","senin","hem","veya","bizi","içinde","hayır","oldu","ilk","yine","fakat","işte",
          "şekilde","kendi","demek","yeni","tüm","olunca","güzel","şu","bişey","gerçekten","olması","niye","büyük","sanki",
          "direk","bana","kesinlikle","başka","kadar","1","mı","o","2","her","olur","her","değil","gibi","bi","nasıl",
          "biz","bence","iyi","zaten","şuan","ki","bu","zaman","gün","bu","ya","herkes","bizim","3","biraz","biri","yada",
          "onu","öyle","aynı","kesin","göre","bey","bize","artık","şey","kaç",
          "en","hiç","bide","şimdi","çünkü","olarak","lazım","abi","bile","iki","benim","sen","ayrıca","ama","hangi",
          "degil","olcak","kaç","belli","olursa","olsa","nasil","yani","diye","lütfen","diye", "anca","diyor","oluyor",
          "onlar","zayıf","sıkıntı","doğru","normal","hepimiz","olabilir","bütün","sonra","sadece","bir","ama","geldi",
          "merak","kardeşim","hemde","ediyor","gerekiyor","biz","umarım","bizde","bın","yoksa","anlamadım","abla",
          "yanlış","birinci","hatta","olmaz","bilmiyorum","zaten","nerden","soru","olduğu","fatih","ederim","türlü",
          "çoğu","ayn","cevap","lütfen","bende","yardımcı","saçma","arkadaşlar","yok","evet","bence","açık","gelir",
          "durum","yani","kötü","hayat","onun","yüzünden","ne","diyelim","olacak","çok","yoksa","bin","","yarın",
          "amma", "belki", "boşuna", "mu","illa","hiçbir")

uzaktan_egitim$textOriginal<-removeWords(uzaktan_egitim$textOriginal, liste)

veri<- data.frame(text=uzaktan_egitim$textOriginal)

kelimeler <- veri %>% mutate(linenumber = row_number()) %>% unnest_tokens(word, text)
head(kelimeler)

kelimeler %>%
  count(word, sort = TRUE) %>%
  filter(n>30) %>%
  mutate(word = reorder(word, n)) %>%
  filter(!is.na(word)) %>%
  filter(n > 0) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_minimal()+
  ggtitle("Yorumlarda En Çok Tekrar Eden Kelimelerin Frekans Analizi")

wordcloud(kelimeler$word, min.freq = 10, max.words = 180, scale = c(2,0.4), colors = brewer.pal(8, "Dark2"), 
          random.color = T, random.order = F,
          main="uzaktan_egitim")


library(plyr)
library(ggeasy)

pos.words <- c("rahat","verimli","mezun","açılacak","sağlık","okulda","final","canlı",
               "bitecek","açılır","devamsızlık","yoklama","yüz","yüze","online",
               "inşallah","sınava","ders","sınav","sorumlu","tercih","telafi","hibrit",
               "zorunluluğu","Sevindim","Güzel","İyi","Memnun","Başarı","İyimser",
               "Teşekkürler","Güldür","sınıfa","dönem","konuları","eğitimde","sınavda",
               "okumak","okuyorum","notu","üniversite","yks","dikkat","karar","gelecek","açılsa",
               "eğitim","uzaktan","okul","keşke","lgs","evde","iş","allah","sınıfta",
               "çözüm")
neg.words <- c("hasta","Sıkıldım","Endişeli","Zor","Sorun","Korku","Rahatsızlık","İsteksiz",
               "Hastalık","kapansın","özledim","internet","bilgisayar","çalışıyorum",
               "ertelenmeli","vakalar","","kapanma","sorumlu","ortalamam","zorunlu",
               "korona","açılmasın","yaz","kaldı","virüsün","erteleyin","kronik","yok",
               "dışarı","ölüm","vaka","corona","ertelensin","olmasın","yardım","düşük",
               "sorumluluk","deprem","kyk","bitti","virüs","sistem","kalma",
               "kaldım","sınavlara","zor","kaldım","korona","olmasın","zorunlu",
               "sorumluluk","kronik","erteleyin","kaldım","ölüm","deprem","corona",
               "hasta","", "yardım","psikolojimiz")

score.sentiment <- function(sentences, poz.words, neg.words, .progress = 'none') 
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
    sentence <- gsub('https://', '', sentence)
    sentence <- gsub('http://', '', sentence)
    sentence <- gsub('[^[:graph:]]', '', sentence)
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- str_replace_all(sentence, "[^[:graph:]]", " ")
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress)
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

analysis <- score.sentiment(kelimeler$word, pos.words, neg.words)

table(analysis$score)


analysis %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "lightblue") +
  ylab("Frekans") +
  xlab("Duygu Skoru") +
  ggtitle("Yorumların Duyarlılık Puanlarının Dağılımı") +
  ggeasy::easy_center_title()

neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
toplam <- positive + neutral + negative
Duygular <- c("Pozitif", "Nötr", "Negatif")


uzaktan_egitim <- c((positive / toplam) * 100, (neutral / toplam) * 100, (negative / toplam) * 100)
output <- data.frame(Duygular, uzaktan_egitim)
output$Duygular <- factor(output$Duygular, levels = Duygular)
ggplot(output, aes(x = Duygular, y = uzaktan_egitim)) +
  geom_bar(stat = "identity", aes(fill = uzaktan_egitim)) +
  ggtitle("Duygu Analizine Göre Pozitif, Negatif ve Nötr Kelimelerin Yüzde Grafiği")

head((positive / toplam) * 100)
head((neutral / toplam) * 100)
head((negative /toplam)*100)




