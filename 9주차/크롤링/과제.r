rm(list=ls())

library(rvest)
library(dplyr)
#1번 과제

url="https://search.naver.com/search.naver?sm=tab_hty.top&where=kin&query=%EC%B7%A8%EC%97%85%ED%95%98%EA%B3%A0%EC%8B%B6%EC%96%B4%EC%9A%94&oquery=%EC%B7%A8%EC%97%85%EC%9D%B4+%ED%95%98%EA%B3%A0%EC%8B%B6%EC%96%B4%EC%9A%94&tqi=TrcF6wpVuEwsstfILR0ssssst8h-316396"

question = read_html(url) %>% html_nodes('.question')%>% html_text #질문뽑기
question = gsub("   ","",gsub("질문","",question))

date=read_html(url) %>% html_nodes('.txt_inline') %>%html_text #날짜 뽑기

content=read_html(url) %>% html_nodes(xpath='//*[@id="elThumbnailResultArea"]/li/dl/dd[2]') %>% html_text

data<-data.frame("질문"=question, "질문 날짜"=date,"질문 내용"=content)

url = read_html("https://search.naver.com/search.naver?where=kin&sm=tab_jum&query=%EC%B7%A8%EC%97%85%ED%95%98%EA%B3%A0%EC%8B%B6%EC%96%B4%EC%9A%94")
question = url %>% html_nodes('dt.question') %>% html_text
date = url %>% html_nodes('dd.txt_inline') %>% html_text 
detail = url %>% html_nodes(xpath = '//*[@id="elThumbnailResultArea"]/li/dl/dd[2]') %>% html_text



###페이스북 크롤링
rm(list=ls())
library(stringr)
url="https://www.facebook.com/pg/konkukbamboo/posts/"
cont=read_html(url)
#content <- cont %>% html_nodes("._5pbx.userContent._22jv._3576") %>% html_text()

category<-html_nodes(cont, xpath = '//*[@class="_5pbx userContent _22jv _3576"]/p[1]/text()[1]') %>% html_text()

time<-html_nodes(cont, xpath = '//*[@class="_5pbx userContent _22jv _3576"]/p[1]/text()[2]') %>% html_text()

content<-html_nodes(cont, xpath = '//*[@class="_5pbx userContent _22jv _3576"]/p[2]/text()[1]') %>% html_text()


df<-data.frame("카테고리"=category,"시간"=time,"내용"=content)
