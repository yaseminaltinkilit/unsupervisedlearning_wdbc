library(readr)
wdbc <- read_csv("/Users/yaseminaltinkilit/Downloads/wdbc.data", col_names = FALSE)
wdbc <- wdbc[1:12]
colnames <- c("ID", "Diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave.points", "symmetry", "fractal.dimension" )

#"ID": Her hasta için benzersiz bir tanımlayıcıdır.
#"Diagnosis": Hastanın teşhisinin ("M" = kötü huylu veya "B" = iyi huylu) kaydedildiği sütundur.
#"radius": Tümörün çapı ile ilgili özelliktir.
#"texture": Tümörün dokusu ile ilgili özelliktir.
#"perimeter": Tümörün çevresi ile ilgili özelliktir.
#"area": Tümörün yüzölçümü ile ilgili özelliktir.
#"smoothness": Hücreler arası geçişteki farklılıkların ölçüsüdür.
#"compactness": Tümörün alanı ve çevresi arasındaki oran ile ilgili bir özelliktir.
#"concavity": Tümör yüzeyindeki oyuklukların veya çöküntülerin ölçüsüdür.
#"concave points": Tümör yüzeyindeki konveks çıkıntıların sayısıdır.
#"symmetry": Hücrelerin simetrisi ile ilgili bir özelliktir.
#"fractal dimension": Tümör yüzeyinin fraktal boyutu ile ilgilidir.

names(wdbc) <- colnames
#clustering için son hali
data <- wdbc[3:12]
head(data)

sum(is.na(data))
#eksik gözlem bulunmuyor.

summary(data)
#radius, texture, smoothness,symetry,fractal.dimension medyan ve mean değerleri birbirne yakın değerler olarak gözüküyor.
#area,compactness,concavity,concave.points ise sağa çarpık olarak gözükmektedir. Görselleştirme yöntemleriyle kontrol edilebilinir.


require(graphics)
pairs(data)

library("corrplot")
corr <- cor((data), method = "pearson")
corr
corrplot.mixed(corr, lower="pie",upper="number")

#radius ve perimeter arasında yüksek korelasyon olarak gözükmektedir.
#radius ve area arasında yüksek korelasyon olarak gözükmektedir.
#area ve perimeter arasında yüksek korelasyon olarak gözükmektedir.
#concavity ve concave.points arasında yüksek korelasyon olarak gözükmektedir.
#compactness ve concave.points arasında yüksek korelasyon olarak gözükmektedir.

library(tidyverse)

breast_cancer_gathered <- data %>%
  gather(key = "ozellik", value = "deger")

ggplot(data = breast_cancer_gathered, aes(x = ozellik, y = deger)) +
  geom_boxplot() +
  labs(x = "Özellik", y = "Değer")

#area da outlier gözüküyor varyansta çok gibi. Aynı şekilde perimeter, radius ve texture da outlier var gibi duruyor.

#####PCA####
#scale ediyoruz
data <- scale(data, center = TRUE, scale = TRUE)
summary(data)
#Değişkenlerin ölçekleri arasındaki fark çok fazla. Kümeleme algoritmalarının düzgün çalışabilmesi içinveri setinin standartlaştırılması gerekiyor.

data.pca <- prcomp(data, center = TRUE, scale. = TRUE)  ##korelasyon matrisi üzerinden
summary(data.pca)
# 1 in üzerinde olan değerler benim için önemli olduğundan 3 bileşen benim için önemli görünüyor.
#1. bileşen %0.54 ünü, 2. bileşen %0.79 unu, 3. bileşen 0.88ini ni açıklıyor olarak yorumlayabiliriz.

####SCREE PLOT#####
par(mfrow=c(1,2)) #1 satır 2 sütunda grafiği özetle demiş oluyoruz
screeplot(data.pca)
screeplot(data.pca, type='lines')
#screeplotta da 3 ten sonra anlamlı gözüküyor.

data.pca$rotation
#PCA analizinde oluşan özvektörler gösterilmektedir. Burada özvektörlerin büyüklük ve yönlerine
#bakıldığında PC3 için texture değişkeninin temsil edildiği ve radius
#değerinin ters orantılı olduğu görülmektedir.

##Değişkenler için inceleme
res.variables <- get_pca_var(data.pca)

res.variables$contrib
fviz_contrib(data.pca, choice = "var", axes = 1, top = 5)
fviz_contrib(data.pca, choice = "var", axes = 2, top = 5)
fviz_contrib(data.pca, choice = "var", axes = 3, top = 5)
fviz_pca_var(data.pca,
             col.var = "contrib",
             gradient.cols = c("lightblue", "blue", "red"),
             repel = TRUE)
#Elde ettiğimiz grafik ve çıktılara baktığımızda 1. temel bileşende concave.points, concavity,perimeter,compactnessve area
#değişkenlerinin temsil edildiği açıkça görülmektedir.
#2. temel bileşende ise fractal.dimension, smoothness,symmetry değişkeni temsil edilmektedir.
#3. temel bileşende ise sadece texture değişkeni temsil etmektedir. Tek değişkeni temsil ettiği için 2 değişkenliyle devam etme kararı aldım.
#Değişkenlerin PCA grafiği incelendiğinde pozitif korelasyon gösteren değişkenlerin aynı bölgeleri işaret ettiği söylenebilir.

res.variables$cos2
par(mfrow = c(1,1))
corrplot(res.variables$cos2, is.corr=FALSE)
#Değişkenlerin temel bileşenler tarafından ne kadar açıklandığına baktığımızda ise
#fractal.dimension 2. temel bileşende %0.86 civarı,texture 3. temel bileşende 0.86 oranında açıklandığı gözlemlenmiştir.


##Gözlemler için inceleme 
res.ind <- get_pca_ind(data.pca)

res.ind$contrib 
sum(res.ind$contrib[,1]) #99,5
fviz_contrib(data.pca, choice = "ind", axes = 1)
fviz_contrib(data.pca, choice = "ind", axes = 2)
fviz_contrib(data.pca, choice = "ind", axes = 1:2)
#Çıktıları ve grafikleri incelediğimizde herbir değişkenin temel bileşenlere olan katkısını görebiliriz


res.ind$cos2
sum(res.ind$cos2[,1:2])
fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("pink", "red", "darkred"),
             repel = TRUE)     # Avoid text overlapping
#Çıktılar ve grafikler incelenerek Her bir gözlem 2 temel bileşen ile ne kadar açıklandığı gözlemlenmiştir.
#Örneğin 123. gözlem çoğunda üçüncü kartilin üstünde değer almış. Kötü huylu tümörler bu bölgede daha çok büyümüş veri için uç değer olduğundan dataya etkisi büyük diyebiliriz.
#179. gözlem için değerleri minimuma yakın olduğundan 2 bileşenli analiz için fractaldimension değeri ifade edilemiyor diyebiliriz.


fviz_pca_biplot(data.pca, repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "blue")  # Individuals color

#Sağ tarafta bir kümeleme görünmekte, sol tarafta ise gözlemler daha ayrışmış durumda diyabiliriz.



####UZAKLIKLAR


dist_eucl <- get_dist(data, method = "euclidean")
sonuclar_eucl <- as.matrix(dist_eucl)
sonuclar_eucl[1:8,1:8]
fviz_dist(dist_eucl, order = TRUE)

#Burada istediğimiz aynı renk olanların aynı yerde toplanması ve öyle gözüküyor.

####KÜMELEME####

install.packages("clValid")

library(clValid)

datapca <- data.pca$x[,1:2]
#Ardından k-means algoritması için önerilen küme sayılarının tespiti yapılmıştır.
set.seed(123)
fviz_nbclust(datapca, kmeans, method = "wss")
#WSS yöntemine göre 4 küme sayısı için uygun gözükmektedir.
fviz_nbclust(datapca, kmeans, method = "silhouette")
#Silhoutte yöntemine göre 2 adet küme uygun görülmektedir. 3 e de bakılabilinir.
fviz_nbclust(datapca, kmeans, method = "gap_stat")
#Gap statistic metoduna göre 2 adet küme uygun görülmektedir.

install.packages("NbClust")
library("NbClust")
res.nbclust <- NbClust(datapca, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

set.seed(123)
km_res<- kmeans(data,2,nstart = 25)
print(km_res)
#2 kümelemede  clustering kısmı hangi değer hangi kümeye atanmış onu görebiliyoruz.
#(between_SS / total_SS =  38.8 %)
#Between_SS / total_SS oranının %38.8 olarak hesaplanması, farklı kümeler arasındaki farklılıkların, tüm veri setindeki değişkenliklere göre daha yüksek olduğunu gösterir. Bu durum, kümeler arasındaki benzerliklerin düşük, farklılıkların yüksek olduğunu gösterir.


#2 küme için k-means kümeleme
mod_km2 <- kmeans(datapca, 2, nstart = 25)
print(mod_km2)
fviz_cluster(mod_km2, data = datapca)
#İki küme için k-means sonuçlarına baktığımızda düzgün bir ayrışma olduğu görülmüştür
#ve wss değerleri de birbirine yakın çıkmıştır. iki küme de dengelidir.
#İki adet kümeleme ile toplam varyansın yaklaşık %45'i açıklanmıştır.
#Within cluster sum of squares by cluster:
#  [1] 1216.540 1121.768
#(between_SS / total_SS =  48.5 %)

#kmedoids için n küme sayısı belirleme metodları
fviz_nbclust(datapca, pam, method= "silhouette")
fviz_nbclust(datapca, pam, method= "wss") 
fviz_nbclust(datapca, pam, method= "gap") 

#2 küme için k-medoids kümeleme
pam_data_2 <- pam(datapca,2)
print(pam_data_2)
pam_data_2$medoids
fviz_cluster(pam_data_2,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
#İki küme için k-medoids sonuçları k-means sonuçlarına çok benzer çıkmıştır.
#swap değeri 1.700399

#3 küme için k-means kümeleme
mod_km3 <- kmeans(datapca, 3, nstart = 25)
print(mod_km3)
fviz_cluster(mod_km3, data = datapca)
#Within cluster sum of squares by cluster:
#  [1] 620.5682 455.6488 634.0446
#(between_SS / total_SS =  62.3 %)


#3 küme için k-medoids kümeleme
pam_data_3 <- pam(datapca,3)
print(pam_data_3)
pam_data_3$medoids
fviz_cluster(pam_data_3,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
#kmeans ve kmedoids birbirinden bariz farklı çıkmaktadır.
#swap değeri 1.437532

#2 kümelemede swap değeri 1.700399 iken, 3 kümelemede swap değeri 1.437532 çıkmıştır. 
#Bu durum, 3 kümeleme sonucunun daha iyi olduğunu gösterir. 
#Swap değeri düşük olduğu için, medoidlerin yerlerini değiştirmenin kümeleme kalitesini önemli ölçüde artırdığı anlaşılır.

#Bu sonuçlar, verilerin daha iyi bir şekilde 3 kümeleme ile ayrıştırılabileceğini gösterir. 
#Ancak, kümeleme işlemi için doğru sayıda küme seçmek her zaman kolay değildir. 
#Bu nedenle, küme sayısını değiştirerek elde edilen sonuçları değerlendirmek ve uygun küme sayısını belirlemek gerekebilir.

#4 küme için k-means kümeleme
mod_km4 <- kmeans(datapca, 4, nstart = 25)
print(mod_km4)
fviz_cluster(mod_km4, data = datapca)

#4 küme için k-medoids kümeleme
pam_data_4 <- pam(datapca,4)
print(pam_data_4)
pam_data_4$medoids
fviz_cluster(pam_data_4,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
#swap değeri 1.272678
#4 kümeleme için elde edilen swap değeri 1.272678, 3 kümeleme için elde edilen swap değerinden daha düşüktür. 
#Bu, 4 kümeleme sonucunun daha iyi olduğunu ve verilerin 4 küme içerisinde daha iyi ayrıştırılabildiğini gösterir.



#kümeleme yorumlaması:
#2 kümelemede:
#2 kümeleme sonucunda elde edilen Within cluster sum of squares değerleri sırasıyla 1216.540 ve 1121.768'dir. 
#Bu değerler, kümeleme sonucunda oluşturulan kümelerin içindeki verilerin benzerliğini ölçen bir metriktir. 
#Daha düşük bir değer, kümelerin daha homojen olduğunu ve verilerin kendi içinde daha benzer olduğunu gösterir.

#Ayrıca, bu değerlerin toplamı, Toplam Kareler Toplamı (total_SS) olarak adlandırılan ve veri setindeki tüm verilerin benzerliklerini ölçen bir diğer metrik ile ilişkilidir. 
#Bu durumda, Between-cluster sum of squares (between_SS), yani kümeler arasındaki benzerliklerin ölçüsü, total_SS'in %48.5'ine karşılık gelir.

#Bu sonuçlara göre, 2 kümeleme sonucunda oluşan kümelerin benzerlikleri birbirine oldukça yakındır ve veriler kümeler içinde homojen dağılmıştır. 
#Ayrıca, kümeler arasındaki fark da ortalama olarak %48.5'tir. Ancak, bu sonuçların doğru şekilde yorumlanabilmesi için, verilerin özellikleri, küme sayısı ve diğer faktörlerin de göz önünde bulundurulması gerekmektedir.

#3 kümeleme:
#3 kümeleme sonucunda, oluşan kümelerin benzerlikleri için Within cluster sum of squares değerleri sırasıyla 455.6488, 620.5682 ve 634.0446 olarak elde edilmiştir. 
#Bu değerler, kümelerin homojenliğini ölçen bir metriktir. Daha düşük bir değer, kümelerin daha homojen olduğunu ve verilerin kendi içinde daha benzer olduğunu gösterir.

#Ayrıca, bu değerlerin toplamı, Toplam Kareler Toplamı (total_SS) olarak adlandırılan ve veri setindeki tüm verilerin benzerliklerini ölçen bir diğer metrik ile ilişkilidir. 
#Bu durumda, Between-cluster sum of squares (between_SS), yani kümeler arasındaki benzerliklerin ölçüsü, total_SS'in %62.3'üne karşılık gelir.

#Bu sonuçlara göre, 3 kümeleme sonucunda oluşan kümeler arasındaki fark daha belirgin hale gelmiştir ve kümeler arasındaki benzerlikler total_SS'in %62.3'üne karşılık gelir. 
#Bu sonuçlar, verilerin kümelere daha iyi ayrıldığını ve kümelerin homojen olduğunu göstermektedir. Ancak, doğru küme sayısının belirlenmesi için diğer faktörlerin de göz önünde bulundurulması gerekmektedir.

#4 kümeleme:
#4 kümeleme sonucunda, oluşan kümelerin benzerlikleri için Within cluster sum of squares değerleri sırasıyla 221.6637, 514.1717, 257.5263 ve 366.0324 olarak elde edilmiştir. 
#Bu değerler, kümelerin homojenliğini ölçen bir metriktir. Daha düşük bir değer, kümelerin daha homojen olduğunu ve verilerin kendi içinde daha benzer olduğunu gösterir.

#Ayrıca, bu değerlerin toplamı, Toplam Kareler Toplamı (total_SS) olarak adlandırılan ve veri setindeki tüm verilerin benzerliklerini ölçen bir diğer metrik ile ilişkilidir. 
#Bu durumda, Between-cluster sum of squares (between_SS), yani kümeler arasındaki benzerliklerin ölçüsü, total_SS'in %70.1'ine karşılık gelir.

#Bu sonuçlara göre, 4 kümeleme sonucunda, oluşan kümeler arasındaki fark daha da belirgin hale gelmiştir ve kümeler arasındaki benzerlikler total_SS'in %70.1'ine karşılık gelir. 
#Bu sonuçlar, verilerin kümelere daha iyi ayrıldığını ve kümelerin homojen olduğunu göstermektedir. Ancak, doğru küme sayısının belirlenmesi için diğer faktörlerin de göz önünde bulundurulması gerekmektedir.

#Sonuç olarak;4 kümeleme için, hem k-medoids hem de k-means algoritmalarının WCSS değerleri diğerlerine göre daha düşüktür, bu nedenle 4 kümeleme tercih edilebilir. 


##CLARA
set.seed(1234)

#2li kümeleme
data_clara <- clara(datapca, 2, samples = 50, pamLike = TRUE)
print(data_clara)

dd2 <- cbind(datapca, cluster = data_clara$cluster)
head(dd2)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)


#3 lü kümeleme
data_clara <- clara(datapca, 3, samples = 50, pamLike = TRUE)
print(data_clara)

dd3 <- cbind(datapca, cluster = data_clara$cluster)
head(dd3)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)


#4 lü kümeleme
data_clara <- clara(datapca, 4, samples = 50, pamLike = TRUE)
print(data_clara)

dd <- cbind(datapca, cluster = data_clara$cluster)
head(dd)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)

#claraya baktığımızda iç içe kümeleme olduğunu gösteriyor. Bu istediğimiz bir durum değil.2 ve 3 kümelemede de aynı sonuca varıyorum.

fviz_nbclust(data, clara, method= "silhouette")

#2 kmedoids:
data_clara <- clara(datapca, 2, samples = 50, pamLike = TRUE)
print(data_clara)

dd2 <- cbind(data, cluster = data_clara$cluster)
head(dd2)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)

#3 kmedoids:
fviz_nbclust(data, clara, method= "silhouette")


data_clara <- clara(datapca, 3, samples = 50, pamLike = TRUE)
print(data_clara)

dd3 <- cbind(datapca, cluster = data_clara$cluster)
head(dd3)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)

#claraya baktığımızda iç içe kümeleme olduğunu gösteriyor. Bu istediğimiz bir durum değil.2 ve 3 kümelemede de aynı sonuca varıyorum.


#SONUÇ:

final_df <- cbind(data, cluster= pam_data_4$clustering )  
aggregate(data, by=list(cluster=wdbc$Diagnosis), mean)  
aggregate(data, by=list(cluster=pam_data_4$clustering), mean)  

wdbc$Diagnosis <- ifelse(wdbc$Diagnosis == "M", 4, 1)
table(wdbc$Diagnosis, pam_data_4$clustering)


#Sonuç, tümörün "B" veya "M" olarak sınıflandırıldığını gösterir. "B" sınıfı, tümörün iyi huylu olduğunu (yani kanser olmadığını) ve "M" sınıfı, tümörün kötü huylu olduğunu (yani kanser olduğunu) belirtir.

#Özelliklerin sayısal değerleri, tümörün özelliklerinin belirli bir ölçüde normalleştirilmiş halleridir. 
#Bu sayısal değerler, modelin hangi sınıfa tümörü sınıflandıracağına karar vermesine yardımcı olacak şekilde işlenir.


#Satırlar gerçek teşhisleri, sütunlar ise kümeleme sonuçlarını temsil eder. 
#Tabloda, her küme için hangi teşhislerin ne kadar sıklıkla düştüğünü görebiliriz. 
#Örneğin, küme 3 içindeki örneklerin çoğu (170+22=192) iyi huylu (B) olarak etiketlenmişken, küme 4 içindeki örneklerin çoğu (167+11=178) kötü huylu (M) olarak etiketlenmiştir.

#Bu tabloya dayanarak, kümeleme sonuçlarının gerçek teşhislere ne kadar iyi uymadığını değerlendirebiliriz. 
#Örneğin, küme 1 içindeki örneklerin tümü iyi huylu olarak etiketlenmişken, gerçekte birkaç kötü huylu örnek de dahil olmak üzere karışık teşhisler içeriyor olabilirler.


#küme 1 ve küme 4'ün kanserli örneklerin daha yoğun olduğu, küme 2 ve küme 3'ün ise daha sağlıklı örneklerin yoğun olduğu gruplar olduğu yorumlanabilir.



