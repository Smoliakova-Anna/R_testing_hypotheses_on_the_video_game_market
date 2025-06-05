#Импортируем наш файл с данными
install.packages("readxl")
library(readxl)

df <- read_excel("DataSet.xlsx")
#Первично посмотрим на данные
View(df)
#Дескриптивный анализ:
#Посмотрим на уникальные значения по перемнным
lapply(df, unique)
#Посмотрим на кол-во уникальных значений по переменным
sapply(df, function(x) length(unique(x)))
#Посмотрим на структуру данных
str(df)
#На описательные статистики
summary(df)

colSums(is.na(df))

#Иссл: что влияет на уровень глобальных продаж?
#Первая гипотеза: Зависит ли уровень глобальных продаж игры от её жанра?
#H₀ (нулевая): Медианы глобальных продаж не отличаются между жанрами.
#H₁ (альтернативная): Медианы глобальных продаж различаются между жанрами.(хотя бы в одной группе)

#То есть тут мы будем работать с 2 перемнными: Genre и Global_Sales, уже выяснили, что у нет них нуль значений, посмотрим, что делать с выбросами (их очень много)
# Базовый боксплот по Global_Sales и Genre
ggplot(df, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Global Sales by Genre (raw data)", x = "Genre", y = "Global Sales") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
#Посмотрим, как выглядит распределение (с логарифмированием) — чтобы оценить выбросы лучше
ggplot(df, aes(x = Genre, y = log(Global_Sales + 1), fill = Genre)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Log-transformed Global Sales by Genre", x = "Genre", y = "log(Global_Sales + 1)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
#Выбросы — не всегда плохо. Если они реальны, их стоит оставить, особенно если они имеют смысл
#Однако было принято решение - Сделать логарифмирование переменной Global_Sales для дальнейшего анализа.
#Использовать непараметрические тесты 
#Удалять выбросы не будем - они оправданы и объяснимы

#- (разделитель)

#Проверим глобальные продажи на нормальность
library(nortest)
ad.test(df$Global_Sales[!is.na(df$Global_Sales)])  # Anderson-Darling test
#Критерий Андерсона-Дарлинга. 1 Придаёт больше веса хвостам распределения, более чувствителен к выбросам и аномальным значениям, лучше обнаруживает отклонения в хвостах распределения. 1
# не используем Колмогорова, так как у нас много повт значений более устойчив к повторяющимся значениям.
#Колмогоров из-за этого выдает ошибку

#Нулевая гипотеза (H₀): данные имеют нормальное распределение.
#Альтернативная гипотеза (H₁): данные не имеют нормального распределения.
#Так как p-value << 0.05, мы отвергаем H₀:
#Global_Sales не распределены нормально.

#Визуализируем ненормальность: 
qqnorm(df$Global_Sales[!is.na(df$Global_Sales)], main = "Q-Q график для Global_Sales")
qqline(df$Global_Sales[!is.na(df$Global_Sales)], col = "red", lwd = 2)


#Нас смутила линия, лежащая внизу плашмя: если данные имеют сильно скошенное распределение, например, много маленьких значений или длинный хвост, то Q-Q график и линия могут выглядеть странно — линия может быть почти горизонтальной или «лежать плашмя».
#Логарифмирование часто помогает сделать распределение более симметричным и лучше подходит для визуализации нормальности.
x <- df$Global_Sales[!is.na(df$Global_Sales)]
x_log <- log10(x + 1)  # +1 чтобы избежать логарифма от нуля, логарифмирование со сдвигом

qqnorm(x_log, main = "Q-Q график для логарифмированных Global_Sales")
qqline(x_log, col = "red", lwd = 2)
#Возможно, график стал более интуитивно понятным, но к нормальному разспределение привести не удалось (логарифмированием обычным в том числе)

#Посмотрим на распределение глабальных продаж: 
library(ggplot2)
library(scales)  # для функции alpha()

df_log <- df[df$Global_Sales > 0, ]
df_log$log_Global_Sales <- log10(df_log$Global_Sales)

ggplot(df_log, aes(x = log_Global_Sales)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Log10(Global_Sales)", x = "log10(Global_Sales)", y = "Frequency") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = alpha("gray", 0.7)),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank()
  )
#Посмотрим на переменные актуальные для исследования: 
library(ggplot2)

# Барплот по жанрам с палитрой Set3
ggplot(df, aes(x = Genre, fill = Genre)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Количество по жанрам", x = "Жанр", y = "Количество ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Барплот по платформам (кол-во игр на каждой платформе)
ggplot(df, aes(x = Platform, fill = Platform)) +
  geom_bar() +
  labs(title = "Количество по платформам", x = "Платформа", y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Гистограмма по годам выпуска
# Сначала преобразуем Year в числовой, если еще не сделано
df$Year <- as.numeric(df$Year)

ggplot(df, aes(x = Year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Распределение по годам выпуска", x = "Год выпуска", y = "Количество") +
  theme_minimal()


#- (разделитель)
#Проверки гипотез:
#Т.к. переменная Global_Sales — непрерывная, а Genre — категориальная, и жанров >2,
#мы будем использовать тест Крускала–Уоллиса (если данные не нормально распределены).
#H₀ (нулевая): Медианы глобальных продаж не отличаются между жанрами.
#H₁ (альтернативная): Медианы глобальных продаж различаются между жанрами.
kruskal.test(Global_Sales ~ Genre, data = df) #Тест Крускала–Уоллиса и последующий тест Данна — это непараметрические тесты.
#Они работают с рангами, а не с исходными значениями, поэтому:
#Логарифмировать данные не обязательно перед ними

#Так как p-значение значительно меньше стандартного уровня значимости (например, 0.05), мы отвергаем нулевую гипотезу. Это означает, что есть веские статистические доказательства того, что медианы глобальных продаж различаются между жанрами.
library(ggplot2)

ggplot(df, aes(x = Genre, y =  Global_Sales, fill = Genre)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Global_Sales by Genre",
    x = "Genre",
    y = "Global_Sales"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#Отвергаем нулевую гипотезу о том, что медианы всех групп равны.
#График не слишком инфомативен, добавим логарифмирование со сдвигом
library(ggplot2)

ggplot(df, aes(x = Genre, y = log(Global_Sales + 1), fill = Genre)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Log-transformed Global Sales by Genre",
    x = "Genre",
    y = "log(Global_Sales + 1)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Отвергаем нулевую гипотезу о равенстве медиан продаж по жанрам.

#- (разделитель)

#Чтобы узнать, какие именно жанры различаются между собой по медианам глобальных продаж, обычно делают постхоковый анализ после теста Крускала-Уоллиса.
#Mожно использовать тест Данна (Dunn’s test) с поправкой Бонферрони или Холма, чтобы контролировать ошибку первого рода.
install.packages("dunn.test")

library(dunn.test)

dunn.test(df$Global_Sales, df$Genre, method = "holm")
#Action vs Adventure - Разность: 22.56, p = 0.0000* → Action и Adventure сильно различаются по глобальным продажам.
#Самые "особенные" жанры (с наибольшим числом значимых отличий): Strategy, Shooter, Adventure, Puzzle.
#Этот результат подтверждает, что твой вывод из Kruskal-Wallis обоснован: жанры действительно различаются между собой по продажам.
#Построим графики по особенным жанрам:
library(ggplot2)
library(dplyr)

# Фильтрация только по нужным жанрам
install.packages("dplyr")
library(dplyr)
selected_genres <- c("Strategy", "Shooter", "Adventure", "Puzzle")
df_filtered <- df %>% filter(Genre %in% selected_genres)
# Добавим лог-переменную
df_filtered$Log_Global_Sales <- log1p(df_filtered$Global_Sales)

#Боксплот (Boxplot) — логарифмированные продажи
ggplot(df_filtered, aes(x = Genre, y = Log_Global_Sales, fill = Genre)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Boxplot: Log(Global Sales + 1) by Genre",
    x = "Genre",
    y = "Log(1 + Global Sales)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Вайолин-плот (Violin plot) — логарифмированные продажи
ggplot(df_filtered, aes(x = Genre, y = Log_Global_Sales, fill = Genre)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Violin Plot: Log(Global Sales + 1) by Genre",
    x = "Genre",
    y = "Log(1 + Global Sales)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#- (разделитель)
#Есть ли различие в глобальных продажах между платформами?
#Global_Sales ~ Platform → Kruskal-Wallis

#Нулевая гипотеза (H₀): распределения Global_Sales одинаковы во всех группах платформ.
#Альтернативная гипотеза (H₁): хотя бы в одной платформе распределение продаж отличается.
kruskal.test(Global_Sales ~ Platform, data = df) #Тест Крускала–Уоллиса
#Так как p-value < 0.05, мы отвергаем H₀:
#Продажи (Global_Sales) статистически значимо различаются между платформами.

library(ggplot2)

ggplot(df, aes(x = Platform, y = log(Global_Sales + 1), fill = Platform)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Log-transformed Global Sales by Platform",
    x = "Platform",
    y = "log(Global_Sales + 1)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Краскал-Уоллис (KW) показал значимое различие в Global_Sales между платформами (p < 2.2e-16) — это значит, что где-то есть различия, но не говорит, где именно.
#Манна-Уитни используется как пост-хок тест: попарно.
#Нашли информацию в открытых источниках о спорах между plastation и xbox, однако мы хотим посмотреть есть ли стат значимые различия в медианах продаж, тк если споры есть логично предположить, что продажи схожи - каждый просто выбирает свое, но если кого-то покупают больше - тут уже не совсем равный рынок и тд
#Для этого объединим все по  plastation и xbox в 2 группы и проведем тест Манна-Уитни
# Создание групп
df$Platform_Group <- NA
df$Platform_Group[df$Platform %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")] <- "PlayStation"
df$Platform_Group[df$Platform %in% c("XB", "X360", "XOne")] <- "Xbox"

# Убираем NA и отбираем только нужные данные
df_grouped <- subset(df, Platform_Group %in% c("PlayStation", "Xbox") & !is.na(Global_Sales))

# Манна-Уитни
#H₀ (нулевая): Медианы глобальных продаж равны в двух группах платформ.

#H₁ (альтернативная): Медианы глобальных продаж отличаются между двумя группами платформ.
#Поскольку p-value значительно меньше обычного уровня значимости 0.05, отвергаем нулевую гипотезу.
#Это означает, что есть статистически значимое различие в медианах глобальных продаж между двумя группами платформ

wilcox.test(Global_Sales ~ Platform_Group, data = df_grouped)

# Boxplot
boxplot(Global_Sales ~ Platform_Group, data = df_grouped,
        col = c("steelblue", "orange"),
        ylab = "Global Sales (millions)",
        main = "Global Sales: PlayStation vs Xbox")
#Тест не требует логарифмирования, но для визуализации (более показательной) можно сделать


df_grouped$log_sales <- log1p(df_grouped$Global_Sales)

boxplot(log_sales ~ Platform_Group, data = df_grouped,
        col = c("steelblue", "orange"),
        ylab = "log(Global Sales + 1)",
        main = "Log-Scaled Global Sales: PlayStation vs Xbox")


#Посчитаем медианы для каждой группы, чтобы оценить эффект: 
library(dplyr)

df %>%
  group_by(Platform_Group) %>%
  summarise(
    median_sales = median(Global_Sales, na.rm = TRUE),
    mean_sales = mean(Global_Sales, na.rm = TRUE),
    count = n()
  )
#Xbox имеет наибольшую медиану (0.21) и среднее значение (0.599) и по тесту Манна-Уитни это стат значимо

#- (разделитель)

# Хотим изучить связь между годом выпуска игры (Year) и глобальными продажами (Global_Sales)
#Выбираем Спирмена:
#1.Спирмен — это непараметрический коэффициент корреляции рангов, он хорошо подходит, если данные не нормальные или есть выбросы.
#2.Год — порядковая/числовая переменная.
#3.Продажи — часто скошенные, поэтому Спирмен устойчивее, чем Пирсон.

#переменная Year у нас типа chr (строка), это проблема для корреляции, потому что функции для корреляции ждут числовые данные.Переведем в num 
#Также есть пропущенные строки в Year
df$Year <- as.numeric(df$Year)
df <- df[!is.na(df$Year), ]

cor.test(df$Year, df$Global_Sales, method = "spearman", exact = FALSE, use = "complete.obs")
#rho = -0.15 — слабая отрицательная корреляция.
#Это значит, что с увеличением года выпуска игры (т.е. более новые игры) немного снижаются глобальные продажи.

#p-value < 2.2e-16 — очень сильная статистическая значимость, то есть эта слабая отрицательная связь — не случайна.
#Возможно, с течением времени средние продажи игр падают (например, из-за большого количества новых релизов, конкуренции, изменения рынка).

#Связь слабая, поэтому на продажи влияют и другие факторы.
#А какие факторы? Смотрим матрицу корреляций по Спирмену (тк не нормальное распределение)

#Выделим числовые переменные:
num_df <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(num_df, method = "spearman", use = "complete.obs")
print(cor_matrix)


library(reshape2)
library(ggplot2)

# "растопить" матрицу
melted_cor <- melt(cor_matrix)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#Видим сильную отрициательную корреляцию с Rank (c продажами по регионам нам не интересно - не в рамках исследовательского вопроса)
#Но Rank нет смысла проверять, так как это рейтинг и он непосредственно включает в себя показатель по глобальным продажам - заранее известно, что сильная корреляция

#the end 