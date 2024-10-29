# Используем официальный образ Haskell
FROM haskell:latest

# Создаем рабочую директорию в контейнере
WORKDIR /app

# Копируем все файлы проекта в контейнер
COPY . /app

RUN cabal clean
# Сборка программы
RUN cabal build

# Запуск программы
CMD ["cabal", "run"]