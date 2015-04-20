# conway-life

[![Build Status](https://travis-ci.org/cmc-haskell-2015/conway-life.svg?branch=master)](https://travis-ci.org/cmc-haskell-2015/conway-life)

Игра «Жизнь» Джона Конвея.

## Установка

Для установки клонируйте репозиторий и запустите `cabal install`:

```
$ git clone https://github.com/cmc-haskell-2015/conway-life.git
$ cd conway-life
$ cabal install
```
## Конфигурационный файл

Конфигурационный файл может состоять из произвольного числа строк. В каждой
строке должно быть ровно 2 целых числа. Строки другого формата игнорируются.
Значения в первой строке задают размер поля, во всех следующих - координаты
живых клеток. Примеры конфигурационных файлов находятся в директории 
`examples`.
