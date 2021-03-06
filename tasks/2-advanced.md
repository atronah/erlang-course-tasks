# Блок "Обзор встроенных библиотек языка"

Данный блок содержит материалы по базовым типам контейнеров языка, рекурсии и
особенности её использования в языке, а также лямбда вычислениям.

### Материалы для изучения:

 - [Recursion](http://learnyousomeerlang.com/recursion)
 - [Lambda calculation](http://learnyousomeerlang.com/higher-order-functions)
 - [Lists](http://erlang.org/doc/man/lists.html)
 - [Proplists](http://erlang.org/doc/man/proplists.html)
 - [Binary](http://erlang.org/doc/man/binary.html)
 - [Sets](http://erlang.org/doc/man/sets.html)
 - [Dict](http://erlang.org/doc/man/dict.html)
 - [Maps](http://erlang.org/doc/man/maps.html)

### Вопросы:

 - Что такая хвостовая рекурсия, почему именно её следует использовать при
   работе на языке Erlang?
 - Что такое лямбда функция? Что такое замыкание контекса функции? Что такое
   анонимная функция?
 - Приведите примеры решаемых задач, где оправдано использование различных типов
   базовых контейнеров языка.
 - Почему необходимо использовать встроенные функции для базовых контейнеров?

### Упражнения

#### 2.0. База данных

Напишите модуль db.erl, создающий базу данных, в которой можно хранить,
записывать и удалять элементы.  Функция `destroy/1` удаляет базу данных. Сборщик
мусора выполнит всю работу за вас. Но в том случае, если база хранится в файле,
при вызове функции `destroy` вам придётся удалить этот файл. Функция `destroy`
включена для полноты интерфейса. При выполнении этого упражнения функциями из
модулей *[lists](http://erlang.org/doc/man/lists.html)*,
*[proplists](http://erlang.org/doc/man/proplists.html)*,
*[dict](http://erlang.org/doc/man/dict.html)* пользоваться нельзя. Все
рекурсивные функции должны быть написаны вами. Подсказка: используйте списки и
кортежи в качестве основного типа данных. При тестировании помните, что все
переменные могут связываться со значением только один раз.

Интерфейс:
```erlang
db:new() -> Db.
db:destroy(Db) -> ok.
db:write(Key, Element, Db) -> NewDb.
db:delete(Key, Db) -> NewDb.
db:read(Key, Db) -> {ok, Element} | {error, instance}.
db:match(Element, Db) -> [Keyl, ..., KeyN].
```

Пример использования в интерпретаторе:
```erlang
1> c(db).
{ok,db}
2> Db = db:new().
[]
3> Dbl = db:write(francesco, london, Db).
[{francesco,london}]
4> Db2 = db:write(lelle, 'Stockholm', Dbl).
[{lelle,'Stockholm'},{francesco,london}]
5> db:read(francesco, Db2).
{ok,london}
6> Db3 = db:write(joern, 'Stockholm', Db2).
[{joern,'Stockholm'}, {lelle,'Stockholm'}, {francesco,london}]
7> db:read(ola, Db3).
{error,instance}
8> db:match('Stockholm', Db3).
[joern,lelle]
9> Db4 = db:delete(lelle, Db3).
[{joern,'Stockholm'}, {francesco,london}]
10> db:match('Stockholm', Db4).
[joern]
```

### 2.1 Новая функциональность в базе данных

Реализуйте следующие возможности в нашей базе данных.

Интерфейс:
```erlang
Parameters = [Opt | Parameters].
Opt = {append, allow|deny} | {batch, Number :: non_neg_integer()}.
KeyList = [Key1,...KeyN].

db:new(Parameters) -> Db.
db:append(Key, Element, Db) -> NewDb.
db:batch_delete(KeyList, Db) -> NewDb | {error, batch_limit}.
db:batch_read(KeyList, Db) -> [{Key, Element}] | {error, instance} | {error, batch_limit}.
```

### 2.2 Сделайте так, чтобы база данных работала с JSON объектами, реализованными ранее в п.1.8

### 2.3 Lambda-вычисления

Реализуйте следующие функции, используя рекурсию:
* Напишите lambda-функцию, которая осуществляет произвольную операцию
  ``Operation(A, B) -> C`` (где ``A, B, C`` - числа), над двумя числовыми
  списками попарно, возвращая список результатов операции также в виде списка.
  Проверьте вашу функцию на разных операциях (``erlang:'+'``, ``erlang:'xor'``,
  ``erlang:'rem'``, ``erlang:'/'`` и собственной фунции, которая возвращает
  среднее гармоническое двух чисел ``H = 2/(1/A + 1/B)``).
* Напишите lambda-функцию, которая для каждой точки точки из списка ``dotsA``
  вычисляет расстояние до всех точек из списка точек ``dotsB`` в пространстве
  размерности N.  Напишите функцию, которая читает следующую нотацию:

```erlang
[
    {dimension, 5},
    {dotsA, [{1, 2, 3, 4 5}, {7, 8, 9, 10, 11}]},
    {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
]
```

и возвращает:
[ 5.360220495669696, 10.720440991339393, 12.988650063170537, 18.14700750425752 ]

### 2.4 Библиотечные функции

Реализуйте следующие те же самые lambda-функции, реализованные в п.2.3,
используя библиотечные функции: ``foldl``, ``map``

Дополнительно:
* Реализуйте собственную функцию `my_lists:filtermap` через `lists:foldl`.
  Синтаксис `my_lists:filtermap` должен совпадать с синтаксисом
  `lists:filtermap`

### 2.5 Ленивые вычисления

Концепция ленивых вычислений достаточно проста - не выполнять вычисления до тех
пор, пока они не понадобятся. Для задержания вычислений можно пользоваться
lambda-функциями. Пример простого задержанного вычисления:

```erlang
Delayed = fun() ->
    2 * 3.14
end.
```

В приведенном примере создается обертка над выражением `2 * 3.14`. Это пример
простейшего задержанного вычисления. В момент, когда нам понадобится его
результат, мы просто делаем вызов `Delayed()`. Другой пример ленивых вычислений
\- ленивые списки. Их еще называют потоками. Чтобы понять их главное отличие,
давайте рассмотрим обычный список, содержащий числа от 1 до  1 000 000. Такой
список занимает достаточно много памяти, и оперировать таким списком становится
затратно. Например, если мы хотим выделить из этого списка список всех нечетных
чисел, а затем отфильтровать все числа, которые меньше заданного значения, то мы
на каждом этапе обработки будем создавать большие списки: сперва список всех
нечетных чисел, затем список всех числе, больше или равных заданного порога.
Такой подход не очень эффективен. Гораздо эффективнее было бы вычислять голову
списка и откладывать вычисление хвоста списка на потом, до тех пор, пока не
понадобится следующий элемент (то есть голова хвоста списка). И даже в этом
случае хвост не будет вычисляться полностью - вычислится голова, а все остальное
вычисление "заморозится". Абстрактный пример:

```erlang
LazyList = [1|fun() -> [2|fun() -> ... end] end]
```

 Этот пример просто показывает организацию вычислений. Теперь давайте
 рассмотрим, как можно построить такой список на самом деле:

```erlang
lazy_list(Begin, End, Step) when Begin =< End, Step > 0 ->
   fun() ->
       [Begin|lazy_list(Begin + Step, End, Step)]
   end;
lazy_list(_, _, _) ->
   fun() ->
        []
   end.
```

Как с таким списком работать? Достаточно просто:
```erlang
LL = lazy_list(1, 1000000, 1),
[Head|Tail] = LL().
```

В данном примере мы "вынуждаем" вычисление, сохраненное в LL, и получаем голову
и хвост списка. Чтобы вычислить следующий элемент списка, нам нужно "вынудить"
вычисление, связанное с Tail.

#### 2.5.1 I'm lazy...

 - Реализуйте ленивые версии функций map, filter, foldl. Назовите эти функции
   `lazy_map`, `lazy_foldl`, `lazy_filter`. Покажите, что в общем случае
   правосторонняя свертка невозможна для ленивых списков. Получится ли выразить
   функции `lazy_map` и `lazy_filter` через `laxy_foldl`? Почему?
 - Реализуйте конкатенацию ленивых списков. В чем особая польза этой функции?
 - Реализуйте ленивое чтение из файла. Воспользуйтесь функциями `file:open/2`,
   `file:read/2`. Ленивый поток является абстракцией последовательности строк -
   то есть каждый вызов отложенного вычисления выдает следующую строку файла.
 - В файле лежит последовательность чисел (для простоты предположим, что одна
   строка - это одно число). Необходимо вычислить максимальную сумму непрерывной
   подпоследовательности данных чисел. Файл должен обрабатываться с помощью
   ленивых списков. Если исходная последовательность пустая, то считаем, что
   сумма равна нулю. Рассмотрим несколько примеров:
    - если последовательность чисел полностью состоит из положительных чисел, то
      такая сумма является суммой всех чисел в последовательности.
    - если последовательность чисел полностью состоит из отрицательных чисел, то
      максимальная сумма подпоследовательности равна нулю, так как в данном
      случае подпоследовательность является пустой.
  Нужно найти такую непрерывную подпоследовательность исходной
  последовательности, сумма которой будет максимальна.
 - То же, что и предыдущий пункт, но с дополнительными условями:
   - необходимо возвращать саму подпоследовательность;
   - добавить ограничение: минимальная длина подпоследовательности должна быть
     K.
