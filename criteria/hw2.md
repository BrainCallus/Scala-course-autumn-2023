## Сразу бан (0 баллов) если:

- Сдано после дедлайна
- Красный CI
- Если CI проходит, но было выполнено 0 тестов
- Есть правки в условии
- Если есть действия после дедлайна, которые меняют код (чекать Overview)

## Логическая часть - 100

| Задание                                                                                                              | Критерий                                                                                                                                    | Баллы |
|----------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|-------|
| Фибоначчи                                                                                                            | **Максимум**                                                                                                                                | 20    |
| +                                                                                                                    | Решение использует хвостовую рекурсию                                                                                                       | 10    |
| +                                                                                                                    | Решение проходит тесты из условия                                                                                                           | 10    |
| Трансформер строк                                                                                                    | **Максимум**                                                                                                                                | 40    | 
| Дублирование                                                                                                         | **Максимум**                                                                                                                                | 5     |                        
| +                                                                                                                    | Реализована функция дублирования строки                                                                                                     | 1     |                          
| +                                                                                                                    | Есть корректный тест                                                                                                                        | 4     |                          
| Деление пополам                                                                                                      | **Максимум**                                                                                                                                | 10    |                          
| +                                                                                                                    | Реализована функция деления строки пополам, которая возвращает префикс строки длиной в половину длины строки, округленной в меньшую сторону | 2     |                         
| +                                                                                                                    | Есть тест со строкой четной длины                                                                                                           | 4     |                          
| +                                                                                                                    | Есть тест со строкой нечетной длины                                                                                                         | 4     |                          
| Разворот строки                                                                                                      | **Максимум**                                                                                                                                | 5     |                          
| +                                                                                                                    | Реализована функция разворота строки                                                                                                        | 1     |                          
| +                                                                                                                    | Есть тест, где строка не палиндром                                                                                                          | 4     |                          
| Функция трансформации строки                                                                                         | **Максимум**                                                                                                                                | 20    |                          
| +                                                                                                                    | Реализована функция трансформации строки строки                                                                                             | 5     |                              
| ИЛИ                                                                                                                  | Есть тесты с каррированием (где функция partially applied, например `transformations.map(transform("test"))`)                               | 15    |                              
| ИЛИ                                                                                                                  | Есть тесты без каррирования (где функция fully applied, например `transform("test")(reverse)`)                                              | 5     |                              
| Стоимость паркета в доме                                                                                             | **Максимум**                                                                                                                                | 40    |   
| +                                                                                                                    | Дом смоделирован через 1 `case class`, в котором есть поле для типа дома, реализованное через `sealed trait`                                | 10    |
| У `case class`'а дома есть объект-компаньон с методом `apply`                                                        | **Максимум**                                                                                                                                | 10    |                                                                                                              
| +                                                                                                                    | Есть проверки      `количество этажей` > 0, `длина` > 0, `ширина` > 0, `высота` > 0                                                         | 4     |                                                                                                              
| +                                                                                                                    | В качестве исключения смарт-конструктор бросает кастомный exception, который наследует от `Exception` или `Throwable`                       | 3     |                                                                                                              
| +                                                                                                                    | Есть тест на смарт-конструктор, где есть `assertThrows[<CustomException>]`                                                                  | 3     |                                                                                                              
| У `case class`'а дома есть метод, рассчитывающий стоимость паркета. Этот метод также может быть у объекта-компаньона | **Максимум**                                                                                                                                | 20    |                                                                                                              
| ИЛИ                                                                                                                  | Для определения типа дома используется pattern-matching с pattern-guards для ветвления по количеству этажей - 10                            | 10    |                                                                                                              
| ИЛИ                                                                                                                  | Для определения типа дома используется pattern-matching без pattern-guards, с `if` для ветвления по количеству этажей                       | 8     |                                                                                                              
| ИЛИ                                                                                                                  | Для определения типа дома используется только `if`                                                                                          | 1     |                                                                                                              
| +                                                                                                                    | Подсчет стоимости реализован правильно                                                                                                      | 1     |                                                                                                              
| +                                                                                                                    | Есть тест с подсчетом стоимости для премиум-дома с >=5 этажами                                                                              | 3     |                                                                                                              
| +                                                                                                                    | Есть тест с подсчетом стоимости для премиум-дома с < 5 этажами                                                                              | 3     |                                                                                                              
| +                                                                                                                    | Есть тест с подсчетом стоимости для дома эконом класса                                                                                      | 3     |                                                                                                              

## Стилистическая часть

Помимо баллов за логическую часть, также снимаются баллы за использование нефункциональных конструкций/методов/подходов
или несоблюдение требований к стилю.

| Запрещенный прием                                                                                                                                                                                                                                                                                                                                                                        | Наказание                                                    |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| Тест классы именуются `<ClassName>Spec`, где `<ClassName>` - класс к которому пишутся тесты.Тест классы находятся в том же пакете, что и класс к которому пишутся тесты -(например, класс `Fibonacci` находится в пакете `fibonacci` в директории `src/main/scala/fibonacci`, значит его тест класс `FibonacciSpec` должен быть в том же пакете в директории `src/test/scala/fibonacci`) | минус 5 если есть хотя бы 1 неправильный спек                |
| `var`  (используйте `for comprehension`, `@tailrec`)                                                                                                                                                                                                                                                                                                                                     | минус 20 если есть хотя бы 1 `var`                           |
| Циклы `while`  (используйте `for comprehension`, `@tailrec`)                                                                                                                                                                                                                                                                                                                             | минус 20 если есть хотя бы 1 `while`                         |
| Мутабельные структуры данных (`scala.collection.mutable.*`, `Array`, `ArrayBuffer`, коллекции из Java)                                                                                                                                                                                                                                                                                   | минус 20 если есть хотя бы 1 инстанс мутабельной коллекции   | 
| Ключевое слово `return`                                                                                                                                                                                                                                                                                                                                                                  | минус 10 если есть хотя бы 1 `return`                        |
| Использовать `asInstanseOf`/`isInstanceOf`                                                                                                                                                                                                                                                                                                                                               | минус 10 если есть хоты бы 1 использование одного из методов | 