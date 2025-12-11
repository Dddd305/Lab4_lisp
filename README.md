<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПІСКС</b></p>
<p align="center">
<b>Звіт лабораторної роботи 4</b><br/>
з дисципліни "Вступ до функціонального програмування"<br/>
Тема: "Функції вищого порядку та замикання"
</p>
<p align="right"><b>Студент:</b> КВ-23 Домущі Дмитро</p>
<p align="right"><b>Рік:</b> 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. №3);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант 8(3)
**Частина 1:** Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

**Частина 2:** Написати функцію `remove-each-rnth-reducer`, яка має один основний параметр `n` та один ключовий параметр функцію `key`. Функція має повернути функцію, яка при застосуванні в якості першого аргумента `reduce` робить наступне: при обході списку з кінця функцією `reduce`, кожен `n`-ний елемент, з числа тих, для яких функція `key` повертає значення `t` (або не `nil`), видаляється зі списку. Якщо функція `key` повертає `nil`, тоді елемент пропускається (не лічиться, але залишається).

## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-pass-hof (lst key test)
  (if (or (null lst) (null (cdr lst)))
      (values lst nil)
      (let* ((x (car lst))
             (y (cadr lst))
             (kx (funcall key x))
             (ky (funcall key y)))
        (if (funcall test kx ky)
            
            (multiple-value-bind (rest swapped)
                (bubble-pass-hof (cons x (cddr lst)) key test)
              (declare (ignore swapped))
              (values (cons y rest) t))

            (multiple-value-bind (rest swapped)
                (bubble-pass-hof (cdr lst) key test)
              (values (cons x rest) swapped))))))

(defun functional-sort-hof (lst &key (key #'identity) (test #'>))
  (multiple-value-bind (new-list swapped) 
      (bubble-pass-hof lst key test)
    (if swapped
        (functional-sort-hof new-list :key key :test test)
        new-list)))
```

### Тестові набори та утиліти першої частини
``` lisp
(defun check-function (name result expected)
  (format t "~:[FAILED~;passed~] ~a~%" (equal result expected) name))

(defun test-lab4 ()
  (format t "~%--- Part 1: Sort with HOF ---~%")
  
  (check-function "test-sort-numbers" 
                  (functional-sort-hof '(3 1 4 1 5 9 2)) 
                  '(1 1 2 3 4 5 9))
  
  (check-function "test-key-length" 
                  (functional-sort-hof '((1 2 3) (1) (1 2)) :key #'length) 
                  '((1) (1 2) (1 2 3)))
  
  (check-function "test-test-less" 
                  (functional-sort-hof '(1 2 3) :test #'<) 
                  '(3 2 1))
```

### Тестування першої частини
```lisp
CL-USER> (test-lab4)

--- Part 1: Sort with HOF ---
passed test-sort-numbers
passed test-key-length
passed test-test-less
```

## Лістинг реалізації другої частини завдання
```lisp
(defun remove-each-rnth-reducer (n &key key)
  (let ((counter 0) 
        (key-fn (or key (constantly t)))) 
    (lambda (elem acc)
      (if (funcall key-fn elem) 
          (progn
            (incf counter) 
            (if (= counter n)
                (progn 
                  (setf counter 0) 
                  acc)
                (cons elem acc)))
          (cons elem acc)))))
```

### Тестові набори та утиліти другої частини
```lisp
(defun test-lab4 ()
 (format t "~%--- Part 2: remove-each-rnth-reducer ---~%")
  
  (check-function "test-reducer-simple"
                  (reduce (remove-each-rnth-reducer 2)
                          '(1 2 3 4 5)
                          :from-end t
                          :initial-value nil)
                  '(1 3 5))

  (check-function "test-reducer-key"
                  (reduce (remove-each-rnth-reducer 2 :key #'evenp)
                          '(1 2 2 2 3 4 4 4 5)
                          :from-end t
                          :initial-value nil)
                  '(1 2 3 4 4 5)))
```

### Тестування другої частини
```lisp
CL-USER> (test-lab4)

--- Part 2: remove-each-rnth-reducer ---
passed test-reducer-simple
passed test-reducer-key
NIL
```
