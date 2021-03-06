[TOC]

Что это такое?
==============

Приложение призвано облегчить работу с большим количеством
удалённых серверов путем предоставления следующего функционала:

- Древовидное меню для выбора серверов.
- Прозрачное использование мультиплексора терминала (screen, tmux).
- Создание нового соединения только в случае необходимости.
- Настройка фона терминала для каждого сервера.
- Создание терминалов на отдельном виртуальном рабочем столе.

Древовидное меню
----------------

При наличии нескольких уровней в древовидном меню, и с использованием
горячих клавиш, можно быстро выбрать сервер из большого набора.

Мультиплексор терминала
-----------------------

По умолчанию на каждом сервере используется мультиплексор
терминала. Благодаря этому мы получаем:
- Возможность в любой момент безопасно закрыть терминал, даже если там
  выполняется что-то продолжительное.
- Возможность подключится с другого компьютера, но продолжить работу с
  места, где мы её оставили.
- Отсутствие единой точки отказа - при перезагрузке сервера мы теряем
  состояние мультиплексора только на нём.

Очевидно, что это естественное использование мультиплексора, но многие
используют единственный экземпляр просто как средство работы с большим
количеством поключений, по одному окну на сервер. В таком случае
теряется древовидная организация и появляется единая точка отказа.

В sshmenu мультиплексор по-умолчанию всегда используется, и делается
это прозрачно, без вмешательства пользователя.

Повторное использование соединений
----------------------------------

Благодаря использования мультиплексора на каждый сервер приходится
ровно один эмулятор терминала. При выборе сервера из меню происходит
попытка обнаружения терминала по уникальному заголовку и передачи ему
фокуса. И только в случае неудачи инициируется новое соединение до
сервера.

По идее, при использовании мультиплексора нет необходимости в
реализации такого функционала, т.к. старый терминал сам отомрет из-за
отцепившегося мультиплексора. Но это будет стоить нескольких
раздражающих секунд на создание нового соединения.

Настройка фона терминала
------------------------

Возможность раскрашивать терминалы в разные цвета - очень важная
особенность. Например, можно сделать терминалы на боевые сервера
красными, а на сервера разработки - зелёными. Тогда с первого взгляда
на красный терминал будет понятно, что нужно быть более внимательным с
выполняемыми действиями.

Отдельный виртуальный рабочий стол
----------------------------------

Перед созданием нового терминала выполняется переключение на отдельный
рабочий стол. Это позволяет не замусоривать переключатель задач
огромным количеством окон.


Установка
=========
