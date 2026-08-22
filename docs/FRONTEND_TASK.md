# Задача для субагента: frontend UI (public/js/main.js)

> **Статус: выполнена.** Текущий UI реализован в `public/js/main.js`
> (график популяций + гистограммы продолжительности жизни + кнопка
> «Обновить правила»; см. `SPEC.md`, разделы 8 и 14). Документ сохранён
> как историческая спецификация.
>
> Эта задача была оформлена для делегирования субагенту. Текущая реализация
> находится в `public/js/main.js` (см. `git log`/дату файла). Задачу можно
> использовать как спецификацию для переделки или доработки UI.

## Контекст

Zero-player симуляция «Игра 8885». Бэкенд полностью готов и работает:
сервер Snap на http://127.0.0.1:8000 (запуск: `./run.sh`), WebSocket на
ws://127.0.0.1:8000/ws. Страница уже содержит нужный DOM (генерируется
`src/MainPage.hs` — не менять):

```html
<div id="layout">
  <h1 class="title">Игра 8885</h1>
  <div id="controls">
    <label for="rule-select">Правила:</label>
    <select id="rule-select"></select>
    <button id="btn-start" class="pure-button button-ctrl">Старт</button>
    <button id="btn-step" class="pure-button button-ctrl">Шаг</button>
    <button id="btn-restart" class="pure-button button-ctrl">Рестарт</button>
  </div>
  <div id="status"></div>
  <div id="chart"></div>
</div>
```

## Протокол WebSocket (точный)

Клиент → сервер (`{"type": ...}`):
- `{"type":"init"}` — запросить состояние (сервер и сам шлёт его при подключении);
- `{"type":"start"}` / `{"type":"pause"}` — автошаги сервера (~10 шагов/сек);
- `{"type":"step"}` — один ручной шаг;
- `{"type":"restart"}` — сброс к началу текущего набора правил;
- `{"type":"select","file":"rules/example.rule"}` — выбрать набор правил.

Сервер → клиент:
1. `{"type":"hello","rules":[...],"default":"rules/8885.rule"}` — при подключении.
2. `{"type":"init","ruleSet":...,"step":n,"free":m,"finished":bool,"result":...,
    "species":[["Имя","#цвет"],...], "steps":[0..n], "series":{"Имя":[c0,c1,...]}}`
   — полное состояние + история; шлётся при подключении и после
   restart/select. Серии выровнены с "steps".
3. `{"type":"state","step":n,"free":m,"finished":bool,"result":...,"population":{"Имя":c}}`
   — после КАЖДОГО шага; добавить точку (n, c) в серию вида.
4. `{"type":"error","message":...}`.

## Требования

1. График динамики популяций: линия на вид, x = шаг, y = численность.
2. Pan и zoom по оси времени (ApexCharts: zoom.enabled, type 'x'; pan.enabled;
   тулбар с zoom/reset).
3. Длинные периоды: при > ~3000 точек — децимация для рендера (каждый k-й
   шаг), документировать подход в комментарии.
4. Управление: #btn-start переключает Старт/Пауза; #btn-step — ручной шаг;
   #btn-restart — рестарт; #rule-select → select.
5. Статус (#status): «Шаг: N | Свободно: M», при завершении —
   «Игра окончена: <result>».
6. Цвета: фон тёмный (#2b2b2b), текст светлый, линии — из init (species[i][1]),
   шрифт Victor Mono; минимализм.
7. Обработка обрыва связи: переподключение и восстановление графика.
8. Язык интерфейса — русский.

## Файлы

- `public/js/main.js` — полная замена.
- `public/css/styles.css` — только минимальные дополнения под график.
- НЕ менять src/, rules/, *.hs.

## Проверка

- `node --check public/js/main.js`;
- проверить поток сообщений hello → init → start (state…) → pause → step →
  select → restart против реального сервера.
