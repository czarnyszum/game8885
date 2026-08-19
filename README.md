# game8885

Игра в размножение в пространстве `K n` — zero-player simulation ("Игра 8885",
see `OriginalRules.md`).

## What this is

A WebSocket-driven simulation of the game 8885:

* a field of 888 slots, 5 base species (red, black, white, blue, yellow) with
  5 chibiks each at the start;
* every chibik acts once per turn — reproduce (needs a free slot) or kill;
* the DSL (`rules/*.rule`) formalizes the rules: base species, hybrids,
  environment parameters, kill probabilities, partner preferences, kill
  targets, sympathies (agreement probabilities, including conditional rules),
  creation outcomes and colors — подробное описание языка см. в
  `LANGUAGE.md`;
* the Snap server exposes a WebSocket command interface
  (`init`/`start`/`pause`/`restart`/`step`/`select`/`list`) and streams the
  population of every species after each step;
* the web UI (`public/js/main.js`) plots the population dynamics with pan and
  zoom (ApexCharts) and the control buttons.

## DSL sections (rules files)

* `Базовые виды:` — comma-separated base species.
* `Синонимы:` — hybrid definitions (`A x B ~ Имя;`).
* `Параметры:` — `Поле` (field size), `Начало` (initial chibiks per species),
  `Победа` (winning species count), `Изнасилование` (rape probability),
  `Максимум шагов` (step cap; 0 = unlimited).
* `Цвета:` — display colors.
* `Действия:` — kill probability per species (alias `Склонность к убийству:`);
  `~смешанные~` = kill only chibiks that just reproduced cross-color.
* `Партнёры:` — `Вид: [preferred] -> [fallback] [условие];` partner preference
  when initiating.
* `Убийство:` — `Вид: цели кроме исключения;` kill targets; special tokens
  `~доминирующий~`, `~смешанные~`, `*`.
* `Симпатии:` — `Инициатор < Принимающий: вероятность [условие];`
  agreement probability of the receiving side.
* `Рождение:` — `Инициатор < Партнёр -> Результат: вероятность ...;`
  creation outcomes.

Special pattern tokens: `*` (any), `~я~` (own species), `~родители~` (parents
of a hybrid), `~чистый~` (pure), `~гибрид~` (hybrid), `~доминирующий~`
(dominant species), `~смешанные~` (just reproduced cross-color).

## Build & run

The system root may be read-only, so `stack` cannot write its cache. The
repository is built with the snapshot GHC directly:

```sh
./build.sh            # build the server (out: .build/game8885)
./test.sh             # build and run the test suite
./run.sh              # run the server on http://127.0.0.1:8000
```

Or with stack in a writable environment: `stack build && stack exec game8885`.

## WebSocket protocol

Client -> server (JSON object with a `type` field): `init`, `start`, `pause`,
`restart`, `step`, `list`, `select` (with `file`). Legacy string events
`"Init"` and `"Step"` are also accepted.

Server -> client:

* `{"type":"hello","rules":[...],"default":"rules/8885.rule"}` — sent on connect;
* `{"type":"init","ruleSet":...,"step":n,"free":n,"finished":bool,
   "result":...,"species":[["Имя","#цвет"],...],
   "steps":[0..n],"series":{"Имя":[c0,c1,...],...}}` — full state and history;
* `{"type":"state","step":n,"free":n,"finished":bool,"result":...,
   "population":{"Имя":n,...}}` — broadcast after every step;
* `{"type":"error","message":...}`.

## Project layout

* `src/Species.hs` — core domain types (species, field state, environment);
* `src/Pattern.hs` — pattern language, interpreted rules, `Tables`;
* `src/Sim.hs` — the simulation engine (turns, actions, end conditions);
* `src/Decl.hs` — declarations and the compiler (rules -> `Tables`);
* `src/Parser.hs` — the DSL parser;
* `src/Cmd.hs`, `src/Ctx.hs`, `src/Server.hs`, `src/MainPage.hs` — server,
  WebSocket protocol, game context, HTML page;
* `src/Test.hs`, `src/TestMain.hs` — test suite;
* `rules/8885.rule` — the full rules of `OriginalRules.md`;
* `rules/8885A.rule` — a balanced variant (changes and rationale in
  `ChangeLog.md`);
* `tools/balance.hs` — statistics tool for tuning rule sets;
* `public/js/main.js` — the web UI.
