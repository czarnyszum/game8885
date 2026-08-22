# game8885

Игра в размножение в пространстве `K n` — zero-player simulation ("Игра 8885",
see `OriginalRules.md`).

## What this is

A WebSocket-driven simulation of the game 8885:

* a field of 888 slots, 5 base species (red, black, white, blue, yellow) with
  5 chibiks each at the start;
* every chibik acts once per turn — reproduce (needs a free slot) or kill;
* every chibik has a **finite lifespan**: it carries an age and a probability
  of dying at the end of each turn; the probability grows with age (aging
  law, per species), drops after a successful kill (reward) and rises after a
  successful reproduction (penalty); lifespans are tracked and displayed as
  per-species histograms in the UI (see `SIMS.md` for the math);
* the DSL (`rules/*.rule`) formalizes the rules: base species, hybrids,
  environment parameters, kill probabilities, kill success, offspring
  distributions, partner preferences, kill targets, sympathies (agreement
  probabilities, including conditional rules), creation outcomes, colors and
  lifespan parameters — подробное описание языка см. в `LANGUAGE.md`;
* the Snap server exposes a WebSocket command interface
  (`init`/`start`/`pause`/`restart`/`step`/`update`/`select`/`list`) and
  streams the population of every species and the lifespan histograms after
  each step;
* the web UI (`public/js/main.js`) plots the population dynamics with pan and
  zoom (ApexCharts) plus the lifespan histograms, and the control buttons.

## DSL sections (rules files)

* `Базовые виды:` — comma-separated base species.
* `Синонимы:` — hybrid definitions (`A x B ~ Имя;`).
* `Параметры:` — `Поле` (field size), `Начало` (initial chibiks per species),
  `Победа` (winning species count), `Изнасилование` (rape probability),
  `Максимум шагов` (step cap; 0 = unlimited), `Награда за убийство` (kill
  reward: death-probability drop of the killer), `Штраф за размножение`
  (reproduction penalty: death-probability rise of the parent).
* `Цвета:` — display colors.
* `Действия:` — kill probability per species (alias `Склонность к убийству:`);
  `~смешанные~` = kill only chibiks that just reproduced cross-color.
* `Смертность:` — starting death probability at birth, per species.
* `Долголетие:` — aging scale L per species (death probability rises by 1/L
  per turn; expected free-chibik lifespan ≈ sqrt(πL/2)).
* `Успех убийства:` — probability that a kill attempt succeeds, per species.
* `Потомство:` — offspring-count distribution per species
  (`Вид: 0: 20% 1: 60% 2: 20%;`).
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
`restart`, `step`, `update` (re-read the current rule file from disk and
restart the game with it), `list`, `select` (with `file`). Legacy string
events `"Init"` and `"Step"` are also accepted.

The server remembers the last rule set chosen via `select` in the persistent
config file `.game8885.conf` (JSON, one `lastRule` field; created in the
working directory and re-read on the next server start, so the game resumes
with the same rule set).

Server -> client:

* `{"type":"hello","rules":[...],"default":"rules/8885.rule"}` — sent on connect;
* `{"type":"init","ruleSet":...,"step":n,"free":n,"finished":bool,
   "result":...,"species":[["Имя","#цвет"],...],
   "steps":[0..n],"series":{"Имя":[c0,c1,...],...},
   "lifespans":{"Имя":[h0,h1,...],...}}` — full state and history
   (`lifespans`: age-at-death histogram per species, refreshed every step);
* `{"type":"state","step":n,"free":n,"finished":bool,"result":...,
   "population":{"Имя":n,...},"lifespans":{"Имя":[h0,h1,...],...}}` —
   broadcast after every step;
* `{"type":"error","message":...}`.

## Project layout

* `src/Species.hs` — core domain types (species, field state with per-chibik
  life state, environment);
* `src/Pattern.hs` — pattern language, interpreted rules, `Tables`;
* `src/Sim.hs` — the simulation engine (turns, actions, aging/death, end
  conditions, lifespan histograms);
* `src/Decl.hs` — declarations and the compiler (rules -> `Tables`);
* `src/Parser.hs` — the DSL parser;
* `src/Cmd.hs`, `src/Ctx.hs`, `src/Server.hs`, `src/MainPage.hs` — server,
  WebSocket protocol, game context, HTML page;
* `src/Test.hs`, `src/TestMain.hs` — test suite;
* `rules/8885.rule` — the full rules of `OriginalRules.md`;
* `rules/8885A.rule` — a balanced variant (changes and rationale in
  `ChangeLog.md`);
* `rules/triplet.rule` — minimal model (2 base species + 1 hybrid), used for
  testing the finite-lifespan mechanics;
* `tools/balance.hs` — statistics tool for tuning rule sets;
* `SPEC.md` — точная спецификация семантики ядра (источник истины для
  проверки кода; раздел 17 — тонкие места механики);
* `SIMS.md` — mathematical analysis and simulations of the aging law;
* `public/js/main.js` — the web UI.
