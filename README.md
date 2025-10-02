# DPLL SAT Solver (Haskell / Stack)

Небольшой SAT-солвер на Haskell с запуском через **Stack** и возможностью сравнения результатов с **Z3**.

## Требования

* **Stack** (включая GHC): см. официальную установку Stack.
* *(Опционально)* **Z3** для сравнения результатов:

  * macOS: `brew install z3`
  * Ubuntu/Debian: `sudo apt-get install z3`

## Сборка

```bash
stack build
```

## Запуск

Формат входа — DIMACS CNF. Примеры лежат в `benchmarks/`.

```bash
# один файл
stack exec dpll-algo benchmarks/2sat_test_simple.cnf
```

Ожидаемый вывод: `SAT(MODEL)` или `UNSAT`.

## Массовый прогон и сравнение с Z3

Скрипт `run_benchmarks.sh` запускает солвер на всех файлах в `./benchmarks`, параллельно вызывает `z3 -dimacs` и сравнивает вердикты.

```bash
chmod +x run_benchmarks.sh
./run_benchmarks.sh
```

Переменные окружения (необязательно):

```bash
BENCHMARK_DIR=./benchmarks \
OUR_CMD="stack exec dpll-algo" \
Z3_CMD=z3 \
TIMEOUT=10s \
./run_benchmarks.sh
```

## Структура проекта

* `app/Main.hs` — точка входа (чтение DIMACS, вызов солвера)
* `src/Solver.hs` — реализация DPLL
* `benchmarks/` — тестовые CNF-кейсы (DIMACS)
* `run_benchmarks.sh` — массовый прогон и сравнение с Z3

## Примечания

* Формат DIMACS: строка заголовка `p cnf <vars> <clauses>`, далее клаузы, завершающиеся `0`.

