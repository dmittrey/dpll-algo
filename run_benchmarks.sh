#!/usr/bin/env bash
# compare-with-z3.sh

set -u
# pipefail оставим, но без `-e`, чтобы не падать из-за grep/timeout
set -o pipefail

BENCHMARK_DIR="${BENCHMARK_DIR:-./benchmarks}"
OUR_CMD="${OUR_CMD:-stack exec dpll-algo}"
Z3_CMD="${Z3_CMD:-z3}"
TIMEOUT="${TIMEOUT:-10s}"

# Цвета для читаемости
GREEN="\033[32m"
RED="\033[31m"
YELLOW="\033[33m"
CYAN="\033[36m"
NC="\033[0m"

if [ ! -d "$BENCHMARK_DIR" ]; then
    echo "Папка '$BENCHMARK_DIR' не найдена"
    exit 1
fi

# Проверяем, что z3 доступен
if ! command -v "$Z3_CMD" >/dev/null 2>&1; then
    echo -e "${YELLOW}Внимание:${NC} Z3 не найден в PATH. Установите Z3 или задайте путь в переменной Z3_CMD."
    echo "Пример (Ubuntu): sudo apt-get install z3"
    echo "Пример (Mac):   brew install z3"
    exit 1
fi

# Счётчики
total=0
ok=0
mismatch=0
skipped=0

extract_verdict() {
    # Нормализуем к SAT/UNSAT/UNKNOWN
    # Читает stdin, находит первое 'sat'/'unsat'/'unknown'
    local v
    v=$(grep -E -io 'sat|unsat|unknown' | head -n1 | tr '[:lower:]' '[:upper:]' || true)
    if [ -z "$v" ]; then
        echo "UNKNOWN"
    else
        echo "$v"
    fi
}

echo -e "${CYAN}== Сравнение вашего солвера и Z3 (-dimacs) ==${NC}"
echo "DIR: $BENCHMARK_DIR"
echo "OUR: $OUR_CMD"
echo "Z3 : $Z3_CMD -dimacs"
echo "TO : $TIMEOUT"
echo

shopt -s nullglob
for file in "$BENCHMARK_DIR"/*; do
    [ -f "$file" ] || continue
    total=$((total+1))
    echo -e "${CYAN}=== $file ===${NC}"

    # Наш солвер (покажем его stdout для удобства)
    echo "- Наш солвер:"
    our_raw=$(timeout "$TIMEOUT" $OUR_CMD "$file" 2>&1 | tee /dev/fd/3 3>&1)
    # tee выше дублирует вывод в терминал; во вторую копию заберём в переменную
    our_verdict=$(printf "%s" "$our_raw" | extract_verdict)

    # Z3
    echo "- Z3:"
    z3_raw=$(timeout "$TIMEOUT" "$Z3_CMD" -dimacs "$file" 2>&1 | tee /dev/fd/3 3>&1)
    z3_verdict=$(printf "%s" "$z3_raw" | extract_verdict)

    # Сравнение
    if [ "$z3_verdict" = "UNKNOWN" ]; then
        echo -e "  Результат: ${YELLOW}SKIP${NC} (Z3 вернул UNKNOWN/нет вердикта)"
        skipped=$((skipped+1))
    elif [ "$our_verdict" = "$z3_verdict" ]; then
        echo -e "  Результат: ${GREEN}OK${NC} (оба: $our_verdict)"
        ok=$((ok+1))
    else
        echo -e "  Результат: ${RED}MISMATCH${NC} (наш: $our_verdict, Z3: $z3_verdict)"
        mismatch=$((mismatch+1))
    fi

    echo
done

echo -e "${CYAN}== ИТОГО ==${NC}"
echo "Всего файлов : $total"
echo -e "Совпадений   : ${GREEN}$ok${NC}"
echo -e "Расхождений  : ${RED}$mismatch${NC}"
echo -e "Пропущено    : ${YELLOW}$skipped${NC}"

# Возвращаем ненулевой код, если были расхождения
if [ "$mismatch" -gt 0 ]; then
    exit 2
fi
