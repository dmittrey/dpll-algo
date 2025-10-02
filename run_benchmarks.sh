#!/bin/bash

# Папка с CNF файлами
BENCHMARK_DIR="./benchmarks"

# Проверяем, что папка существует
if [ ! -d "$BENCHMARK_DIR" ]; then
    echo "Папка $BENCHMARK_DIR не найдена"
    exit 1
fi

# Проходим по всем файлам в папке
for file in "$BENCHMARK_DIR"/*; do
    if [ -f "$file" ]; then
        echo "=== Запуск на $file ==="
        stack exec dpll-algo "$file"
        echo ""
    fi
done
