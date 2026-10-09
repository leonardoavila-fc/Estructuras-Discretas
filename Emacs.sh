#!/bin/bash

echo "=== Estado actual del repositorio ==="
git status
echo "====================================="

if [ -z "$(git status --porcelain)" ]; then
    echo "El directorio de trabajo está limpio. No hay cambios para hacer commit."
    exit 0
fi

read -p "Ingresa el mensaje para el commit: " commit_msg

if [ -z "$commit_msg" ]; then
    echo "Error: El mensaje de commit no puede estar vacío."
    exit 1
fi

git add . && git commit -m "$commit_msg" && git push
