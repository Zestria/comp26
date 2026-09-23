#!/bin/sh
set -e

WORKSPACE="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"

usage() {
    echo "usage: dune.sh <pin|deps|build>" >&2
    echo "  Resolves the compiler package from \$COMPILER (or \$COMITTER via teams.yml)" >&2
}

get_compiler() {
    if [ -n "${COMPILER:-}" ]; then
        echo "$COMPILER"
    elif [ -n "${COMITTER:-}" ]; then
        yq --arg who "$COMITTER" -r \
            '.[] | to_entries[] | select(.value[]? == $who) | .key' \
            "$WORKSPACE/teams.yml" | head -n1
    else
        echo "error: neither COMPILER nor COMITTER is set" >&2
        exit 1
    fi
}

CMD="$1"
if [ -z "$CMD" ]; then
    usage
    exit 1
fi

COMPILER_NAME="$(get_compiler)"
if [ -z "$COMPILER_NAME" ]; then
    echo "error: could not resolve compiler (got empty name)" >&2
    exit 1
fi
echo "compiler = $COMPILER_NAME"

cd "$WORKSPACE"

case "$CMD" in
    pin)
        opam pin add "$COMPILER_NAME" . --no-action --yes
        ;;
    deps)
        opam install "$COMPILER_NAME" --deps-only --with-test --with-doc --yes
        ;;
    build)
        /usr/bin/time -f "%U" opam exec -- dune build -p "$COMPILER_NAME" @check "@$COMPILER_NAME/runtest" @install
        ;;
    *)
        usage
        exit 1
        ;;
esac