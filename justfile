default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Run ghciwatch to watch for changes and reload
# There's no reason this command should be as long as it is, cabal is dumb
run:
    ghciwatch --command "cabal v2-repl --repl-no-load" \
        --watch src --watch app  \
        --after-startup-ghci ":load app/Main.hs" \
        --test-ghci "main"
