default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Run ghciwatch
# "cabal is better" they said, however this command is stupid and shouldn't be as long as it is
run:
    ghciwatch --command "cabal v2-repl" --watch src --watch app --after-startup-ghci ":load app/Main.hs" --test-ghci "main"    
