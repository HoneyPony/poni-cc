if ! [[ -f "$1" ]]; then
    echo 'please provide an existing file'
    exit 1
fi

fname="$(basename $1)"
fname="${fname%.c}"
out="testing-ground"

set -e

# preprocess
gcc -E -P "$1" -o "$out/$fname.i"

# compile
cargo run -p poni-cc-cli -- -i "$out/$fname.i" -o "$out/$fname.s"

# assemble & link
gcc "$out/$fname.s" -o "$out/$fname"