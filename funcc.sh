set -e
cargo run --quiet -- --fun --no-cpp -o out -i "$1"
# Also generate the other code for comparison
./target/debug/poni-cc-cli -o out.s -i "$1"