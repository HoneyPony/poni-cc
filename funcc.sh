set -e
cargo run -- --fun --no-cpp -o out -i "$1"
# Also generate the other code for comparison
cargo run -- -o out.s -i "$1"