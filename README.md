cargo tarpaulin --out html
cargo llvm-cov --html

# requires nightly

wasm-pack build --target web --manifest-path ./Cargo.toml -Z build-std=panic_abort,std -Z build-std-features=panic_immediate_abort

wasm-pack build --target web;cp index.html pkg/
python -m http.server 8080 --directory pkg
firebase emulators:start
firebase deploy
