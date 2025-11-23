# Artistic Swimming Coach Card Checker

This is a tool to go through an Artistic Swimming Coach Card and look
for any errors or potential errors that can be caught in advance. An
instance of this checker can be found at https://check-cards.web.app/.

# Building

Simple testing can be done via standard commands such as `cargo test`
and `cargo clippy`. To build the wasm files for the web interface,
run `cargo install wasm-bindgen-cli` and then manually install `wasm-opt`.
Do not use the version from crates.io as that version is out of date
(checked 11/22/2025).

# Testing

For the non-web bits, the code can be tested by running `cargo test`.
To check code coverage, run `cargo llvm-cov --html`.

At this time there are no automated tests for the web code. To run a
development server, build and package the code by running:

```
cargo build --release --target wasm32-unknown-unknown &&
wasm-bindgen --target web ./target/wasm32-unknown-unknown/release/check_cards.wasm --out-dir pkg &&
wasm-opt pkg/check_cards_bg.wasm -o pkg/check_cards_bg.wasm -O --enable-bulk-memory --enable-nontrapping-float-to-int &&
cp index.html pkg/ && cp text.html pkg/
```

and then run a webserver that serves `pkg/` directory. For example:

`python -m http.server 8080 --directory pkg`

or

`firebase emulators:start`

# Deployment

Currently, there is nothing in CI/CD to automatically deploy new
releases to https://check-cards.web.app/. Developers will access can
deploy by running `firebase deploy`.
