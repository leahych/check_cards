# Artistic Swimming Coach Card Checker

This is a tool to go through an Artistic Swimming Coach Card and look
for any errors or potential errors that can be caught in advance. An
instance of this checker can be found at https://check-cards.web.app/.

# Testing

For the non-web bits, the code can be tested by running `cargo test`.
To check code coverage, run `cargo llvm-cov --html`.

At this time there are no automated tests for the web code. To run a
development server, build and package the code by running:
`wasm-pack build --target web;cp index.html pkg/; cp text.html pkg/`

and then run a webserver that serves `pkg/` directory. For example:

`python -m http.server 8080 --directory pkg`

or

`python -m http.server 8080 --directory pkg`

# Deployment

Currently, there is nothing in CI/CD to automatically deploy new
releases to https://check-cards.web.app/. Developers will access can
deploy by running `firebase deploy`.
