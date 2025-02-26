export RUSTFLAGS := "-Awarnings"

lint:
    cargo fmt --all -- --check

    cargo clippy -- \
        -D warnings \
        -D trivial_casts \
        -D trivial_numeric_casts \
        -D unused_extern_crates \
        -D unused_import_braces \
        -D unused_qualifications \
        -D clippy::all \
        -D clippy::correctness \
        -D clippy::suspicious \
        -D clippy::complexity \
        -D clippy::perf \
        -D clippy::style

test:
    cargo test

clean:
    cargo clean
