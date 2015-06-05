#!/bin/sh
LD_LIBRARY_PATH=../../rust/x86_64-apple-darwin/stage2/lib cargo build --release
AFL_RS_CRASH_ON_PANIC=1 afl-fuzz -i samples -o sync_dir -S fuzzer01 -- target/release/gif-afl