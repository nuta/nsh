install:
	cargo build --release
	cp target/release/nsh /usr/local/bin/nsh
