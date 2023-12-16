all:
	@cargo build --release
	@cbindgen --config cbindgen.toml --lang c --crate tinywitch > witch.h
	@gcc witch.c ../../target/release/libtinywitch.a -Os -ffunction-sections -fdata-sections -Wl,--gc-sections -flto
	@strip a.out