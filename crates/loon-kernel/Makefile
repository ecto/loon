# Loon unikernel. Requires qemu-system-riscv64 and the riscv64gc target:
#   brew install qemu
#   rustup target add riscv64gc-unknown-none-elf

KERNEL := target/riscv64gc-unknown-none-elf/release/loon-kernel
QEMU   := qemu-system-riscv64
QFLAGS := -machine virt -cpu rv64 -smp 1 -m 128M -nographic -bios default

.PHONY: build run check clean

build:
	cargo build --release

# Boot the machine. It powers itself off when init returns.
run: build
	$(QEMU) $(QFLAGS) -serial mon:stdio -kernel $(KERNEL)

# The same program on the host, for comparison. Run from the workspace root:
# this directory's .cargo/config.toml pins a bare-metal target that the host
# build must not inherit.
host:
	cd ../.. && cargo run -q -p loon-cli -- run crates/loon-kernel/boot/init.oo

# Boot, and prove the machine agrees with the host byte for byte — for init,
# and for the fractal (which is the float path's parity check in disguise).
check: build
	@$(QEMU) $(QFLAGS) -serial mon:stdio -kernel $(KERNEL) < /dev/null 2>/dev/null \
	  | tr -d '\r' > /tmp/loon-metal-all.txt
	@sed -n '/^hello from loon/,/^init done/p' /tmp/loon-metal-all.txt > /tmp/loon-metal.txt
	@sed -n '/^@@@@@@@@@@@@%/,/^@@@@@@@@@@@%%%%%%%%####/p' /tmp/loon-metal-all.txt > /tmp/loon-metal-mandel.txt
	@cd ../.. && cargo run -q -p loon-cli -- run crates/loon-kernel/boot/init.oo 2>/dev/null > /tmp/loon-host.txt
	@cd ../.. && cargo run -q -p loon-cli -- run crates/loon-kernel/boot/mandel.oo 2>/dev/null > /tmp/loon-host-mandel.txt
	@diff /tmp/loon-host.txt /tmp/loon-metal.txt \
	  && diff /tmp/loon-host-mandel.txt /tmp/loon-metal-mandel.txt \
	  && echo "ok: identical output on the host and on bare metal"

clean:
	cargo clean
