#!/usr/bin/env python3
"""Boot the kernel headless with a ramfb, wait for the GUI to come up, and
grab the framebuffer through QMP as a PNG.

This is how the display gets verified without a window: the same path a CI
box would use. Prints the output path on success and exits non-zero if the
GUI never reported in.
"""
import json, os, socket, struct, subprocess, sys, tempfile, time, zlib

KERNEL = "target/riscv64gc-unknown-none-elf/release/loon-kernel"
OUT = sys.argv[1] if len(sys.argv) > 1 else "screenshot.png"

tmp = tempfile.mkdtemp()
sock = os.path.join(tmp, "qmp.sock")
ppm = os.path.join(tmp, "fb.ppm")
serial = open(os.path.join(tmp, "serial.txt"), "w+b")

qemu = subprocess.Popen(
    ["qemu-system-riscv64", "-machine", "virt", "-cpu", "rv64", "-smp", "1",
     "-m", "128M", "-nographic", "-serial", "mon:stdio", "-bios", "default",
     "-device", "ramfb", "-display", "none",
     "-qmp", f"unix:{sock},server,nowait", "-kernel", KERNEL],
    stdin=subprocess.DEVNULL, stdout=serial, stderr=subprocess.STDOUT,
)

def serial_text():
    serial.seek(0)
    return serial.read().decode("utf-8", "replace")

deadline = time.time() + 120
while time.time() < deadline:
    if "gui up" in serial_text() or qemu.poll() is not None:
        break
    time.sleep(0.5)

if "gui up" not in serial_text():
    print("gui never came up. serial:\n" + serial_text(), file=sys.stderr)
    qemu.kill()
    sys.exit(1)

s = socket.socket(socket.AF_UNIX)
s.connect(sock)
f = s.makefile("rw")
f.readline()  # greeting

def cmd(o):
    f.write(json.dumps(o) + "\n"); f.flush()
    while True:
        line = json.loads(f.readline())
        if "return" in line or "error" in line:
            return line

cmd({"execute": "qmp_capabilities"})
cmd({"execute": "screendump", "arguments": {"filename": ppm}})
cmd({"execute": "quit"})
qemu.wait()

# PPM -> PNG, no dependencies.
data = open(ppm, "rb").read()
magic, dims, _maxval, px = data.split(b"\n", 3)
w, h = map(int, dims.split())
raw = b"".join(b"\x00" + px[y * w * 3:(y + 1) * w * 3] for y in range(h))
def chunk(t, b):
    return struct.pack(">I", len(b)) + t + b + struct.pack(">I", zlib.crc32(t + b) & 0xFFFFFFFF)
png = (b"\x89PNG\r\n\x1a\n"
       + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
       + chunk(b"IDAT", zlib.compress(raw, 9))
       + chunk(b"IEND", b""))
open(OUT, "wb").write(png)
print(OUT)
