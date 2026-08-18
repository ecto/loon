/* QEMU `virt` riscv64. OpenSBI runs in M-mode at 0x80000000 and hands us
   S-mode control at 0x80200000, so that is where the image must start. */
OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY {
  RAM (rwxa) : ORIGIN = 0x80200000, LENGTH = 120M
}

SECTIONS {
  .text : {
    KEEP(*(.text.entry))
    *(.text .text.*)
  } > RAM

  .rodata : { *(.rodata .rodata.*) } > RAM
  .data   : { *(.data .data.*) *(.sdata .sdata.*) } > RAM

  .bss (NOLOAD) : {
    __bss_start = .;
    *(.bss .bss.*) *(.sbss .sbss.*) *(COMMON)
    __bss_end = .;
  } > RAM

  /* Boot stack, then everything above is the heap. */
  . = ALIGN(16);
  __stack_bottom = .;
  . += 512K;
  __stack_top = .;

  . = ALIGN(4096);
  __heap_start = .;
  __heap_end = ORIGIN(RAM) + LENGTH(RAM);

  /DISCARD/ : { *(.eh_frame .eh_frame_hdr) }
}
