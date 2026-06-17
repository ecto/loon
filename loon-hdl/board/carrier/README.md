# loon brick — FPGA carrier board

The carrier board for the **inference brick**: it hosts a Zynq UltraScale+
System-on-Module running the loon-hdl streaming accelerator, plus the NVMe
**cartridge** slot, the USB-C/PD power-and-host link, and a BLE module for the
phone tether. Designed in vcad (schematic → placement → ground plane → route →
render).

## Why a SoM carrier (not a from-scratch FPGA board)

The hard, fine-pitch parts — the FPGA BGA, the DDR4, the power tree, the config
flash — live **on the SoM**, pre-integrated and routed by the module vendor.
The carrier only fans the SoM's exposed I/O out to peripherals, so it has **no
BGA fan-out and no DDR routing** — a simple board that places and routes
cleanly, unlike a monolithic FPGA design.

## Architecture

```
 USB-C (PD ~15-20W) ──► TPS65987 PD ──► VIN ──► AP63203 ──► 3V3
        │ USB2 (D±, ESD)                                     │
        ▼                                                    ▼
   ┌─────────────────────  ZYNQ SoM  ──────────────────────────┐
   │  PS: NVMe stack + tokenizer + autoregressive orchestration │
   │  PL: loon-hdl streaming accelerator (ternary array + DMA   │
   │      + hot-expert cache)                                   │
   └───┬───────────────┬──────────────────┬───────────────┬────┘
   PCIe x1 (TX/RX±)  UART (BLE_TX/RX)   JTAG          USB2 D±
       │                 │                │
       ▼                 ▼                ▼
   M.2 M-key         nRF52840          debug header
   (NVMe cartridge)  (BLE → phone)
       ▲
   REFCLK± ── 100 MHz diff osc
```

- **PS (ARM cores)** run the host: NVMe driver, tokenizer, and the
  autoregressive loop (the orchestration we'd otherwise build as RTL).
- **PL (FPGA fabric)** runs the loon-hdl datapath (the verified streaming
  accelerator).
- The **model is a cartridge** on the M.2 NVMe; weights stream over PCIe into
  the PL's DMA + hot-expert cache. The phone talks to the brick over **BLE**;
  the weights never leave the cartridge.

## Status

- ✅ Schematic complete — 13 components, 25 nets, all connectivity resolved
  (only intentional NC pins open). See `BOM.md`.
- ✅ Clean placement (manual floorplan; zero clearance/short/courtyard).
- ✅ Ground plane poured (B.Cu); power + most signals routed.
- 🔧 Fab-prep remaining: rotate the long 2×N headers 90° (they overhang the
  board edge as placed), finish the last signal nets, route the PCIe/USB pairs
  as length-matched diff pairs (`route_diff_pair` + a controlled-impedance
  stackup), and swap the placeholder header footprints for the real connectors
  (M.2 M-key, USB-C, the SoM board-to-board connector).
- ⚠️ vcad reports a recurring `-0.708 mm` hole-to-hole on the 2.54 mm THT
  headers regardless of placement — appears to be a footprint false-positive,
  not a real collision.

## Two FPGA tiers

| | Bring-up (Stage A) | Brick (Stages B–D) |
|---|---|---|
| FPGA | Lattice ECP5-85 (open toolchain) | Zynq UltraScale+ SoM |
| NVMe | PCIe Gen1 (~250 MB/s) | PCIe Gen3+ (~3.5 GB/s) |
| Host | external MCU | on-chip ARM (NVMe + tokenizer + orchestration) |

Use an off-the-shelf ECP5 dev board to bring up the datapath; this carrier is
the production brick around a Zynq SoM.
