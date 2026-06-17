# BOM — loon-hdl inference flight controller

Real manufacturable parts. Passives resolved via vcad's generative parts catalog
(`resolve_part`) — each carries a generated IPC footprint + 3D body + MPN xref.

| Ref | Value / Part | MPN | Package | Source |
|-----|--------------|-----|---------|--------|
| U1 | LOON-NPU accelerator | *prototype:* Lattice **LFE5U-25F-6BG256C** (ECP5) / *production:* custom ASIC | caBGA-256 (proto) / TBD | runs the emitted Verilog (mac/gemm/qlinear) |
| U2 | Flight-controller MCU | ST **STM32F405RGT6** | LQFP-64 | — |
| U3 | 6-axis IMU | TDK InvenSense **ICM-42688-P** | LGA-14 (2.5×3 mm)¹ | — |
| U4 | QSPI PSRAM (weight store) | APMemory **APS6404L-3SQR-ZR** | SOP-8 (=SOIC-8) | 8 MB, the weight-streaming memory |
| U5 | 3.3 V LDO | Diodes **AP2112K-3.3TRG1** | SOT-23-5 | 600 mA |
| C1–C4 | 100 nF X7R 16 V | Samsung **CL05B104KO5NNNC** | 0402 | resolved (IPC footprint) |
| C5 | 10 µF X5R 25 V | Samsung **CL21A106KAYNNNE** | 0805 | resolved (IPC footprint) |
| J1 | Camera header, 8-pin | generic 2.54 mm | PinHeader 1×08 | — |
| J2 | Power / ESC, 6-pin | generic 2.54 mm | PinHeader 1×06 | — |

¹ ICM-42688-P real footprint is LGA-14; the schematic used SOIC-8 as a placeholder pin-map —
swap to the LGA-14 footprint before fab.

## Notes
- The accelerator is the one non-COTS part. For a first board it's an **ECP5 FPGA**
  loaded with the emitted RTL (the loon-hdl Verilog elaborates clean); a custom ASIC is
  the production path once the design is frozen.
- QSPI PSRAM (U4) is genuinely SOIC-8 (APS6404) — the footprint used is correct.
- Decoupling: one 100 nF per IC power pin (C1–C4) + one 10 µF bulk (C5).
