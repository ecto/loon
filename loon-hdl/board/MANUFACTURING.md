# Manufacturing readiness — loon-hdl drone

Status of the whole stack against "can we actually build this." Honest: green where
verified, amber where there's a known gate.

## Chip (loon-hdl accelerator)
- 🟢 RTL emits clean, synthesizable Verilog (mac / gemm / qlinear), **co-sim bit-exact**
  vs. pure-Loon golden models (Icarus Verilog).
- 🟢 8×8 int8 GEMM tile + int4×int8 quantized-linear lane elaborate clean.
- 🟡 First silicon path = **ECP5 FPGA** (synthesize the Verilog with yosys/nextpnr → bitstream);
  custom ASIC is a later, funded step. Needs: top-level wrapper, pin constraints, timing closure.

## Frame (vcad CAD)
- 🟢 FDM Design-for-Manufacturing: **43 warnings, 0 errors** — all are downward faces that
  sit on the build plate; prints in natural orientation, supports only for pod undersides.
- 🟢 Mass ≈ 35–55 g (nylon, typical infill) — reasonable for a 26 cm quad.
- 🔴 **GATE: center of mass reads off-center (+27 mm x)** despite symmetric bounds — an
  overlapping-union artifact in the boolean. Must resolve (model arms as distinct parts or
  subtract the boom intersection) before flight; a real CoM offset = uncontrollable trim.

## Board (vcad PCB)
- 🟢 Schematic + netlist correct: 27 nets, all buses (QSPI weight, SPI-IMU, camera, motors,
  power) resolved; only the two intentional NC pins unconnected.
- 🟢 BOM resolved to real MPNs (see BOM.md); passives carry generated IPC footprints.
- 🟢 Placed on 64×64 mm (fits the drone center plate), power/QSPI partially routed.
- 🔴 **GATE: routing incomplete — 23 of 27 nets unrouted** by the single-pass autorouter on a
  dense 2-layer board around two big QFPs; 1 placement overlap (J2/C5). Until routing is
  finished and **DRC is clean**, gerber export would emit incomplete copper.

## To reach fab-release (ordered)
1. **Board:** finish routing (manual or stronger autoroute) → DRC clean → `export_gerber`
   (gerbers + drill + pick-and-place + BOM) → `build_receipt` (durable DRC proof).
2. **Frame:** fix the CoM (distinct-part arms) → re-`inspect_cad` for centered CoM →
   `export_cad` STL → slice.
3. **Chip:** wrap the RTL for ECP5, add constraints, run yosys/nextpnr → bitstream.
4. Swap U3 IMU footprint to LGA-14; pick connectors; add mounting holes to the board outline.

## What's verified vs. what's left
Verified end-to-end: the *design* (RTL↔golden bit-exact), the *manufacturability checks*
(DFM clean of errors, BOM real), and the *integration* (board carries chip+memory+sensors+power
sized to the frame). Left: two concrete gates (board routing, frame CoM) before pressing "fab".
