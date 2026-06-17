# BOM — loon brick FPGA carrier

| Ref | Part | Function | Footprint |
|-----|------|----------|-----------|
| J1 | **Zynq UltraScale+ SoM** (Kria K26 / Trenz / Enclustra) | FPGA + DDR4 + power + config, pre-integrated; runs PS host + PL accelerator | board-to-board (placeholder: 2×10 header) |
| J2 | **M.2 M-key** connector | NVMe cartridge slot (the model) | M.2 (placeholder: 1×12 header) |
| J3 | **USB-C** receptacle | power-in (USB-PD) + USB2 host | USB-C (placeholder: 1×8 header) |
| U1 | TI **TPS65987D** | USB-PD controller; negotiates ~15–20 W → VIN | SOIC-8 (placeholder for QFN) |
| U2 | Nordic **nRF52840** module | BLE — the phone tether (UART to PS) | module (placeholder: SOIC-8) |
| U3 | 100 MHz **differential oscillator** | PCIe reference clock (REFCLK±) | osc (placeholder: SOIC-8) |
| U4 | Diodes **AP63203** | buck → 3.3 V (M.2, BLE, osc) | SOT-23-5 |
| D1 | ST **USBLC6-2** | USB2 ESD protection (D±) | SOT-23 |
| J4 | JTAG header | SoM debug/program | 1×6 header |
| C1–C2 | 100 nF | IC decoupling | 0402 |
| C3–C4 | 10 µF | bulk (VIN, 3V3) | 0805 |

## Nets (25)

Power: `VBUS` (USB-C→PD), `VIN` (PD→reg+SoM), `V3V3` (reg→M.2/BLE/osc/SoM),
`GND` (plane). **PCIe** (the cartridge link): `PCIE_TX±`, `PCIE_RX±`, `REFCLK±`,
`PERST`, `CLKREQ`. **USB2**: `USB_DP/DM`, `CC1/CC2`. **BLE tether**: `BLE_TX/RX`.
**JTAG**: `TCK/TMS/TDI/TDO`.

The four diff pairs (`PCIE_TX±`, `PCIE_RX±`, `REFCLK±`, `USB_DP/DM`) route as
length-matched, controlled-impedance pairs (`route_diff_pair`).

> Footprints marked *placeholder* are the routable stand-ins used for the vcad
> layout; the production board swaps in the real connector/IC footprints. The
> fine-pitch parts (FPGA BGA, DDR) are all on the SoM, so the carrier itself
> stays simple.
