module stream_mac (
  input  wire clk,
  input  wire rst,
  input  wire wpush,
  input  wire [1:0] wdata,
  input  wire xwe,
  input  wire [2:0] xaddr,
  input  wire [7:0] xdata,
  output reg  [15:0] acc,
  output reg  [3:0] pos
);
  reg [1:0] wmem [0:7];
  reg [7:0] xmem [0:7];
  reg  [2:0] whead;
  reg  [2:0] wtail;
  reg  [3:0] wcount;
  wire wempty;
  wire do_pop;
  assign wempty = (wcount == 0);
  assign do_pop = ((wempty == 0) & ((pos == 8) == 0));
  always @(posedge clk) begin
    if (rst) begin
      whead <= 0;
      wtail <= 0;
      wcount <= 0;
      pos <= 0;
      acc <= 0;
    end else begin
      if (wpush) begin
        wmem[wtail] <= wdata;
        wtail <= (wtail + 1);
      end
      if (xwe) begin
        xmem[xaddr] <= xdata;
      end
      if (do_pop) begin
        acc <= ($signed(acc) + ($signed(wmem[whead]) == 1 ? $signed(xmem[pos]) : ($signed(wmem[whead]) == -1 ? -$signed(xmem[pos]) : 0)));
        whead <= (whead + 1);
        pos <= (pos + 1);
      end
      wcount <= ((wcount + wpush) - do_pop);
    end
  end
endmodule
