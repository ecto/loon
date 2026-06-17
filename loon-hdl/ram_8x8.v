module ram_8x8 (
  input  wire clk,
  input  wire we,
  input  wire [2:0] waddr,
  input  wire [7:0] wdata,
  input  wire [2:0] raddr,
  output wire [7:0] rdata
);
  reg [7:0] mem [0:7];
  always @(posedge clk) begin
    if (we) begin
      mem[waddr] <= wdata;
    end
  end
  assign rdata = mem[raddr];
endmodule
