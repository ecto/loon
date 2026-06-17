module mac_accel_4 (
  input  wire clk,
  input  wire rst,
  input  wire en,
  input  wire signed [7:0] a0,
  input  wire signed [7:0] a1,
  input  wire signed [7:0] a2,
  input  wire signed [7:0] a3,
  input  wire signed [7:0] b0,
  input  wire signed [7:0] b1,
  input  wire signed [7:0] b2,
  input  wire signed [7:0] b3,
  output reg  signed [17:0] acc
);
  wire signed [17:0] sum;
  assign sum = ((((a0 * b0) + (a1 * b1)) + (a2 * b2)) + (a3 * b3));
  always @(posedge clk) begin
    if (rst) begin
      acc <= 18'd0;
    end else begin
      if (en) begin
        acc <= (acc + sum);
      end
    end
  end
endmodule
