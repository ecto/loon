module act_silu (
  input  wire signed [7:0] x,
  output wire signed [15:0] y
);
  assign y = ((x >= 64) ? x : ((x >= 0) ? ((3 * x) / 4) : ((x >= -64) ? (x / 4) : 0)));
endmodule
