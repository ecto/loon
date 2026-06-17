module attn_head (
  input  wire signed [15:0] q0,
  input  wire signed [15:0] q1,
  input  wire signed [15:0] k00,
  input  wire signed [15:0] k01,
  input  wire signed [15:0] k10,
  input  wire signed [15:0] k11,
  input  wire signed [15:0] k20,
  input  wire signed [15:0] k21,
  input  wire signed [15:0] k30,
  input  wire signed [15:0] k31,
  input  wire signed [15:0] v00,
  input  wire signed [15:0] v01,
  input  wire signed [15:0] v10,
  input  wire signed [15:0] v11,
  input  wire signed [15:0] v20,
  input  wire signed [15:0] v21,
  input  wire signed [15:0] v30,
  input  wire signed [15:0] v31,
  output wire signed [15:0] out0,
  output wire signed [15:0] out1
);
  wire signed [15:0] s0;
  wire signed [15:0] s1;
  wire signed [15:0] s2;
  wire signed [15:0] s3;
  wire signed [15:0] m;
  wire signed [15:0] e0;
  wire signed [15:0] e1;
  wire signed [15:0] e2;
  wire signed [15:0] e3;
  wire signed [15:0] sm;
  wire signed [15:0] p0;
  wire signed [15:0] p1;
  wire signed [15:0] p2;
  wire signed [15:0] p3;
  assign s0 = ((q0 * k00) + (q1 * k01));
  assign s1 = ((q0 * k10) + (q1 * k11));
  assign s2 = ((q0 * k20) + (q1 * k21));
  assign s3 = ((q0 * k30) + (q1 * k31));
  assign m = ((((s0 >= s1) ? s0 : s1) >= ((s2 >= s3) ? s2 : s3)) ? ((s0 >= s1) ? s0 : s1) : ((s2 >= s3) ? s2 : s3));
  assign e0 = (((s0 - m) == 0) ? 256 : (((s0 - m) == -1) ? 94 : (((s0 - m) == -2) ? 35 : (((s0 - m) == -3) ? 13 : (((s0 - m) == -4) ? 5 : (((s0 - m) == -5) ? 2 : (((s0 - m) == -6) ? 1 : 0)))))));
  assign e1 = (((s1 - m) == 0) ? 256 : (((s1 - m) == -1) ? 94 : (((s1 - m) == -2) ? 35 : (((s1 - m) == -3) ? 13 : (((s1 - m) == -4) ? 5 : (((s1 - m) == -5) ? 2 : (((s1 - m) == -6) ? 1 : 0)))))));
  assign e2 = (((s2 - m) == 0) ? 256 : (((s2 - m) == -1) ? 94 : (((s2 - m) == -2) ? 35 : (((s2 - m) == -3) ? 13 : (((s2 - m) == -4) ? 5 : (((s2 - m) == -5) ? 2 : (((s2 - m) == -6) ? 1 : 0)))))));
  assign e3 = (((s3 - m) == 0) ? 256 : (((s3 - m) == -1) ? 94 : (((s3 - m) == -2) ? 35 : (((s3 - m) == -3) ? 13 : (((s3 - m) == -4) ? 5 : (((s3 - m) == -5) ? 2 : (((s3 - m) == -6) ? 1 : 0)))))));
  assign sm = ((e0 + e1) + (e2 + e3));
  assign p0 = ((e0 * 256) / sm);
  assign p1 = ((e1 * 256) / sm);
  assign p2 = ((e2 * 256) / sm);
  assign p3 = ((e3 * 256) / sm);
  assign out0 = ((((p0 * v00) + (p1 * v10)) + ((p2 * v20) + (p3 * v30))) / 256);
  assign out1 = ((((p0 * v01) + (p1 * v11)) + ((p2 * v21) + (p3 * v31))) / 256);
endmodule
