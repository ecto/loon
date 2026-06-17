module token_gen (
  input  wire signed [15:0] h0,
  input  wire signed [15:0] h1,
  input  wire signed [15:0] k00,
  input  wire signed [15:0] k01,
  input  wire signed [15:0] k10,
  input  wire signed [15:0] k11,
  input  wire signed [15:0] v00,
  input  wire signed [15:0] v01,
  input  wire signed [15:0] v10,
  input  wire signed [15:0] v11,
  input  wire signed [15:0] wu00,
  input  wire signed [15:0] wu01,
  input  wire signed [15:0] wu10,
  input  wire signed [15:0] wu11,
  input  wire signed [15:0] wd00,
  input  wire signed [15:0] wd01,
  input  wire signed [15:0] wd10,
  input  wire signed [15:0] wd11,
  input  wire signed [15:0] wl00,
  input  wire signed [15:0] wl01,
  input  wire signed [15:0] wl10,
  input  wire signed [15:0] wl11,
  input  wire signed [15:0] wl20,
  input  wire signed [15:0] wl21,
  output wire signed [7:0] token,
  output wire signed [15:0] l0,
  output wire signed [15:0] l1,
  output wire signed [15:0] l2
);
  wire signed [15:0] s0;
  wire signed [15:0] s1;
  wire signed [15:0] m;
  wire signed [15:0] e0;
  wire signed [15:0] e1;
  wire signed [15:0] sm;
  wire signed [15:0] p0;
  wire signed [15:0] p1;
  wire signed [15:0] c0;
  wire signed [15:0] c1;
  wire signed [15:0] a0;
  wire signed [15:0] a1;
  wire signed [15:0] u0;
  wire signed [15:0] u1;
  wire signed [15:0] g0;
  wire signed [15:0] g1;
  wire signed [15:0] f0;
  wire signed [15:0] f1;
  wire signed [15:0] o0;
  wire signed [15:0] o1;
  assign s0 = ((h0 * k00) + (h1 * k01));
  assign s1 = ((h0 * k10) + (h1 * k11));
  assign m = ((s0 >= s1) ? s0 : s1);
  assign e0 = (((s0 - m) == 0) ? 256 : (((s0 - m) == -1) ? 94 : (((s0 - m) == -2) ? 35 : (((s0 - m) == -3) ? 13 : (((s0 - m) == -4) ? 5 : (((s0 - m) == -5) ? 2 : (((s0 - m) == -6) ? 1 : 0)))))));
  assign e1 = (((s1 - m) == 0) ? 256 : (((s1 - m) == -1) ? 94 : (((s1 - m) == -2) ? 35 : (((s1 - m) == -3) ? 13 : (((s1 - m) == -4) ? 5 : (((s1 - m) == -5) ? 2 : (((s1 - m) == -6) ? 1 : 0)))))));
  assign sm = (e0 + e1);
  assign p0 = ((e0 * 256) / sm);
  assign p1 = ((e1 * 256) / sm);
  assign c0 = (((p0 * v00) + (p1 * v10)) / 256);
  assign c1 = (((p0 * v01) + (p1 * v11)) / 256);
  assign a0 = (h0 + c0);
  assign a1 = (h1 + c1);
  assign u0 = ((wu00 * a0) + (wu01 * a1));
  assign u1 = ((wu10 * a0) + (wu11 * a1));
  assign g0 = ((u0 >= 64) ? u0 : ((u0 >= 0) ? ((3 * u0) / 4) : ((u0 >= -64) ? (u0 / 4) : 0)));
  assign g1 = ((u1 >= 64) ? u1 : ((u1 >= 0) ? ((3 * u1) / 4) : ((u1 >= -64) ? (u1 / 4) : 0)));
  assign f0 = ((wd00 * g0) + (wd01 * g1));
  assign f1 = ((wd10 * g0) + (wd11 * g1));
  assign o0 = (a0 + f0);
  assign o1 = (a1 + f1);
  assign l0 = ((wl00 * o0) + (wl01 * o1));
  assign l1 = ((wl10 * o0) + (wl11 * o1));
  assign l2 = ((wl20 * o0) + (wl21 * o1));
  assign token = ((l0 >= l1) ? ((l0 >= l2) ? 0 : 2) : ((l1 >= l2) ? 1 : 2));
endmodule
