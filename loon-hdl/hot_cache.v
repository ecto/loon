module hot_cache (
  input  wire clk,
  input  wire rst,
  input  wire req,
  input  wire [3:0] eid,
  output wire hit,
  output reg  [7:0] hits,
  output reg  [7:0] misses
);
  reg [3:0] tag [0:3];
  reg valid [0:3];
  wire [1:0] idx;
  wire hit_w;
  assign idx = (eid & 3);
  assign hit_w = (valid[idx] & (tag[idx] == eid));
  assign hit = (req & hit_w);
  always @(posedge clk) begin
    if (rst) begin
      valid[0] <= 0;
      valid[1] <= 0;
      valid[2] <= 0;
      valid[3] <= 0;
      hits <= 0;
      misses <= 0;
    end else begin
      if (req) begin
        if (hit_w) begin
          hits <= (hits + 1);
        end else begin
          tag[idx] <= eid;
          valid[idx] <= 1;
          misses <= (misses + 1);
        end
      end
    end
  end
endmodule
