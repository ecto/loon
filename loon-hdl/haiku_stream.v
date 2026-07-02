module haiku_stream (
  input  wire clk,
  input  wire rst,
  input  wire tload,
  input  wire [3:0] taddr,
  input  wire [3:0] tdata,
  input  wire start,
  input  wire [3:0] seed,
  output wire [3:0] tok,
  output wire tvalid
);
  reg [3:0] trans [0:15];
  reg  [3:0] cur;
  reg  run;
  wire [3:0] nxt;
  assign nxt = trans[cur];
  assign tok = cur;
  assign tvalid = run;
  always @(posedge clk) begin
    if (rst) begin
      cur <= 0;
      run <= 0;
    end else begin
      if (tload) begin
        trans[taddr] <= tdata;
      end
      if (start) begin
        cur <= seed;
        run <= 1;
      end else begin
        if (run) begin
          if ((cur == 13)) begin
            run <= 0;
          end else begin
            cur <= nxt;
          end
        end
      end
    end
  end
endmodule
