module haiku_gen (
  input  wire clk,
  input  wire rst,
  input  wire tload,
  input  wire [3:0] taddr,
  input  wire [3:0] tdata,
  input  wire start,
  input  wire [3:0] seed,
  input  wire [3:0] oaddr,
  output wire [3:0] otok,
  output wire [4:0] count
);
  reg [3:0] trans [0:15];
  reg [3:0] outm [0:15];
  reg  [3:0] cur;
  reg  [4:0] step;
  reg  run;
  wire [3:0] nxt;
  assign nxt = trans[cur];
  assign otok = outm[oaddr];
  assign count = step;
  always @(posedge clk) begin
    if (rst) begin
      cur <= 0;
      step <= 0;
      run <= 0;
    end else begin
      if (tload) begin
        trans[taddr] <= tdata;
      end
      if (start) begin
        cur <= seed;
        step <= 0;
        run <= 1;
      end else begin
        if (run) begin
          outm[step] <= cur;
          step <= (step + 1);
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
