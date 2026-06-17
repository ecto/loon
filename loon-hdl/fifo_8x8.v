module fifo_8x8 (
  input  wire clk,
  input  wire rst,
  input  wire push,
  input  wire pop,
  input  wire [7:0] wdata,
  output wire [7:0] rdata,
  output wire full,
  output wire empty
);
  reg [7:0] mem [0:7];
  reg  [2:0] head;
  reg  [2:0] tail;
  reg  [3:0] count;
  wire do_push;
  wire do_pop;
  assign full = (count == 8);
  assign empty = (count == 0);
  assign do_push = (push & (full == 0));
  assign do_pop = (pop & (empty == 0));
  assign rdata = mem[head];
  always @(posedge clk) begin
    if (rst) begin
      head <= 0;
      tail <= 0;
      count <= 0;
    end else begin
      if (do_push) begin
        mem[tail] <= wdata;
        tail <= (tail + 1);
      end
      if (do_pop) begin
        head <= (head + 1);
      end
      count <= ((count + do_push) - do_pop);
    end
  end
endmodule
