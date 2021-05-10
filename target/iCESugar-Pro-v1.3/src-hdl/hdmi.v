`default_nettype none

module hdmi (
	input clk_25mhz,
        output [3:0] gpdi_dp, gpdi_dn,
	output wifi_gpio0);

  // Tie gpio0 high, this keeps the board from rebooting
  assign wifi_gpio0 = 1'b1;

  wire clk_px, clk_tmds;
  clock clock_instance(
      .clkin_25MHz(clk_25mhz),
      .clk_75MHz(clk_px),
      .clk_375MHz(clk_tmds)
  );

  wire [7:0] red, grn, blu;
  wire [23:0] pixel;
  assign red= pixel[23:16];
  assign grn= pixel[15:8];
  assign blu= pixel[7:0];

  wire [1:0] o_red;
  wire [1:0] o_grn;
  wire [1:0] o_blu;
  wire [1:0] o_clk;
  assign o_clk[0]= clk_px;
  assign o_clk[1]= clk_px;
  wire o_rd, o_newline, o_newframe;

  // A reset line that goes low after 16 ticks
  reg [2:0] reset_cnt = 0;
  wire reset = ~reset_cnt[2];
  always @(posedge clk_25mhz)
    if (reset) reset_cnt <= reset_cnt + 1;

  hdmiClash hdmiClash_instance(
    .tmdsClk(clk_tmds),
    .clk_i(clk_px),
    .r(o_red),
    .g(o_grn),
    .b(o_blu),
  );

  ddr_diff ddr_diff_instance(
    .clk_shift(clk_tmds),
    .in_clock(o_clk),
    .in_red(o_red),
    .in_green(o_grn),
    .in_blue(o_blu),
    .out_p(gpdi_dp),
    .out_n(gpdi_dn)
  );

endmodule

