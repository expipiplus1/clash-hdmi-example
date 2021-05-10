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
      .clk_25MHz(clk_px),
      .clk_250MHz(clk_tmds)
  );

  wire [7:0] red, grn, blu;
  wire [23:0] pixel;
  assign red= pixel[23:16];
  assign grn= pixel[15:8];
  assign blu= pixel[7:0];

  wire o_red;
  wire o_grn;
  wire o_blu;
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

  OBUFDS OBUFDS_red(.I(o_red), .O(gpdi_dp[2]), .OB(gpdi_dn[2]));
  OBUFDS OBUFDS_grn(.I(o_grn), .O(gpdi_dp[1]), .OB(gpdi_dn[1]));
  OBUFDS OBUFDS_blu(.I(o_blu), .O(gpdi_dp[0]), .OB(gpdi_dn[0]));
  OBUFDS OBUFDS_clock(.I(clk_px), .O(gpdi_dp[3]), .OB(gpdi_dn[3]));

endmodule

