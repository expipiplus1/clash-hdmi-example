# HDMI Test Pattern for the iCESugar-Pro 1.3

Outputs at `1280×720@60Hz`, this can be changed in
[DisplayMode.hs](./src/DisplayMode.hs).

The little LFE5U at the -6 speed grade can just about get to half the required
pixel clock. As it has no SERDES elements one has to use the DDR transcievers
to send out the clock and pixels.

## Building

Can build with `./Shakefile.hs iCESugar-Pro-v1.3/bitfile` and program with
`./Shakefile.hs iCESugar-Pro-v1.3/program`.

Alternate targets can be added to `targets` in [ShakeFile.hs](./Shakefile.hs).

## Verilog source

`clock.v` was generated with `ecppll --clkin_name clkin_25MHz --clkin 25 --clkout0_name clk_375MHz --clkout0 375 --clkout1_name clk_75MHz --clkout1 75 --module clock --internal_feedback --file target/iCESugar-Pro-v1.3/src-hdl/clock.v`
