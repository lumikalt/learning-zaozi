// SPDX-License-Identifier: Apache-2.0
package me.jiuyang.subleqtest

import me.jiuyang.subleq.*
import utest.*

// Runs the design through iverilog against real SUBLEQ programs. The
// structural spec only checks that generated text contains expected
// substrings; it can't catch a functional bug where the RTL is
// structurally fine but computes the wrong value (as happened during
// development: a capture/write timing hazard, and a halt condition that
// fired on the wrong path). Only simulation catches that class of bug.
object SubleqSimSpec extends TestSuite:

  private def moduleNameOf(verilog: String): String =
    "module (\\S+)\\(".r.findFirstMatchIn(verilog).get.group(1)

  // iverilog (this Icarus version) doesn't support `automatic` variable
  // lifetime, which firtool emits for the jextract-safe stack-depth
  // workaround elsewhere in this toolchain; harmless to strip for a
  // single-instance testbench with no recursion.
  private def iverilogCompatible(verilog: String): String =
    verilog.replace("automatic logic", "logic")

  private def testbench(dut: String): String =
    s"""`timescale 1ns/1ps
       |module tb;
       |  reg clock = 0;
       |  reg reset = 1;
       |  wire [7:0] memAddr;
       |  wire memWen;
       |  wire [7:0] memWdata;
       |  reg [7:0] memRdata;
       |  wire halted;
       |
       |  reg [7:0] mem [0:255];
       |  integer i;
       |
       |  $dut dut (
       |    .clock(clock), .reset(reset), .memAddr(memAddr), .memWen(memWen),
       |    .memWdata(memWdata), .memRdata(memRdata), .halted(halted)
       |  );
       |
       |  always #5 clock = ~clock;
       |
       |  always @(posedge clock) begin
       |    memRdata <= mem[memAddr];
       |    if (memWen) mem[memAddr] <= memWdata;
       |  end
       |
       |  task doReset;
       |    begin
       |      reset = 1;
       |      @(posedge clock);
       |      @(posedge clock);
       |      reset = 0;
       |    end
       |  endtask
       |
       |  initial begin
       |    // subleq Y, X, pc+3 (not taken); subleq ZERO, ZERO, halt (always taken)
       |    for (i = 0; i < 256; i = i + 1) mem[i] = 8'd0;
       |    mem[0] = 8'd6; mem[1] = 8'd7; mem[2] = 8'd3;
       |    mem[3] = 8'd8; mem[4] = 8'd8; mem[5] = 8'd255;
       |    mem[6] = 8'd3;   // Y
       |    mem[7] = 8'd10;  // X
       |    mem[8] = 8'd0;   // ZERO
       |    doReset;
       |    wait (halted);
       |    #1;
       |    if (mem[7] !== 8'd7) begin
       |      $$display("FAIL single-sub: mem[7]=%0d expected 7", mem[7]);
       |      $$finish;
       |    end
       |    $$display("PASS single-sub");
       |
       |    // decrement loop: exercises fallthrough (pc+3) and the
       |    // unconditional jump-back idiom (0 - 0 <= 0 is always true)
       |    for (i = 0; i < 256; i = i + 1) mem[i] = 8'd0;
       |    mem[0] = 8'd10; mem[1] = 8'd11; mem[2] = 8'd255;
       |    mem[3] = 8'd12; mem[4] = 8'd12; mem[5] = 8'd0;
       |    mem[10] = 8'd1; // ONE
       |    mem[11] = 8'd3; // COUNTER
       |    mem[12] = 8'd0; // ZERO
       |    doReset;
       |    wait (halted);
       |    #1;
       |    if (mem[11] !== 8'd0) begin
       |      $$display("FAIL loop: mem[11]=%0d expected 0", mem[11]);
       |      $$finish;
       |    end
       |    $$display("PASS loop");
       |
       |    $$display("ALL TESTS PASSED");
       |    $$finish;
       |  end
       |
       |  initial begin
       |    #200000;
       |    $$display("TIMEOUT");
       |    $$finish;
       |  end
       |endmodule
       |""".stripMargin

  val tests = Tests:
    test("subleq programs simulate correctly"):
      val verilog = iverilogCompatible(Subleq.verilogString(SubleqParameter(8)))
      val dut     = moduleNameOf(verilog)

      val dir = os.temp.dir(prefix = "subleq-sim")
      os.write(dir / "subleq.v", verilog)
      os.write(dir / "tb.v", testbench(dut))

      os.proc("iverilog", "-g2012", "-o", "sim.out", "subleq.v", "tb.v").call(cwd = dir)
      val result = os.proc("vvp", "sim.out").call(cwd = dir, check = false)
      val output = result.out.text()

      assert(result.exitCode == 0)
      assert(output.contains("ALL TESTS PASSED"))
      assert(!output.contains("FAIL"))
      assert(!output.contains("TIMEOUT"))
