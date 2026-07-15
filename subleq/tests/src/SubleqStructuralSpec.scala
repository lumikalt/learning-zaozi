// SPDX-License-Identifier: Apache-2.0
package me.jiuyang.subleqtest

import me.jiuyang.subleq.*
import utest.*

object SubleqStructuralSpec extends TestSuite:
  val tests = Tests:
    test("memory port declarations"):
      Subleq.verilogTest(SubleqParameter(8))(
        "output [7:0] memAddr,",
        "output       memWen,",
        "output [7:0] memWdata,",
        "input  [7:0] memRdata,",
        "output       halted"
      )

    // Regression guard: operand-fetch/pc-increment arithmetic must be real
    // width-preserving adders, not accidentally truncated to 1 bit by a
    // misused tail() (tail(n) drops the top n bits, it does not resize to
    // n bits -- an earlier version of this design got that backwards).
    test("pc and state arithmetic is width-preserving"):
      Subleq.verilogTest(SubleqParameter(8))(
        "pc + 8'h1",
        "pc + 8'h2",
        "pc + 8'h3",
        "state + 3'h1"
      )

    // Regression guard: halt must be gated on the branch actually being
    // taken, not merely on the instruction's c-operand equal to all-ones
    // (an earlier version halted on the fallthrough path too).
    test("branch condition feeds both pc update and halt"):
      Subleq.verilogTest(SubleqParameter(8)): out =>
        val branchLine = out.linesIterator.find(_.contains("branchTaken =")).getOrElse("")
        val pcLine     = out.linesIterator.find(_.contains("pc <= branchTaken")).getOrElse("")
        branchLine.contains("$signed(diff) < 8'sh1") && pcLine.contains("branchTaken ? rC : pc + 8'h3")
