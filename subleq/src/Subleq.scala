// SPDX-License-Identifier: Apache-2.0
package me.jiuyang.subleq

import me.jiuyang.zaozi.*
import me.jiuyang.zaozi.reftpe.*
import me.jiuyang.zaozi.valuetpe.*
import me.jiuyang.zaozi.default.{*, given}
import me.jiuyang.testlib.HasVerilogTest

// Single-instruction (SUBLEQ) core with an external single-port synchronous
// memory: subleq a, b, c computes mem[b] -= mem[a], then jumps to c if the
// result is <= 0 (signed), else falls through to pc+3. A branch target of
// all-ones halts the core, the common SUBLEQ convention for an invalid/
// sentinel address. The memory read latency is one cycle, so every operand
// fetch takes two states: one to drive memAddr, one to capture memRdata.
// rMB (mem[b]) needs its own capture state separate from the write: the
// write's data depends on rMB, which isn't valid until the cycle after the
// read that produces it.
case class SubleqParameter(width: Int) extends Parameter
given upickle.default.ReadWriter[SubleqParameter] = upickle.default.macroRW

class SubleqLayers(parameter: SubleqParameter) extends LayerInterface(parameter):
  def layers = Seq.empty

class SubleqIO(parameter: SubleqParameter) extends HWBundle(parameter):
  val clock:    BundleField[Clock] = Flipped(Clock())
  val reset:    BundleField[Reset] = Flipped(Reset())
  val memAddr:  BundleField[UInt]  = Aligned(UInt(parameter.width))
  val memWen:   BundleField[Bool]  = Aligned(Bool())
  val memWdata: BundleField[UInt]  = Aligned(UInt(parameter.width))
  val memRdata: BundleField[UInt]  = Flipped(UInt(parameter.width))
  val halted:   BundleField[Bool]  = Aligned(Bool())

class SubleqProbe(parameter: SubleqParameter) extends DVBundle[SubleqParameter, SubleqLayers](parameter)

@generator
object Subleq extends Generator[SubleqParameter, SubleqLayers, SubleqIO, SubleqProbe] with HasVerilogTest:
  def architecture(parameter: SubleqParameter) =
    val io           = summon[Interface[SubleqIO]]
    given ClockScope = ClockScope.posedge(io.clock)
    given ResetScope = ResetScope.syncActiveHigh(io.reset)

    val w = parameter.width

    val sFetchA   = 0
    val sFetchB   = 1
    val sFetchC   = 2
    val sReadA    = 3
    val sReadB    = 4
    val sCapture  = 5
    val sWriteB   = 6
    val sUpdatePc = 7

    val state  = RegInit(sFetchA.U(3))
    val pc     = RegInit(0.U(w))
    val rA     = Reg(UInt(w))
    val rB     = Reg(UInt(w))
    val rC     = Reg(UInt(w))
    val rMA    = Reg(UInt(w))
    val rMB    = Reg(UInt(w))
    val halted = RegInit(false.B)

    val pcPlus1 = (pc + 1.U(w)).asBits.tail(1).asUInt
    val pcPlus2 = (pc + 2.U(w)).asBits.tail(1).asUInt
    val pcPlus3 = (pc + 3.U(w)).asBits.tail(1).asUInt

    val diff        = (rMB - rMA).asBits.tail(1).asUInt
    val branchTaken = diff.asBits.asSInt <= 0.S(w)
    val allOnes     = ((BigInt(1) << w) - 1).U(w)
    val isHalt      = branchTaken & (rC === allOnes)

    io.memAddr  := (state === sFetchA.U(3)) ? (
      pc,
      (state === sFetchB.U(3)) ? (
        pcPlus1,
        (state === sFetchC.U(3)) ? (
          pcPlus2,
          (state === sReadA.U(3)) ? (
            rA,
            (state === sReadB.U(3)) ? (rB, (state === sCapture.U(3)) ? (rB, (state === sWriteB.U(3)) ? (rB, pc)))
          )
        )
      )
    )
    io.memWen   := state === sWriteB.U(3)
    io.memWdata := diff
    io.halted   := halted

    rA  := (state === sFetchB.U(3)) ? (io.memRdata, rA)
    rB  := (state === sFetchC.U(3)) ? (io.memRdata, rB)
    rC  := (state === sReadA.U(3)) ? (io.memRdata, rC)
    rMA := (state === sReadB.U(3)) ? (io.memRdata, rMA)
    rMB := (state === sCapture.U(3)) ? (io.memRdata, rMB)

    pc     := (state === sUpdatePc.U(3)) ? (branchTaken ? (rC, pcPlus3), pc)
    halted := (state === sUpdatePc.U(3)) ? ((halted | isHalt), halted)
    state  := (state === sUpdatePc.U(3)) ? (sFetchA.U(3), (state + 1.U(3)).asBits.tail(1).asUInt)
