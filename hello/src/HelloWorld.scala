// SPDX-License-Identifier: Apache-2.0
package me.jiuyang.hello

import me.jiuyang.zaozi.*
import me.jiuyang.zaozi.reftpe.*
import me.jiuyang.zaozi.valuetpe.*
import me.jiuyang.zaozi.default.{*, given}

case class HelloWorldParameter(width: Int) extends Parameter
given upickle.default.ReadWriter[HelloWorldParameter] = upickle.default.macroRW

class HelloWorldLayers(parameter: HelloWorldParameter) extends LayerInterface(parameter):
  def layers = Seq.empty

class HelloWorldIO(parameter: HelloWorldParameter) extends HWBundle(parameter):
  val out = Aligned(UInt(parameter.width))

class HelloWorldProbe(parameter: HelloWorldParameter) extends DVBundle[HelloWorldParameter, HelloWorldLayers](parameter)

@generator
object HelloWorld extends Generator[HelloWorldParameter, HelloWorldLayers, HelloWorldIO, HelloWorldProbe]:
  def architecture(parameter: HelloWorldParameter) =
    val io = summon[Interface[HelloWorldIO]]
    io.out := 42.U(parameter.width)
