package simulations

import com.weiglewilczek.slf4s.Logging

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._

class QuickCheckCircuit(val sim: CircuitSimulator) extends Properties("Circuit") with Logging {
  property("inverter") = forAll {s: Boolean =>
    val in, out = new Wire
    sim.inverter(in, out)

    in.setSignal(s)
    sim.run
    logger.debug(s"${out.getSignal} is not ${s}")
    out.getSignal == (!s)
  }

  property("andGate") = forAll {(s0: Boolean, s1: Boolean) =>
    val in0, in1, out = new Wire
    sim.andGate(in0, in1, out)

    in0.setSignal(s0)
    in1.setSignal(s1)
    sim.run
    logger.debug(s"${s0} & ${s1} is ${out.getSignal}/${s0 & s1}")
    out.getSignal == (s0 & s1)
  }

  property("orGate") = forAll {(s0: Boolean, s1: Boolean) =>
    val in0, in1, out = new Wire
    sim.orGate(in0, in1, out)

    in0.setSignal(s0)
    in1.setSignal(s1)
    sim.run
    logger.debug(s"${s0} | ${s1} is ${out.getSignal}/${s0 | s1}")
    out.getSignal == (s0 | s1)
  }

  property("orGate2") = forAll {(s0: Boolean, s1: Boolean) =>
    val in0, in1, out = new Wire
    sim.orGate2(in0, in1, out)

    in0.setSignal(s0)
    in1.setSignal(s1)
    sim.run
    logger.debug(s"${s0} | ${s1} is ${out.getSignal}/${s0 | s1}")
    out.getSignal == (s0 | s1)
  }

  property("demux i 0") = forAll {(s0: Boolean, s1: Boolean) =>
    val input = new Wire
    val selector = List(new Wire, new Wire)
    val output = List(new Wire, new Wire, new Wire, new Wire)
    sim.demux(input, selector, output)

    input.setSignal(false)
    selector(0).setSignal(s0)
    selector(1).setSignal(s1)
    sim.run
    // logger.debug(s"${input}/${s0}/${s1} is ${output.mkString(""",""")}")
    output.forall(_.getSignal == false)
  }

  property("demux i 1") = forAll {(s0: Boolean, s1: Boolean) =>
    val input = new Wire
    val selector = List(new Wire, new Wire)
    val output = List(new Wire, new Wire, new Wire, new Wire)
    sim.demux(input, selector, output)

    input.setSignal(true)
    selector(0).setSignal(s0)
    selector(1).setSignal(s1)
    sim.run
    // logger.debug(s"${input}/${s0}/${s1} is ${output.mkString(""",""")}")
    if(!s0 & !s1) {
      output(0).getSignal & !output(1).getSignal & !output(2).getSignal & !output(3).getSignal
    } else if(s0 & !s1) {
      !output(0).getSignal & output(1).getSignal & !output(2).getSignal & !output(3).getSignal
    } else if(!s0 & s1) {
      !output(0).getSignal & !output(1).getSignal & output(2).getSignal & !output(3).getSignal
    } else if(s0 & s1) {
      !output(0).getSignal & !output(1).getSignal & !output(2).getSignal & output(3).getSignal
    } else {
      false
    }
  }
}

class CircuitSuite extends Properties("Circuit") with FunSuite with Checkers {
  val sim = new CircuitSimulator with Parameters

  test("inverter") {
    val in, out = new Wire
    sim.inverter(in, out)

    in.setSignal(true)
    sim.run
    assert(out.getSignal === false, "inv 1")

    in.setSignal(out.getSignal)
    sim.run
    assert(out.getSignal === true, "inv 2")
  }

  test("andGate") {
    val in1, in2, out = new Wire
    sim.andGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    sim.run
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    sim.run
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    sim.run
    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    sim.orGate(in1, in2, out)

    in1.setSignal(true)
    in2.setSignal(true)
    sim.run
    assert(out.getSignal === true, "or 1")

    in1.setSignal(false)
    sim.run
    assert(out.getSignal === true, "or 2")

    in2.setSignal(false)
    sim.run
    assert(out.getSignal === false, "or 3")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    sim.orGate2(in1, in2, out)

    in1.setSignal(true)
    in2.setSignal(true)
    sim.run
    assert(out.getSignal === true, "or 1")

    in1.setSignal(false)
    sim.run
    assert(out.getSignal === true, "or 2")

    in2.setSignal(false)
    sim.run
    assert(out.getSignal === false, "or 3")
  }

  test("demux i 0") {
    val control = List(new Wire, new Wire)
    val input = new Wire
    val output = List(new Wire, new Wire, new Wire, new Wire)
    sim.demux(input, control, output)

    input.setSignal(false)
    sim.run
    output.foreach(o => assert(o.getSignal === false, "demux in 0"))
  }

  test("quickcheck") {
    check(new QuickCheckCircuit(sim))
  }
}
