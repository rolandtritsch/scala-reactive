package simulations

import common._

import com.weiglewilczek.slf4s.Logging

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean): Unit = {
    if(s != sigVal) {
      sigVal = s
      actions.foreach(a => a())
    }
  }

  def addAction(a: Simulator#Action): Unit = {
    actions = a :: actions
    a()
  }

  override def toString: String = {
    sigVal.toString
  }
}

abstract class CircuitSimulator extends Simulator with Logging {
  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int
  val DemuxDelay: Int

  def probe(name: String, wire: Wire): Unit = {
    wire.addAction {
      () => afterDelay(0) {
        logger.info(s"$currentTime: $name -> ${wire.getSignal}")
      }
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction():Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    }
    input.addAction(invertAction)
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output.setSignal(a1Sig & a2Sig)
      }
    }
    a1.addAction(andAction)
    a2.addAction(andAction)
  }

  def orGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1Sig | a2Sig)
      }
    }
    a1.addAction(orAction)
    a2.addAction(orAction)
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      afterDelay(0) {
        val a1Inv, a2Inv, added = new Wire

        inverter(a1, a1Inv)
        inverter(a2, a2Inv)
        andGate(a1Inv, a2Inv, added)
        inverter(added, output)
      }
    }
    a1.addAction(orAction)
    a2.addAction(orAction)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = {
    def demuxActionNand(): Unit = {
      afterDelay(0) {
        val s0 = c(0)
        val s1 = c(1)
        val s0Inv, s1Inv = new Wire
        val out0, out1, out2, out3 = new Wire

        inverter(s0, s0Inv)
        inverter(s1, s1Inv)

        andGate(s0Inv, s1Inv, out0)
        andGate(in, out0, out(0))
        andGate(s0, s1Inv, out1)
        andGate(in, out1, out(1))
        andGate(s0Inv, s1, out2)
        andGate(in, out2, out(2))
        andGate(s0, s1, out3)
        andGate(in, out3, out(3))
      }
    }

    def demuxAction(): Unit = {
      afterDelay(DemuxDelay) {
        out(0).setSignal(!c(0).getSignal & !c(1).getSignal & in.getSignal)
        out(1).setSignal(c(0).getSignal & !c(1).getSignal & in.getSignal)
        out(2).setSignal(!c(0).getSignal & c(1).getSignal & in.getSignal)
        out(3).setSignal(c(0).getSignal & c(1).getSignal & in.getSignal)
      }
    }
    // This need to work for 0/1, 1/2, 2/4, 3/8, 4/16, ...
    require(c.size == 2 & out.size == 4, "need to make this variable")
    in.addAction(demuxAction)
    c.foreach(_.addAction(demuxAction))
  }
}

object Circuit extends CircuitSimulator with Parameters {
  def andGateExample: Unit = {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
}

object CircuitMain extends App {
  Circuit.andGateExample
}
