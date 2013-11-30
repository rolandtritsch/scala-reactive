package simulations

import com.weiglewilczek.slf4s.Logging

class Simulator extends Logging {
  type Action = () => Unit

  protected type Agenda = List[WorkItem]

  case class WorkItem(time: Int, action: Action)

  protected[simulations] var agenda: Agenda = List()
  protected var currentTime = 0

  protected def afterDelay(delay: Int)(action: => Unit):Unit = {
    val item = WorkItem(currentTime + delay, () => action)
    def insert(ag: Agenda): Agenda = {
      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail)
    }
    agenda = insert(agenda)
  }

  protected[simulations] def next: Unit = {
    agenda match {
      case List() => {}
      case WorkItem(time, action) :: rest => {
        agenda = rest
        currentTime = time
        action()
      }
    }
  }

  def run: Unit = {
    logger.debug("*** New propagation ***")
    while(!agenda.isEmpty) {
      next
    }
  }
}
