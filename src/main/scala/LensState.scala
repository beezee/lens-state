package main

import scalaz.State
import scalaz.syntax.state._
import shapeless.{lens, Lens}

object LogState {
  sealed trait Severity
  case object Silent extends Severity
  case object Info extends Severity
  case object Error extends Severity

  case class Trace(lines: List[String])
  case class Log(severity: Severity, message: String, aux: Map[String, Any], trace: Trace)

  object Log {
    val empty = Log(Silent, "", Map(), Trace(List()))
  }

  val log = lens[Log]

  def write[A](l: Lens[Log, A])(m: A => A) =
    State.modify(l.modify(_: Log)(m))
}

object Example extends App {
  import LogState._

  val app = for {
    x <- 2.state[Log]
    _ <- write(log.severity)(_ => Error)
    _ <- write(log.trace.lines)(_ :+ "foo")
    _ <- write(log.trace.lines)(_ :+ "bar")
    _ <- write(log.aux)(_ + ("foo" -> List("bar")))
    y <- 3.state[Log]
    _ <- write(log.message)(_ => "Heyo!")
  } yield x + y

  println(app.run(Log.empty))
}
