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

  implicit class LensStateOps[S, A](l: Lens[S, A]) {
    def update(m: A => A) =
      State.modify(l.modify(_: S)(m))
    def set(m: A) =
      State.modify(l.set(_: S)(m))
    def value = State.gets(l.get _)
  }
}

object Example extends App {
  import LogState._

  val app1 = for {
    x <- 2.state[Log]
    _ <- log.severity.set(Error)
    _ <- log.trace.lines.update(_ :+ "foo")
    _ <- log.trace.lines.update(_ :+ "bar")
  } yield x

  val app = for {
    x <- app1
    _ <- log.aux.update(_ + ("foo" -> List("bar")))
    t <- log.trace.value
    _ = println(t)
    y <- 3.state[Log]
    _ <- log.message.set("Heyo!")
  } yield x + y

  println(app.run(Log.empty))
}
