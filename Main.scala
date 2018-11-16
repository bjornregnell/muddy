package muddy

import scala.concurrent._
import ExecutionContext.Implicits.global

object Main {
	def main(args: Array[String]) {
	  val port = scala.util.Try(args(0).toInt).getOrElse(8089)
    Future { Muddy.serverLoop(port) }
    def stop =
      Option(scala.io.StdIn.readLine("Type stop<enter> to stop Server: "))
        .getOrElse("")
    while (stop != "stop") println
	}
}
