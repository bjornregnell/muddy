package muddy

object Main {
  def helpMsg: String = s"""
	|  Commands:
	|    stop       stop server, exit
	|    show       show database
	|    help | ?   show this message
	""".stripMargin

	def getCmd() =
		Option(scala.io.StdIn.readLine("muddy> ")).getOrElse("").trim.toLowerCase

	def main(args: Array[String]) {
	  val port = scala.util.Try(args(0).toInt).getOrElse(8089)
    Muddy.start(port)
		var quit = false
    while (!quit) {
			val reply = getCmd() match {
				case "stop"       => quit = true; "Goodbye Almighty User!"
				case "show"       => Muddy.showDatabase
				case "help" | "?" => helpMsg
				case ""           => ""
				case unkown       => s"Unknown command: $unkown\n$helpMsg"
			}
			println(reply)
		}
		System.exit(0)
	}
}
