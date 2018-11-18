package muddy

import java.net.{ServerSocket, Socket}
import java.io.OutputStream
import java.util.Scanner
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import ExecutionContext.Implicits.global

object Html {
	def page(body: String): String =  //minimal web page
    s"""<!DOCTYPE html>
       |<html><head><meta charset="UTF-8"><title>Muddy Sörvor</title></head>
       |<body>
       |$body
       |</body>
       |</html>
       """.stripMargin

	def header(length: Int): String = //standardized header of reply to client
	  s"HTTP/1.0 200 OK\nContent-length: $length\nContent-type: text/html\n\n"

  def errorResponse(uri:String, msg: String) =
    Html.page(s"FATTAR NOLL: $msg <br> $uri")

}

object Url {
  def decode(url: String): String = java.net.URLDecoder.decode(url, "UTF-8")
}

object Muddy {
  var version = "0.0.1"


  def loginForm(topic: String, id: String): String = s"""
  <form action="/muddy/$topic/session=$id" method="get">
    <div>
      <button type="submit">Login</button>
    </div>
  </form>
  """

  def loginPage(sessionId: String, topic: String): String =
    Html.page(s"""
      Welcome to Muddy! Press button to start voting on $topic! <br>
      ${loginForm(topic, sessionId)}
    """)

  def votingForm(value: String = ""): String = s"""
  <form action="" method="get">
    <div>
      <label for="mud">Your vote: </label>
      <input name="mud" id="mud" value="$value">
      <button>Update</button>
    </div>
  </form>
  """


  case class Key(id: String, topic: String)
  case class Value(value: String)

  import scala.collection.JavaConverters._
  val db = new java.util.concurrent.ConcurrentHashMap[Key,Value]

  //TODO: def database: Map[Key, Value] = db.toArray.toMap ???
  //TODO: check that session is existing

  def setVote(id: String, topic: String, value: String): Unit =
    db.put(Key(id, topic), Value(value))

  def getVote(id: String, topic: String): String =
    Option(db.get(Key(id, topic))).map(_.value).getOrElse("")

  def showCounts(topic: String): String =
    db.asScala.filterKeys(_.topic == topic)
      .values.collect { case Value(s) if s.nonEmpty => s }
      .groupBy(x => x)
      .collect { case (v,xs) => (v, xs.size) }.toSeq
      .sortBy(_._2).reverse
      .map { case (v, n) => s" $v = $n " }.mkString("<br>")

  def showDatabase(): String = db.toString

  def nextId(): String = java.util.UUID.randomUUID.toString

  def handleRequest(cmd: String, uri: String, socket: Socket): Unit = {
    val os = socket.getOutputStream
    println(s"\n\nHANDLE REQUEST: $cmd $uri $socket")
    val parts = uri.split('/').drop(1).toVector // skip initial slash
    println(s"parts=$parts" )
    val response: String = (parts.head, parts.tail) match {

      case ("muddy", Seq(topic, info)) if info.startsWith("session=") =>
        val id = info.stripPrefix("session=").takeWhile(_ != '?')
        val vote = Url.decode(info.dropWhile(_ != '?').stripPrefix("?").stripPrefix("mud="))
        println(s"\n*** vote = $vote")
        setVote(id = id, topic = topic, value = vote)
        val result = Html.page("Your ip address: " + socket.getInetAddress() + "<br>" + votingForm(vote) + "<br>"  +
          showCounts(topic) + "<br> <br> <br>"
          + showDatabase
        )
        result

      case ("muddy", Seq(topic)) => loginPage(sessionId = nextId(), topic)

      case _ => Html.errorResponse(uri, "sidan är vojd :( ")
    }
    os.write(Html.header(response.size).getBytes("UTF-8"))
    os.write(response.getBytes("UTF-8"))
    os.close
    socket.close
  }

  def serverLoop(port: Int): Unit = {
    println(s"\nStarting Muddy server version $version on port $port...")
    val server= new ServerSocket(port)
    println(s"Server running at: http://localhost:${server.getLocalPort}/muddy")
		while (true) {
  		Try {
  		  var socket = server.accept  // blocks thread until connect
	  	  val scan = new Scanner(socket.getInputStream, "UTF-8")
		    val (cmd, uri) = (scan.next, scan.next)
			  println(s"Request: $cmd $uri from SOCKET: $socket at RemoteSocketAddress = ${socket.getRemoteSocketAddress()} at InetAddress=${socket.getInetAddress()}")
		    Future { handleRequest(cmd, uri, socket) }.onComplete {
		      case Failure(e) => println(s"Reqest failed: $e")
          case Success(_) => println(s"\n___Request complete.\n")
		    }
		  }.recover{ case e: Throwable => s"Connection failed: $e" }
		}
  }
}
