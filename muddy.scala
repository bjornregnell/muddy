package muddy

import java.net.{ServerSocket, Socket}
import java.io.OutputStream
import java.util.Scanner
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import ExecutionContext.Implicits.global

object html {
	def page(body: String): String =  //minimal web page
    s"""<!DOCTYPE html>
       |<html><head><meta charset="UTF-8"><title>Min Sörver</title></head>
       |<body>
       |$body
       |</body>
       |</html>
       """.stripMargin

	def header(length: Int): String = //standardized header of reply to client
	  s"HTTP/1.0 200 OK\nContent-length: $length\nContent-type: text/html\n\n"

  def errorResponse(uri:String) = html.page("FATTAR NOLL: " + uri)

}

object Muddy {
  var version = "0.0.1"

  val db = new java.util.concurrent.ConcurrentHashMap[String,String]

  def form(value: String = ""): String = s"""
  <form action="" method="get">
    <div>
      <label for="mud">Muddiest point?</label>
      <input name="mud" id="mud" value="$value">
      <button>Vote</button>
    </div>
  </form>
  """

  def login(group: String, id: String): String = s"""
  <form action="session=$id" method="post">
    <div>
      <button type="submit">Login</button>
    </div>
  </form>
  """

  def setVote(id: String, value: String): Unit = if (value.nonEmpty) db.put(id, value)

  def getVote(id: String): String = Option(db.get(id)).getOrElse("")

  def showDatabase(): String = db.toString

  def nextId(): String = java.util.UUID.randomUUID.toString

  def handleRequest(cmd: String, uri: String, socket: Socket): Unit = {
    val os = socket.getOutputStream
    println(s"\n\nHANDLE REQUEST: $cmd $uri $socket")
    val parts = uri.split('/').drop(1).toVector // skip initial slash
    println(s"parts=$parts" )
    val response: String = (parts.head, parts.tail) match {

      case ("muddy", Seq(group, info)) if info.startsWith("session=") =>
        val id = info.stripPrefix("session=").takeWhile(_ != '?')
        val vote = info.dropWhile(_ != '?').stripPrefix("?mud=")
        println(s"\n*** vote = $vote")
        setVote(id, vote)
        val result = html.page("Session: " + uri + " " + form("hejsan") + "<br>" + group + "/" + info + "<br>" + socket.getInetAddress()) + "<br>" + showDatabase
        result

      case ("muddy",Seq(group)) =>
        val sessionId = nextId()
        html.page(s"Welcome to Muddy! Your session id is $sessionId in $group at $uri \n ${login(group, sessionId)}")

      case _ => html.errorResponse(uri)
    }
    os.write(html.header(response.size).getBytes("UTF-8"))
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
		    Future { handleRequest(cmd, uri, socket) }.onFailure {
		      case e => println(s"Reqest failed: $e")
		    }
		  }.recover{ case e: Throwable => s"Connection failed: $e" }
		}
  }
}
