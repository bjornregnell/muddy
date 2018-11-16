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
}

object Muddy {
  var version = "0.0.1"

  def errorResponse(uri:String) = html.page("FATTAR NOLL: " + uri)

  def form(id: Int): String = s"""
  <form action="" method="get">
    <div>
      <label for="mud">Muddiest point?</label>
      <input name="mud" id="mud" value="">
      <button>Vote</button>
    </div>
  </form>
  """

  def login(group: String, id: Int): String = s"""
  <form action="$group/session=$id" method="post">
    <div>
      <button type="submit">Login</button>
    </div>
  </form>
  """



  private var _id = 1000
  def nextId(): Int = { _id += 1 ; _id }

  def handleRequest(cmd: String, uri: String, socket: Socket): Unit = {
    val os = socket.getOutputStream
    println(s"\n\nHANDLE REQUEST: $cmd $uri $socket")
    val parts = uri.split('/').drop(1).toVector // skip initial slash
    println(s"parts=$parts" )
    val response: String = (parts.head, parts.tail) match {
      case ("muddy", Seq(group, info)) if info.startsWith("session") =>
        html.page("Session: " + uri + " " + form(42) + "<br>" + group + "/" + info + "<br>" + socket.getInetAddress())

      case ("muddy",Seq(group)) =>
        val sessionId = nextId()
        html.page(s"Welcome to Muddy! Your session id is $sessionId in $group at $uri \n ${login(group, sessionId)}")
      case _ => errorResponse(uri)
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
