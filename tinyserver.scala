package tinyserver

import java.net.{ServerSocket, Socket}
import java.io.OutputStream
import java.util.Scanner
import scala.util.{Try, Success, Failure}

object Html {
  val faviconData = """image/x-icon;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQEAYAAABPYyMiAAAABmJLR0T///////8JWPfcAAAACXBIWXMAAABIAAAASABGyWs+AAAAF0lEQVRIx2NgGAWjYBSMglEwCkbBSAcACBAAAeaR9cIAAAAASUVORK5CYII="""

  val faviconMini = "image/x-icon;,"

  val favicon = """<link href="data:$faviconMini" rel="icon" type="image/x-icon" />"""

  val styles = """
    |body {
    |    background-color: pink;
    |    font-size:3vw;
    |    font-family: "Lucida Console", Monaco, monospace;
    |}
    |.button {
    |      background-color: #4CAF50;
    |      border: none;
    |      color: pink;
    |      padding: 3vw 2vw;
    |      text-align: center;
    |      text-decoration: none;
    |      display: inline-block;
    |      font-size: 5vw;
    |      margin: 1vw 1vw;
    |      cursor: pointer;
    |}
    |.inpluttadej {
    |      width: 25%;
    |      font-size: 5vw;
    |      margin: 1px 1px;
    |}
  """.stripMargin

  def page(body: String): String =  {
    //minimal web page, the link tag prevents GET /favicon.ico
    s"""<!DOCTYPE html>
       |<html><head><meta charset="UTF-8"><title>MUDDY VOTING SÖRVER</title>
       |$favicon
       |<style>
       |$styles
       |</style>
       |</head>
       |<body>
       |$body
       |</body>
       |</html>
       """.stripMargin
  }

  def header(length: Int): String = //standardized header of reply to client
    s"HTTP/1.0 200 OK\nContent-length: $length\nContent-type: text/html\n\n"

  def errorResponse(uri:String, msg: String) =
    Html.page(s"FATTAR NOLL: $uri <br> $msg")

}

object Url {
  def decode(url: String): String = java.net.URLDecoder.decode(url, "UTF-8")
}

class KeyValueDatabase[K, V] {
  protected val db = new java.util.concurrent.ConcurrentHashMap[K,V]

  def put(k: K, v: V): Unit = db.put(k, v)
  def get(k: K): Option[V] = if (db.containsKey(k)) Some(db.get(k)) else None

  def toMap: Map[K, V] = {
    import scala.collection.JavaConverters._
    db.asScala.toMap
  }

  override def toString: String = toMap.toString
}

object Concurrently {
  var isUseBareThreads = false
  def apply(code: => Unit): Unit =
    if (isUseBareThreads) new Thread { override def run() = code }.start
    else {
      import scala.concurrent._
      import ExecutionContext.Implicits.global
      Future { code }
    }
}

trait TinyWebServer {
  var isLogging = false

  def toggleLogging(): String = {
    isLogging = !isLogging
    s"isLogging == $isLogging"
  }

  def log(msg: String, level: Int = 0): Unit = if (isLogging) println(msg)

  def err(msg: String): Unit = println(s"${"*" * 10} ERROR: $msg")

  final def start(port: Int): Unit = Concurrently {
    val serverSocket = new ServerSocket(port)
    log(s"Server running at: http://localhost:${serverSocket.getLocalPort}")
    while (true) {
      var clientSocket: Socket = null
      try {
        log("*** Waiting for connection...")
        clientSocket = serverSocket.accept  // blocks this thread until connect
        log(s"""${"-" * 20}
          |REQUEST:
          |  FROM: ${clientSocket.getInetAddress()}
          |  PORT: ${clientSocket.getPort()}
          |""".stripMargin)

        Concurrently {
          handleRequest(clientSocket)
          log(s"\nRequest complete.\n${"=" * 20}")
        }
      } catch { case e: Throwable =>
          err(s"Connection failed due to exception: $e")
      }
    }
  }

  private def handleRequest(clientSocket: Socket): Unit = {
    var os: OutputStream = null
    try {
      val is = clientSocket.getInputStream
      log("waiting for data on input stream")
      val scan = new Scanner(is, "UTF-8")
      val cmd = Try(scan.next)
      val url = Try(scan.next)
      log(s"cmd = $cmd url = $url")
      val output: String = response(cmd, url, clientSocket.getInetAddress.toString)
      log(s"SENDING RESPONSE:\n$output")
      os = clientSocket.getOutputStream
      os.write(output.getBytes("UTF-8"))
    } finally {
      if (os != null) os.close
      if (clientSocket != null) clientSocket.close
    }
  }

  def response(cmd: Try[String], url: Try[String], inetAdress: String): String
}
