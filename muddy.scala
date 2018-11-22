package muddy

import java.net.{ServerSocket, Socket}
import java.io.OutputStream
import java.util.Scanner
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import ExecutionContext.Implicits.global

object Html {
  def faviconData = """image/x-icon;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQEAYAAABPYyMiAAAABmJLR0T///////8JWPfcAAAACXBIWXMAAABIAAAASABGyWs+AAAAF0lEQVRIx2NgGAWjYBSMglEwCkbBSAcACBAAAeaR9cIAAAAASUVORK5CYII="""

  def faviconMini = "image/x-icon;,"

  def favicon = """<link href="data:$faviconMini" rel="icon" type="image/x-icon" />"""

	def page(body: String): String =  {
		//minimal web page, the link tag prevents GET /favicon.ico
    s"""<!DOCTYPE html>
       |<html><head><meta charset="UTF-8"><title>Muddy Sörvor</title>
       |$favicon
       |<style>
       |body {
       |    background-color: pink;
       |    font-size:5vw;
       |    font-family: "Lucida Console", Monaco, monospace;
       |}
       |.button {
       |      background-color: #4CAF50;
       |      border: none;
       |      color: white;
       |      padding: 15px 32px;
       |      text-align: center;
       |      text-decoration: none;
       |      display: inline-block;
       |      font-size: 5vw;
       |      margin: 1px 1px;
       |      cursor: pointer;
       |}
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

object Concurrently {
  def apply(code: => Unit): Unit = new Thread{
    override def run(): Unit = code
  }.start
}

object Muddy {
  var version = "0.0.3"

  def log(msg: String): Unit = println(msg)

  def loginForm(topic: String, id: String): String = s"""
  <form action="/muddy/$topic/session=$id" method="get">
    <div>
      <button class="button" type="submit">Login</button>
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
      <button class="button">Update</button>
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

  def response(cmd: Try[String], url: Try[String], inetAdress: String): String = {
    (cmd, url) match {
      case (Success("GET"), Success("/favicon.ico")) =>
        Html.faviconMini

      case (Success("GET"), Success("/hej")) =>
        val page = Html.page("Hejhej")
        Html.header(page.size) + page

      case (Success("GET"), Success(s)) if s.startsWith("/muddy") =>
        val parts = s.stripPrefix("/muddy").split('/').toSeq.filter(_.nonEmpty)
        println(s"MUDDY: $parts")
        val page = parts match {
          case Seq(topic, info) if info.startsWith("session=") =>
            val id = info.stripPrefix("session=").takeWhile(_ != '?')
            val vote = Url.decode(
              info.dropWhile(_ != '?')
                .stripPrefix("?")
                .stripPrefix("mud=")
                .trim
                .toLowerCase
            )
            log(s"\n*** setVote($id,$topic,$vote)\n")
            setVote(id = id, topic = topic, value = vote)
            val result = Html.page(
              "Your ip address: " + inetAdress + "<br>" +
              votingForm(vote) + "<br>"  +
              showCounts(topic) + "<br> <br> <br>" +
              showDatabase
            )
            result

          case Seq(topic) => loginPage(sessionId = nextId(), topic)
          case _ => Html.page(s"MUDDY: $parts")
        }
        Html.header(page.size) + page

      case (Success("GET"), what ) =>
        val page = Html.errorResponse(what.toString, "sidan är vojd :( ")
        Html.header(page.size) + page

      case _ => " "
    }

    // parts match {
    //   case Seq(_, "muddy", topic, info) if info.startsWith("session=") =>
    //     val id = info.stripPrefix("session=").takeWhile(_ != '?')
    //     val vote = Url.decode(
    //       info.dropWhile(_ != '?')
    //         .stripPrefix("?")
    //         .stripPrefix("mud=")
    //         .trim
    //         .toLowerCase
    //     )
    //     log(s"\n*** setVote($id,$topic,$vote)\n")
    //     setVote(id = id, topic = topic, value = vote)
    //     val result = Html.page(
    //       "Your ip address: " + inetAdress + "<br>" +
    //       votingForm(vote) + "<br>"  +
    //       showCounts(topic) + "<br> <br> <br>" +
    //       showDatabase
    //     )
    //     result
    //
    //   case Seq(_, "muddy", topic) => loginPage(sessionId = nextId(), topic)
    //
    //   case _ => Html.errorResponse(uriOpt.getOrElse(""), "sidan är vojd :( ")
    // }
  }

  var n = 0

  def handleRequest(clientSocket: Socket): Unit = {
    n += 1
    println(s"${n}th $clientSocket")
    var os: OutputStream = null
    try {
      val is = clientSocket.getInputStream
      log("waiting for data on input stream")
      val scan = new Scanner(is, "UTF-8")
      val cmd = Try(scan.next)
      val url = Try(scan.next)
      log(s"""
        |---INPUT BEGIN:
        | cmd = $cmd
        | url = $url
        |---INPUT END.
      """.stripMargin)
      val output: String = response(cmd, url, clientSocket.getInetAddress.toString)
  		log(s"SENDING RESPONSE:\n$output")
      os = clientSocket.getOutputStream
      os.write(output.getBytes("UTF-8"))
    // } catch { case e: Throwable =>
    //     log(s"ERROR: handleRequest failed due to exception: $e")
    } finally {
      if (os != null) os.close
      if (clientSocket != null) clientSocket.close
    }
  }

  def start(port: Int): Unit = Concurrently {
    log(s"\nStarting Muddy server version $version on port $port...")
    val serverSocket = new ServerSocket(port)
    log(s"Server running at: http://localhost:${serverSocket.getLocalPort}")
		while (true) {
      var clientSocket: Socket = null
  		try {
        log("*** Waiting for connection...")
        clientSocket = serverSocket.accept  // blocks this thread until connect
        log(s"""
          |REQUEST:
          |  FROM: ${clientSocket.getInetAddress()}
          |  PORT: ${clientSocket.getPort()}
        """.stripMargin)

		    Concurrently {
          handleRequest(clientSocket)
          log(s"\n___Request complete.\n")
        }
  	  } catch { case e: Throwable =>
          log(s"*********** ERROR: Connection failed due to exception: $e")
      }
		}
  }

}
