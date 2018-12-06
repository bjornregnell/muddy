package muddy

import tinyserver._
import java.net.{ServerSocket, Socket}
import java.io.OutputStream
import java.util.Scanner
import scala.util.{Try, Success, Failure}

object Muddy extends TinyWebServer {
  val version = "0.0.6"

  case class Key(id: String, topic: String)
  case class Value(value: String)

  val db = new KeyValueDatabase[Key, Value]

  def loginForm(topic: String, id: String): String = s"""
  <form action="/muddy/$topic/session=$id" method="get">
    <div>
      <button class="button" type="submit">Login</button>
    </div>
  </form>
  """

  def loginPage(sessionId: String, topic: String, ip: String): String =
    Html.page(s"""
      Welcome to Muddy v$version! <br><br>
      Your ip: $ip<br><br>
      Press Login to start voting on "$topic".<br><br>
      ${loginForm(topic, sessionId)}
    """)

  val MaxLettersInTopic = 20

  def votingForm(value: String = ""): String = s"""
  <form action="" method="get">
    <div>
      <label for="mud">Your vote: </label>
      <input name="mud" id="mud" value="$value" class="inpluttadej">
      <button class="button">Update</button>
    </div>
  </form>
  """

  def setVote(id: String, topic: String, value: String): Unit =
    db.put(Key(id, topic), Value(value))

  def getVote(id: String, topic: String): String =
    db.get(Key(id, topic)).map(_.value).getOrElse("")

  def showCounts(topic: String): String = {
    val register = db.toMap.filterKeys(_.topic == topic)
      .values.collect { case Value(s) if s.nonEmpty => s }
      .groupBy(x => x)
      .collect { case (v, xs) => (v, xs.size) }.toSeq
      .sortBy(_._2).reverse
    val n = register.map(_._2).sum
    val max = Try(register.maxBy(_._2)._2).getOrElse(0)
    val winners = Try(register.filter(_._2 == max).map(_._1).mkString(", ")).getOrElse("")
    s"TOT=$n. MAX=$max: $winners.<br>" +
      register.map { case (v, n) => s"$v($n)" }.mkString(", ")
  }

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
        val page = parts.map(Url.decode) match {
          case Seq(topic, info) if info.startsWith("session=") =>
            val id = info.stripPrefix("session=").takeWhile(_ != '?')
            val vote = info
              .dropWhile(_ != '?')
              .stripPrefix("?")
              .stripPrefix("mud=")
              .toLowerCase(java.util.Locale.getDefault)
              .map(ch => if (ch == '-' | ch.isLetter) ch else ' ')
              .trim.replaceAll(" +", " ")  //only a single space
              .take(MaxLettersInTopic)     //not too long
            log(s"\n*** setVote($id,$topic,$vote)\n")
            setVote(id = id, topic = topic, value = vote)
            val result = Html.page(
              s"""Vote on Topic("$topic")""" +
              votingForm(vote) + "<br>"  +
              showCounts(topic) + "<br> <br> <br>" +
              s"""Muddy v$version from $inetAdress<br>"""
            )
            result

          case Seq(topic) => loginPage(sessionId = nextId(), topic, inetAdress)
          case _ => Html.page(s"MUDDY: $parts")
        }
        Html.header(page.size) + page

      case (Success("GET"), what ) =>
        val page = Html.errorResponse(what.toString, "sidan är vojd :( ")
        Html.header(page.size) + page

      case _ => " "
    }
  }

}
