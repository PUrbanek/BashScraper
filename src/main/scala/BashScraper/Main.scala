import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

import com.typesafe.config._

import java.io._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.collection.mutable.ListBuffer

package BashScraper {}

object Main extends App {
  if (args.length == 0) {
    println("No parameter given")
  }
  val numberOfPages = 3//args(0).toInt
  val browser = JsoupBrowser()
//  val numberOfPages = 5
  var postCounter = 0
  var postParseTimes = ListBuffer[Long]()
  var pageParseTimes = ListBuffer[Long]()
  var output = ListBuffer[JObject]()
  val conf = ConfigFactory.load()
  assert(numberOfPages <= getHighestPageAvailable(), "Requested number of pages is too large")
  assert(numberOfPages >= 0, "Requested number of pages is too small")

  List.range(1, numberOfPages+1, 1).foreach(x => {
    val address = s"http://bash.org.pl/latest/?page=$x"
    val page = browser.get(address)
    pageParseTimes += time(parsePage(page))
  })

  println(s"Number of parsed posts: $postCounter")
  println(s"Average parsing time for posts: ${postParseTimes.foldLeft(0.0){_ + _} / postParseTimes.length / 100 / 100} milliseconds")
  println(s"Average parsing time for pages: ${pageParseTimes.foldLeft(0.0){_ + _} / pageParseTimes.length / 100 / 100} milliseconds")

  val pw = new PrintWriter(new File(conf.getString("output-file.file-path")))

  output.foreach(post => {
    pw.write(prettyRender(post))
    pw.write("\n")
  })
  pw.close()

  def getHighestPageAvailable(): Int = {
    val pagesAvailable = browser.get("http://bash.org.pl/latest/") >> elementList(".page")
    pagesAvailable.maxBy(_.text).text.toInt
  }

  def parsePage(document: Document): Unit = {
    val posts = document >> elementList(".q.post")
    for (post <- posts) {
      postParseTimes += time(parsePost(post))
    }
  }

  def parsePost(post: Element): Unit = {
    val id = post.attr("id").substring(1)
    val score = (post >> element(".points")).text
    val content = (post >> element(".quote")).text
    val json = ("id" -> id) ~ ("score" -> score) ~ ("content" -> content)
    postCounter += 1
    output += json
  }

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    t1-t0
  }
}
