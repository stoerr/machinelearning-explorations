package net.stoerr.learning.learnalgorithmexplorations.util

import java.time.LocalDateTime
import java.util.Properties

import scala.io.Source

object GitPrinter {

  val gitprops = new Properties()
  try {
    gitprops.load(getClass.getClassLoader.getResourceAsStream("git.properties"))
  } catch {
    case e: Exception =>
  }

  val headinfo : String = try {
    val src = Source.fromFile(".git/logs/HEAD")
    if (src.nonEmpty) src.getLines().toArray.last else ""
  } catch {
    case e: Exception => ""
  }

  val commitDescription: String = gitprops.getProperty("git.commit.id.describe")

  def printGitinfo(): Unit = {
    print("git " + LocalDateTime.now() + " : ")
    if (gitprops.size() > 0) println(gitprops.getProperty("git.commit.id.describe") + " - " + gitprops.getProperty("git.commit.message.short"))
    if (headinfo !=  null && !headinfo.isEmpty) println(headinfo)
  }

}
