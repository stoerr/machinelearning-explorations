package net.stoerr.stocklearning.util

import java.util.Properties

import scala.io.Source

object GitPrinter {

  val gitprops = new Properties()
  try {
    gitprops.load(getClass.getClassLoader.getResourceAsStream("git.properties"))
  } catch {
    case e: Exception => println(e)
  }

  val commitDescription: String = gitprops.getProperty("git.commit.id.describe")

  def printGitinfo(): Unit = {
    println("git: " + gitprops.getProperty("git.commit.id.describe") + " - " + gitprops.getProperty("git.commit.message.short"))
    try {
      val src = Source.fromFile(".git/logs/HEAD")
      if (src.nonEmpty) src.getLines().toArray.takeRight(1).foreach(println)
    } catch {
      case e: Exception => println(e)
    }
  }

}
