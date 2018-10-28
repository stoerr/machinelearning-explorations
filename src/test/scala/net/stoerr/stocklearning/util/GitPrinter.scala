package net.stoerr.stocklearning.util

import java.util.Properties

import scala.io.Source

object GitPrinter {

  val gitprops = new Properties()
  gitprops.load(getClass.getClassLoader.getResourceAsStream("git.properties"))

  val commitDescription: String = gitprops.getProperty("git.commit.id.describe")

  def printGitinfo(): Unit = {
    Source.fromFile(".git/logs/HEAD").getLines().toArray.takeRight(1).foreach(println)
    println("git: " + gitprops.getProperty("git.commit.id.describe") + " - " + gitprops.getProperty("git.commit.message.short"))
  }

}
