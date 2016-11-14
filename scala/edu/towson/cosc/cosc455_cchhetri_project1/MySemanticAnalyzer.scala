package edu.towson.cosc.cosc455_cchhetri_project1
import scala.collection.mutable
import scala.collection.mutable.Stack

import java.awt.Desktop
import java.io.{File, IOException}
/**
  * Created by camychhetri on 10/23/16.
  */


class MySemanticAnalyzer {
  //var Myparser = Stack[String]()

  val parseTree = Compiler.Parser.parseTree
  var stackThem = Stack[String]()
  var displayTree: List[String] = Nil
  var nextToken: String = ""


  var Count: Int = 0
  var isPrinted: Boolean = false

  def startSymantics(): Unit = {

    nextToken = parseTree.pop()

    begin()
  }


  def begin() {
    while (!parseTree.isEmpty) {

      if (nextToken.equalsIgnoreCase(CONSTANTS.DOCB)) {

        stackThem.push("<html>")
        print("<html>")
        nextToken = parseTree.pop()

      }

      else if (nextToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        stackThem.push("<head>")
        print("<head>")
        stackThem.push("<title>")
        print("<title>")
        stackThem.push(parseTree.pop())
        print(parseTree.pop())
        stackThem.push("</title>")
        print("</title>")
        stackThem.push("</head>")
        print("</head>")
        parseTree.pop()
        nextToken = parseTree.pop()
      }

      else if (nextToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        stackThem.push("<h1>")

      }


    }






















    def openHTMLFileInBrowser(htmlFileStr: String) = {
      val file: File = new File(htmlFileStr.trim)
      println(file.getAbsolutePath)
      if (!file.exists())
        sys.error("File " + htmlFileStr + " does not exist.")

      try {
        Desktop.getDesktop.browse(file.toURI)
      }
      catch {
        case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
        case e: Exception => sys.error("He's dead, Jim!")
      }
    }
  }
}




