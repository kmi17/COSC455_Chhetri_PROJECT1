package edu.towson.cosc.cosc455_cchhetri_project1

/**
  * Created by camychhetri on 10/11/16.
  */
class MyLexicalAnalyzer extends LexicalAnalyzer {

  val lexems: List[String] = List("\\BEGIN", "\\END", "\\TITLE[", "]", "#", "\\PARAB", "\\PARAE", "**", "*", "+", "\\\\", "[", "(", ")", "![", "\\DEF[", "=", "\\USE[")
  val singleChar: Array[Char] = Array('\\', '*', '!', '[', ']', '(', ')', '+', '#', '=')
  var index = 0
  var pos = 0
  var nextChar = ' '
  var entireFileText: Array[Char] = Array()
  var tempChar = ' '
  var textList = List()
  var possibleToken = " "
  var nextToken = " "
  val space = " "
  var fileInputSize = 0
  val singleToken: List[Char] = List('+', '=', '#', '(', ')', '[', ']')
  var isText = false

  def start(fileContents: String): Unit = {
    entireFileText = fileContents.toCharArray
    fileInputSize = entireFileText.length - 1

  }


  override def addChar(): Unit = {
    //var str = " "

    possibleToken += nextChar
    //str = possibleToken

    //} else {

    // getChar()
    // }
    //return str
  }


  override def getChar(): Char = {
    var giveChar : Char = ' '
    if (pos >= fileInputSize) {

      //nextChar = entireFileText.charAt(pos)


      //addChar(nextChar)

      // trim the char

     return '\n'

    }
    giveChar = entireFileText.charAt(pos)
    pos = pos + 1

    giveChar

  }


  override def getNextToken(): Unit = {
    // it calls everything

    //getChar()

    //nextToken = addChar(nextChar)

    // if(lookup(nextToken)) {
    // Compiler.currentToken = nextToken

    //}

    possibleToken = ""
    while ((isSpace(nextChar)) || nextChar.equals('\r') || nextChar.equals('\n') || nextChar.equals(' ') || nextChar.equals('\t')) {
      nextChar = getChar()
    }

    if (singleChar.contains(nextChar)) {
      checkSingleChar()
    } else {

      //while(isTextFound()) {
        //addChar()
       // nextChar = getChar()
      //
      // }

      textChar()


    //while(isTextFound()){
      //addChar()
      //nextChar = getChar()
   // }
    }

    //println("Lexical: " + strToken)
    //redoes the getNextToken function if the token is "\r"
    if (possibleToken == "\r") {
      getNextToken()
    }
    Compiler.currentToken = possibleToken


  }


  override def lookup(nextToken: String): Boolean = {

    /// checks if the lexemes is legal..

    //var flag = false
    for (x <- lexems) {


      if (x.equalsIgnoreCase(possibleToken)) {

        return true


      }
    }
    return false
  }


  def isTextFound(): Boolean = {

    for (alpha <- 'A' to 'Z') {
      if (nextChar.equals(alpha)) {
        isText = true
      }
    }
    for (num <- '0' to '9') {
      if (nextChar.equals(num))
        isText = true
    }
    for (alpha <- 'a' to 'z') {
      if (nextChar.equals(alpha))
        isText = true
    }
    if (nextChar.equals(',') || nextChar.equals('.') || nextChar.equals('\\') || nextChar.equals('.')
      || nextChar.equals('?') || nextChar.equals('_') || nextChar.equals('/') || nextChar.equals(' '))
      isText = true

    return isText
  }

  def isSpace(s: Char): Boolean = s == ' '

  def checkSingleChar(): Unit = {

    if (nextChar == '\\') {
      while (nextChar != '[' && nextChar != '\n' && !isSpace(nextChar) && nextChar != 0) {
        addChar()
        nextChar = getChar()
      }

      if (nextChar == '\r') {
        nextChar = getChar()

      } else if (nextChar == '\n') {
        nextChar = getChar()

      } else {
        addChar()
        nextChar = getChar()
      }
      if (possibleToken.endsWith("\r")) {
       var Strsize = possibleToken.length()
        possibleToken = possibleToken.dropRight(1)
        //possibleToken = possibleToken.last.
      }
      if (possibleToken.endsWith("\t")) {
        possibleToken = possibleToken.dropRight(1)
      }
      if (possibleToken.endsWith(" ")) {
        possibleToken = possibleToken.dropRight(1)
      }
      possibleToken = possibleToken.toUpperCase
      //println(strToken)

    } else if (nextChar == '*') {
      addChar()
      nextChar = getChar()
      if (nextChar == '*') {
        addChar()
        nextChar = getChar()
      }
    } else if (nextChar == '!') {
      addChar()
      nextChar = getChar()
      if (nextChar == '[') {
        addChar()
        nextChar = getChar()
      } else {
        print("Lexical Error:: Can't find '" + nextChar + "'")
        System.exit(1)
      }
    } else {
      addChar()
      nextChar = getChar()
    }
    if(!lookup(possibleToken)){
      print("illegal")
      print(possibleToken)
      System.exit(1)
    }else{
      //print("Pass: ")
     // print(possibleToken)
     // println()
    }


  }
  def textChar() = {
    do{

      addChar()
      nextChar = getChar()
    } while (!singleChar.contains(nextChar) && nextChar != '\n' && nextChar != '\r')
  }

}
