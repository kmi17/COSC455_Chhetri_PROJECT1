  package edu.towson.cosc.cosc455_cchhetri_project1

  /**
    * Created by camychhetri on 10/11/16.
    */
  class MySyntaxAnalyzer extends SyntaxAnalyzer {

    var parseTree = new scala.collection.mutable.Stack[String]
    val symbolChar: Array[Char] = Array('\\', '*', '!', '[', ']', '(', ')', '+', '#', '=')
    var isText = false
    var isInnerItem = false


    override def gittex(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {

        parseTree.push(CONSTANTS.DOCB)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        variableDefine()
        title()
        body()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
          // do stuff
          parseTree.push(CONSTANTS.DOCE)
          print(Compiler.currentToken)

          //Compiler.SemanticAnalyzer.startSymantics()
        } else {
          println("Syntax Error BECAUSE NO DOCE")
          System.exit(1)

        }
      } else {
        println("Syntax Error BECAUSE NO DOCB")
        System.exit(1)
      }
    }


    override def title(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        parseTree.push(CONSTANTS.TITLEB)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        //reqtext()

        if (textFound(Compiler.currentToken)) {

          text()

          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            parseTree.push(CONSTANTS.BRACKETE)
            print(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

          } else {
            println("Syntax Error: Missing Closing Bracket")
            System.exit(1)

          }
        } else {
          println("Syntax Error: Text Required")
          System.exit(1)
        }
      } else {
        println("Syntax Error: Title Missing")
        System.exit(1)
      }
    }


    override def body(): Unit = {
      /// if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {

      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {


      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {

      //} else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {


      //} else{
      //text()
      innerText()

      paragraph()
      newline()


      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        Compiler.Scanner.getNextToken()
        print(Compiler.currentToken)
        body()
      }
    }


    //}


    override def paragraph(): Unit = {

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        print(Compiler.currentToken)
        parseTree.push(CONSTANTS.PARAB)
        print("this is parab")

        Compiler.Scanner.getNextToken()

        variableDefine()
        innerText()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
          print(Compiler.currentToken + " this is paraE")
          parseTree.push(CONSTANTS.PARAE)

        } else {
          println("Syntax Error: Missing Paragraph end tag")
          System.exit(1)
        }
      }

    }

    override def innerText(): Unit = {


      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        variableUse()
        innerText()

      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        heading()
        innerText()
        //or
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        bold()
        innerText()
        //or
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        italics()
        innerText()
        //or
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        listItem()
        innerText()
        //or
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {

        image()
        innerText()
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        //or
        link()
        innerText()
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        //or
        newline()
        innerText()

      } else {
        if (textFound(Compiler.currentToken)) {
          text()
          innerText()
        }
      }
      /* println("Syntax Error")
      System.exit(1)*/

    }

    override def heading(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        parseTree.push(CONSTANTS.HEADING)
        print(Compiler.currentToken + "head")
        Compiler.Scanner.getNextToken()

        //reqtext()
        if (textFound(Compiler.currentToken)) {
          text()


        } else {
          print("Syntax error")
          System.exit(1)
        }
      }
    }


    override def variableDefine(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        parseTree.push(CONSTANTS.DEFB)
        print(Compiler.currentToken + "variableDef")
        Compiler.Scanner.getNextToken()

        //reqtext()
        if (textFound(Compiler.currentToken)) {
          text()
          print("in var def")
        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
          parseTree.push(CONSTANTS.EQSIGN)
          print(Compiler.currentToken + "variabledef eq")
          Compiler.Scanner.getNextToken()
          //reqtext()

        } else {
          println("Syntax Error: Required equal sign here")
          System.exit(1)
        }

        if (textFound(Compiler.currentToken)) {
          text()
          print("in var def after equal")
        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(CONSTANTS.BRACKETE)
          print(Compiler.currentToken + "var def brack")
          Compiler.Scanner.getNextToken()

          variableDefine()


        } else {
          println("Syntax Error: NO Ending Bracket")
          System.exit(1)
        }
      }

    }

    override def variableUse(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        parseTree.push(CONSTANTS.USEB)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        //reqtext()
        if (textFound(Compiler.currentToken)) {
          //text()
          text()
          print("in var use")
        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(CONSTANTS.BRACKETE)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          println("Syntax Error: Bracket ending is required")
          System.exit(1)
        }

      }

    }

    override def bold(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        parseTree.push(CONSTANTS.BOLD)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        // /text()
        if (textFound(Compiler.currentToken)) {
          text()
          print("in bold")
        }

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
          parseTree.push(CONSTANTS.BOLD)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          println("Syntax Error: Bold Ending Tag Not Here")
          System.exit(1)
        }
      }

    }


    override def italics(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        parseTree.push(CONSTANTS.ITALICS)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        // add it  so that u can build a parse tree

        //text()
        if (textFound(Compiler.currentToken)) {
          text()
          print("in italics")
        }

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
          parseTree.push(CONSTANTS.ITALICS)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          println("Syntax Error: Italics Ending tag not found")
          System.exit(1)
        }
      }

    }


    override def listItem(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        parseTree.push(CONSTANTS.LISTITEM)
        print(Compiler.currentToken)
        print(" i am in list")
        Compiler.Scanner.getNextToken()

        innerItem()
        listItem()

      }
    }

    override def innerItem(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        isInnerItem = true
        variableUse()
        innerItem()
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        //or
        isInnerItem = true
        bold()
        innerItem()
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        //or
        isInnerItem = true
        italics()
        innerItem()
      } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        //or
        isInnerItem = true
        link()
        innerItem()
      } else {
        if (isInnerItem == false) {
          //reqtext()
          if (textFound(Compiler.currentToken)) {
            text()
            print("in inner item")
          } else {
            print("Syntax Error: text is required here")
            System.exit(1)
          }
        }

        //innerItem()
      }
      /*println("Syntax Error")
      System.exit(1)*/
    }

    override def link(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        parseTree.push(CONSTANTS.LINKB)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        // if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {
        // parseTree.push(CONSTANTS.REQTEXT)


        if (textFound(Compiler.currentToken)) {
          text()
          print("in link")
        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }
        //Compiler.Scanner.getNextToken()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(CONSTANTS.BRACKETE)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          print("Syntax Error: Ending Bracket Not Found")
          System.exit(1)
        }

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(CONSTANTS.ADDRESSB)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          // if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {
          // parseTree.push(CONSTANTS.REQTEXT)
          //variableDefine()
          //Compiler.Scanner.getNextToken()
        } else {
          print("Syntax Error: Address Not Found")
          System.exit(1)
        }

        if (textFound(Compiler.currentToken)) {
          text()
          print("after link address b")
        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }


        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
          parseTree.push(CONSTANTS.ADDRESSE)
          print(Compiler.currentToken)
          //variableDefine()
          Compiler.Scanner.getNextToken()


          if (textFound(Compiler.currentToken)) {
            text()
            print("in  address end")
          } else {
            print("Syntax Error: text is required here")
            System.exit(1)
          }
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(CONSTANTS.ADDRESSE)
            print(Compiler.currentToken)
            //variableDefine()
            Compiler.Scanner.getNextToken()
          }
        } else {
          println("Syntex Error: Ending LInk not found")
          System.exit(1)
        }
      }
    }


    // } else {
    //println("Syntax Error")
    //System.exit(1)
    //}
    //} else {
    //println("Syntax Error")
    //System.exit(1)
    // }

    // } else {
    // println("Syntax Error")
    // System.exit(1)
    //}
    //} else {
    // println("Syntax Error")
    //System.exit(1)
    //}
    //} else {
    // println("Syntax Error")
    // System.exit(1)
    //}

    // } else {
    //println("Syntax Error")
    //System.exit(1)


    override def image(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
        parseTree.push(CONSTANTS.IMAGEB)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (textFound(Compiler.currentToken)) {
          text()

        } else {
          print("Syntax Error: text is required here")
          System.exit(1)
        }


        //if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {
        // parseTree.push(CONSTANTS.REQTEXT)
        //Compiler.Scanner.getNextToken()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(CONSTANTS.BRACKETE)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          print("Syntax Error: BracketE is required here")
          System.exit(1)
        }

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(CONSTANTS.ADDRESSB)
          print(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (textFound(Compiler.currentToken)) {
            text()
            print("in address b")

          }

          // if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {
          // parseTree.push(CONSTANTS.REQTEXT)
          //variableDefine()
          //Compiler.Scanner.getNextToken()

          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(CONSTANTS.ADDRESSE)
            print(Compiler.currentToken)
            //variableDefine()
            Compiler.Scanner.getNextToken()
          } else {
            println("Syntax Error: AddressE is required here")
            System.exit(1)
          }

        } else {
          println("Syntax Error: AddressB is required here")
          System.exit(1)
        }
      } else {
        println("Syntax Error: Image Begin Tag is required here")
        System.exit(1)
      }
    }


    /*
               // } else {
                  println("Syntax Error")
                  System.exit(1)
               // }
             // } else {
                println("Syntax Error")
                System.exit(1)
             // }

            } else {
              println("Syntax Error")
              System.exit(1)
            }
          } else {
            println("Syntax Error")
            System.exit(1)
          }
        } else {
          println("Syntax Error")
          System.exit(1)
        }

      } else {
        println("Syntax Error")
        System.exit(1)
      }
    }
*/
    override def newline(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        parseTree.push(CONSTANTS.NEWLINE)
        print(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
    }


    def text(): Unit = {

      //alphabetsTXT()
      //numbersTXT()
      //strExtraTXT()

        parseTree.push(Compiler.currentToken)
      print(Compiler.currentToken + "i am text")
      Compiler.Scanner.getNextToken()

  }


/*
    def reqtext(): Unit = {
      alphabets()
      numbers()
      strExtraText()
    }


    def alphabets(): Unit = {
      for (alphabets <- 'a' to 'z') {

        if (Compiler.currentToken.equals(alphabets)) {
          val charToStr = alphabets.toString
          parseTree.push(charToStr)
        } else {
          println("Syntax Error")
          System.exit(1)
        }
      }
    }

    def numbers(): Unit = {
      for (numbers <- 0 to 9) {
        val intToStr = numbers.toString
        if (Compiler.currentToken.equals(intToStr)) {
          val charToStr = intToStr.toString
          parseTree.push(intToStr)
        }
      }
    }

    def strExtraText(): Unit = {
      val strList: List[String] = List(",", ".", "?", "_", "/", " \" ")
      for (i <- 0 until strList.length - 1)
        if (Compiler.currentToken.equals(strList {i})) {
          parseTree.push(strList {i})
        } else {
          println("Syntax Error")
          System.exit(1)
        }
    }


    def alphabetsTXT(): Unit = {
      for (alphabets <- 'a' to 'z') {

        if (Compiler.currentToken.equals(alphabets)) {
          val charToStr = alphabets.toString
          parseTree.push(charToStr)

        }
      }
    }

    def numbersTXT(): Unit = {
      for (numbers <- 0 to 9) {
        val intToStr = numbers.toString
        if (Compiler.currentToken.equals(intToStr)) {
          val charToStr = intToStr.toString
          parseTree.push(intToStr)
        }
      }
    }

    def strExtraTXT(): Unit = {
      val strList: List[String] = List(",", ".", "?", "_", "/", " \" ")
      for (i <- 0 until strList.length - 1)
        if (Compiler.currentToken.equals(strList {i})) {
          parseTree.push(strList {i})

        }
    }*/

      def textFound(myStr: String): Boolean ={
        for(sC <- symbolChar){
          if(myStr.contains(sC))
            return false
        }
        true

      }

  }






