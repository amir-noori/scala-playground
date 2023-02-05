package code.playground

import scala.reflect.macros.blackbox

object SyntaxTree {

  //  val tree = q"i am { a quasiquote }"
  //  println(tree match { case q"i  am  { a  quasiquote } " => "it worked!" case _ => "not working" })


  object Logger {
    def debug(message: String): Unit = println(message)

    var enabled = false
  }

  object LogMacro {
    import scala.language.experimental.macros

    var enabled = false

    def debug(message: String): Unit = macro impl

    def impl(c: blackbox.Context)(message: c.Expr[String]): c.universe.Tree = {
      import c.universe._
      q"""
         if(LogMacro.enabled) {
            Logger.log($message)
         }
       """
    }
  }

}
