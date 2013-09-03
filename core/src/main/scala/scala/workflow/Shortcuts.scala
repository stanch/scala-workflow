package scala.workflow

import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

object Shortcuts extends TreeRewriter {
  trait Shortcut {
    val instanceName: String
    val annotationName: String
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val context = contextFromTerm(c)(c.typeCheck(Ident(newTermName(instanceName))))
      val transformer = rewrite(c)(_: c.Tree, context)
      rewriteCode(c)(annottees, transformer, annotationName, isContext = false)
    }
  }

  class async extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro async.impl
  }

  object async extends Shortcut {
    val instanceName = "future"
    val annotationName = "@async"
  }

  class opt extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro opt.impl
  }

  object opt extends Shortcut {
    val instanceName = "option"
    val annotationName = "@opt"
  }
}
