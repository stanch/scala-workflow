package scala.workflow

import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

object Annotations {
  class async extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro asyncImpl
  }

  def asyncImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    workflowMacro(c)(c.typeCheck(q"future"), annottees)
  }

  class opt extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro optImpl
  }

  def optImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    workflowMacro(c)(c.typeCheck(q"option"), annottees)
  }
}
