package scala

import language.experimental.macros
import language.higherKinds
import scala.annotation.StaticAnnotation
import reflect.macros.{TypecheckException, Context}
import util.{Failure, Success}
import scala.reflect.internal.annotations.compileTimeOnly

package object workflow extends TreeRewriter with FunctorInstances with SemiIdiomInstances with IdiomInstances with MonadInstances {
  /*def context[F[_]](code: _): _ = macro contextImpl
  def contextImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    c.macroApplication.updateAttachment(contextFromType(c)(typeTree))

    code
  }

  object context {
    def apply(workflow: Any)(code: _): _ = macro contextImpl
    def contextImpl(c: Context)(workflow: c.Expr[Any])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = workflow

      c.macroApplication.updateAttachment(contextFromTerm(c)(instance))

      code
    }
  }*/

  /*def workflow[F[_]](code: _): _ = macro workflowImpl
  def workflowImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    val workflowContext = contextFromType(c)(typeTree)

    rewrite(c)(code, workflowContext).asInstanceOf[Tree]
  }*/

  @compileTimeOnly("Please annotate the enclosing method with `@enableWorkflow`")
  def workflow(wf: Any)(code: Any): Any = ???
  @compileTimeOnly("Please annotate the enclosing method with `@enableWorkflow`")
  def workflow(code: Any): Any = ???

//  class enableWorkflow extends StaticAnnotation {
//    def macroTransform(annottees: Any*) = macro enableWorkflowImpl
//  }
//
//  def contextImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
//    import c.universe._
//
//    val q"${_}.${nme.CONSTRUCTOR}($instance)" = c.prefix.tree
//    val workflowContext = contextFromTerm(c)(instance)
//  }

  class workflow(wf: Any) extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro workflowImpl
  }

  def workflowImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val q"${_}.${nme.CONSTRUCTOR}($instance)" = c.prefix.tree
    workflowMacro(c)(c.typeCheck(instance), annottees)
  }

  def workflowMacro(c: Context)(instance: c.Tree, annottees: Seq[c.Expr[Any]]): c.Expr[Any] = {
    import c.universe._

    val workflowContext = contextFromTerm(c)(instance)

    util.Try {
      val Seq(Expr(ValDef(mods, name, typetree, code))) = annottees
      c.Expr[Any](Block(ValDef(mods, name, typetree, rewrite(c)(code, workflowContext).asInstanceOf[Tree]), Literal(Constant(()))))
    } getOrElse {
      c.abort(c.enclosingPosition, "Workflow is not allowed here. Use @workflow(...) val foo = ...")
    }
  }

  /*object workflow {
    def apply(workflow: Any)(code: _): _ = macro workflowImpl
    def workflowImpl(c: Context)(workflow: c.Expr[Any])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = workflow

      val workflowContext = contextFromTerm(c)(instance)

      rewrite(c)(code, workflowContext).asInstanceOf[Tree]
    }
  }*/

  /*def $[F[_]](code: _): _ = macro $impl
  def $impl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree: TypeTree)), _) = c.macroApplication

    val workflowContext = if (typeTree.original != null)
                            contextFromType(c)(typeTree)
                          else
                            contextFromEnclosure(c)

    rewrite(c)(code, workflowContext).asInstanceOf[Tree]
  }*/

  private def contextFromType(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val tpe = typeTree.tpe

    val typeRef = TypeRef(NoPrefix, typeOf[Workflow[Any]].typeSymbol, List(tpe))
    val instance = c.inferImplicitValue(typeRef)

    if (instance == EmptyTree)
      c.abort(typeTree.pos, s"Unable to find $typeRef instance in implicit scope")

    WorkflowContext(tpe, instance)
  }

  private def contextFromTerm(c: Context)(instance: c.Tree): WorkflowContext = {
    import c.universe._

    val workflowSymbol = instance.tpe.baseClasses find (_.fullName == "scala.workflow.Workflow") getOrElse {
      c.abort(instance.pos, "Not a workflow instance")
    }

    val TypeRef(_, _, List(tpe)) = instance.tpe.baseType(workflowSymbol)

    WorkflowContext(tpe, instance)
  }

  private def contextFromEnclosure(c: Context) = {
    val workflowContext = for {
      context ← c.openMacros.view
      attachments = context.macroApplication.attachments
      workflowContext ← attachments.get[WorkflowContext]
    } yield workflowContext

    workflowContext.headOption getOrElse {
      c.abort(c.enclosingPosition, "Workflow brackets outside of `context' block")
    }
  }
}