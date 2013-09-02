package scala

import language.experimental.macros
import language.higherKinds
import scala.annotation.StaticAnnotation
import scala.reflect.macros.{Universe, TypecheckException, Context}
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

  /* Get WorkflowContext from annotation context’s prefix */
  private def contextFromAnnotation(c: Context) = {
    import c.universe._

    val q"${_}.${nme.CONSTRUCTOR}($instance)" = c.prefix.tree
    contextFromTerm(c)(c.typeCheck(instance))
  }

  /* Extract code from the definition and apply `rewrite` to it. If extraction fails, abort with `msg` */
  private def rewriteCode(c: Context)(annottees: Seq[c.Expr[Any]], rewrite: c.Tree ⇒ c.Tree, msg: String) = {
    import c.universe._

    // use a continuation approach not to put `rewrite` inside util.Try
    val (code, cont) = util.Try {
      val Seq(Expr(DefDef(mods, name, tparams, vparamss, typetree, code))) = annottees
      code → { x: Tree ⇒
        c.Expr[Any](Block(DefDef(mods, name, tparams, vparamss, typetree, x), Literal(Constant(()))))
      }
    } orElse util.Try {
      val Seq(Expr(ValDef(mods, name, typetree, code))) = annottees
      code → { x: Tree ⇒
        c.Expr[Any](Block(ValDef(mods, name, typetree, x), Literal(Constant(()))))
      }
    } getOrElse {
      c.abort(c.enclosingPosition, msg)
    }

    cont(rewrite(code))
  }

  /** Stub to be replaced by @workflowContext */
  @compileTimeOnly("Please annotate the enclosing definition with `@workflowContext`")
  def $(code: Any): Any = ???

  /** Annotation to enable workflow brackets (`$`) */
  class workflowContext(wf: Any) extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro workflowContextImpl
  }

  def workflowContextImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    // get context
    val context = contextFromAnnotation(c)

    // transform the code by replacing all stubs with rewritten trees
    val transformer = { code: Tree ⇒
      val t = new Transformer {
        val WorkflowTerm = newTermName("$")
        override def transform(tree: Tree): Tree = tree match {
          case Apply(Ident(WorkflowTerm), x :: Nil) ⇒ rewrite(c)(x, context)
          case _ ⇒ super.transform(tree)
        }
      }
      t.transform(code)
    }

    // return the original definition with the transformed code
    rewriteCode(c)(annottees, transformer, "@workflowContext should annotate a definition of either value, method or function")
  }

  class workflow(wf: Any) extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro workflowImpl
  }

  def workflowImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val context = contextFromAnnotation(c)
    val transformer = rewrite(c)(_: c.Tree, context)
    rewriteCode(c)(annottees, transformer, "@workflow should annotate a definition of either value, method or function")
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