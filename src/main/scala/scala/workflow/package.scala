package scala

import language.experimental.macros
import language.higherKinds
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.reflect.internal.annotations.compileTimeOnly

package object workflow extends TreeRewriter with FunctorInstances with SemiIdiomInstances with IdiomInstances with MonadInstances {
  /** Workflow brackets */
  @compileTimeOnly("Please annotate the enclosing definition with `@context`")
  def $(code: Any): Any = ???

  /** Annotation to enable workflow brackets (`$`) */
  class context[F[_]](wf: Workflow[F] = null) extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro contextImpl
  }

  def contextImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
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
    rewriteCode(c)(annottees, transformer, "@context", isContext = true)
  }

  /** Workflow annotation */
  class workflow[F[_]](wf: Workflow[F] = null) extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro workflowImpl
  }

  def workflowImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val context = contextFromAnnotation(c)
    val transformer = rewrite(c)(_: c.Tree, context)
    rewriteCode(c)(annottees, transformer, "@workflow", isContext = false)
  }

  /* Extract code from the definition and apply `rewrite` to it. If extraction fails, abort with `msg` */
  private[workflow] def rewriteCode(c: Context)(annottees: Seq[c.Expr[Any]], rewrite: c.Tree ⇒ c.Tree, annotationName: String, isContext: Boolean) = {
    import c.universe._

    annottees match {
      case Seq(Expr(DefDef(mods, name, tparams, vparamss, typetree, code))) ⇒
        c.Expr[Any](Block(DefDef(mods, name, tparams, vparamss, typetree, rewrite(code)), c.literalUnit.tree))

      case Seq(Expr(ValDef(mods, name, typetree, code))) ⇒
        c.Expr[Any](Block(ValDef(mods, name, typetree, rewrite(code)), c.literalUnit.tree))

      case Seq(Expr(ModuleDef(mods, name, Template(parents, self, code)))) if isContext ⇒
        val pieces = code map {
          case d @ DefDef(_, _, _, _, _, _) ⇒ d
          case x ⇒ rewrite(x)
        }
        c.Expr[Any](Block(ModuleDef(mods, name, Template(parents, self, pieces)), c.literalUnit.tree))

      case _ ⇒
        val moduleAllowed = if (isContext) " module," else ""
        c.abort(c.enclosingPosition, s"$annotationName should annotate a definition of either$moduleAllowed value, method or function")
    }
  }

  /* Get WorkflowContext from annotation context’s prefix */
  private def contextFromAnnotation(c: Context) = {
    import c.universe._

    c.prefix.tree match {
      case q"new ${_}[$t]" ⇒ contextFromType(c)(t)
      case q"new ${_}($t)" ⇒ contextFromTerm(c)(t)
      case _ ⇒ c.abort(c.enclosingPosition, s"Couldn't get workflow instance from ${showRaw(c.prefix.tree)}")
    }
  }

  /* Get WorkflowContext from type argument */
  private[workflow] def contextFromType(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val termTree = c.typeCheck(q"new workflow[$typeTree](null)")
    val Apply(Select(New(appliedTree: TypeTree), _), _) = termTree
    val AppliedTypeTree(_, List(checkedTypeTree)) = appliedTree.original
    val tpe = checkedTypeTree.tpe

    val typeRef = TypeRef(NoPrefix, typeOf[Workflow[Any]].typeSymbol, List(tpe))
    val instance = c.inferImplicitValue(typeRef)

    if (instance == EmptyTree)
      c.abort(typeTree.pos, s"Unable to find $typeRef instance in implicit scope")

    WorkflowContext(tpe, instance)
  }

  /* Get WorkflowContext from workflow instance */
  private[workflow] def contextFromTerm(c: Context)(instance: c.Tree): WorkflowContext = {
    import c.universe._

    val checkedInstance = c.typeCheck(instance)
    val workflowSymbol = checkedInstance.tpe.baseClasses find (_.fullName == "scala.workflow.Workflow") getOrElse {
      c.abort(checkedInstance.pos, "Not a workflow instance")
    }

    val TypeRef(_, _, List(tpe)) = checkedInstance.tpe.baseType(workflowSymbol)

    WorkflowContext(tpe, checkedInstance)
  }
}