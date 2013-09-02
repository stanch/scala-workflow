package scala.workflow

import scala.reflect.macros.{TypecheckException, Context}
import scala.util.{Failure, Success}

trait TreeRewriter {
  protected case class WorkflowContext(tpe: Any, instance: Any)

  protected def rewrite(c: Context)(code: c.Tree, workflowContext: WorkflowContext): c.Tree = {
    import c.universe._

    val WorkflowContext(workflow: Type, instance: Tree) = workflowContext

    val interfaces = instance.tpe.baseClasses map (_.fullName)
    def assertImplements(interface: String) {
      if (!interfaces.contains(interface))
        c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement $interface")
    }

    def resolveLiftedType(tpe: Type): Option[Type] =
      tpe.baseType(workflow.typeSymbol) match {
        case baseType @ TypeRef(_, _, typeArgs) ⇒
          workflow match {
            case PolyType(List(wildcard), typeRef: TypeRef) ⇒
              // When workflow is passed as type lambda, we need to take the type
              // from wildcard position, so we zip through both typerefs to seek for a substitution
              def findSubstitution(wildcardedType: Type, concreteType: Type): Option[Type] = {
                if (wildcardedType.typeSymbol == wildcard)
                  Some(concreteType)
                else
                  (wildcardedType, concreteType) match {
                    case (wctpe: TypeRef, ctpe: TypeRef) ⇒
                      wctpe.args zip ctpe.args find {
                        case (wct, at) ⇒ !(wct =:= at)
                      } flatMap {
                        case (wct, at) ⇒ findSubstitution(wct, at)
                      }
                    case _ ⇒ None
                  }
              }
              findSubstitution(typeRef, baseType)
            case _ ⇒
              // This only works for type constructor of one argument
              // TODO: provide implementation for n-arity type constructors
              typeArgs.headOption
          }
        case _ ⇒ None
      }

    case class Bind(name: TermName, tpt: TypeTree, value: Tree) {
      def isUsedIn(frame: Frame) = frame exists ((_: Bind).value exists (_ equalsStructure q"$name"))
    }
    type Frame = List[Bind]
    class Scope(val materialized: List[Frame], val frames: List[Frame]) {
      def materialize(bind: Bind) = new Scope(materialized.init :+ (materialized.last :+ bind), frames)
      def :+ (bind: Bind) = new Scope(materialized, (bind :: frames.head) :: frames.tail)
      def ++ (binds: List[Bind]) = new Scope(materialized, (frames.head ++ binds) :: frames.tail)
      def enter = new Scope(materialized :+ Nil, Nil :: frames)
      def leave = new Scope(materialized.init, frames.tail)
      def local = materialized.flatten ++ frames.flatten
      def wrapping(tree: Tree) = local.foldLeft(tree) {
        case (expr, bind) ⇒ q"{ val ${bind.name}: ${bind.tpt} = ???; $expr }"
      }
    }
    object Scope {
      val empty = new Scope(List(Nil), List(Nil))
      def merge(scopes: List[Scope]) = {
        val materialized = scopes map (_.materialized)
        val frames = scopes map (_.frames)
        new Scope(mergeFrames(materialized), mergeFrames(frames))
      }
      def merge(scopes: Scope*): Scope = merge(scopes.toList)
      private def mergeFrames(frames: List[List[Frame]]) = frames.map(_.head).flatten.distinct :: frames.head.tail
    }

    def printval[A](x: A): A = { println(x); x }

    def typeCheck(tree: Tree, scope: Scope): util.Try[Tree] = /*printval*/ {
      util.Try(c.typeCheck(scope wrapping tree.duplicate)) recoverWith {
        case e: TypecheckException
          if e.msg.contains("follow this method with `_'") || e.msg.contains("ambiguous reference") ||
            (e.msg.contains("package") && e.msg.contains("is not a value")) ⇒ Success(EmptyTree)
        case e: TypecheckException
          if e.msg.contains("missing arguments for constructor") ⇒
          util.Try(c.typeCheck(scope wrapping q"(${tree.duplicate})(_)")) recover {
            case e: TypecheckException
              if !e.msg.contains("too many arguments for constructor") ⇒ EmptyTree
          }
      }
    }

    /* This whole function stinks. It's long, unreliable and some looks redundant.
     * TODO: Obviously need to refactor it at some point */
    def rewriteBlock(scope: Scope): Block ⇒ (Scope, Tree) = {
      case Block(stats, result) ⇒
        def contExpr(scope: Scope, signature: Option[(Modifiers, TermName, Tree)], expr: Tree) = {
          val (newscope, newexpr) = rewrite(scope.enter)(expr)
          val frame = newscope.materialized.last
          if (frame.isEmpty) {
            val name = signature map { case (_, name, _) ⇒ name } getOrElse newTermName("_")
            val tpe = typeCheck(newexpr, newscope).get.tpe
            val bind = Bind(name, TypeTree(tpe), newexpr)
            val cont = (_: Boolean) ⇒ signature map {
              case (mods, _, tpt) ⇒ block(ValDef(mods, name, tpt, newexpr))
            } getOrElse block(q"$newexpr")

            val newerscope = if (signature.isDefined) scope :+ bind else scope

            (newerscope ++ newscope.frames.head, cont)
          } else {
            val value = apply(frame)(newexpr)
            val tpe = typeCheck(newexpr, newscope).get.tpe
            val name = signature map { case (_, term, _) ⇒ term } getOrElse newTermName("_")
            val bind = Bind(name, TypeTree(tpe), value)
            val cont = (x: Boolean) ⇒ if (x) >>=(bind) compose lambda(bind) // Especially dirty hack!
            else   map(bind) compose lambda(bind) // TODO: figure out a better way

            val newerscope = if (signature.isDefined) scope :+ bind else scope

            (newerscope ++ newscope.frames.head, cont)
          }
        }

        def contRewrite(scope: Scope): Tree ⇒ (Scope, Boolean ⇒ Tree ⇒ Tree) = {
          case ValDef(mods, name, tpt, expr) ⇒
            contExpr(scope, Some((mods, name, tpt)), expr)

          case expr ⇒
            contExpr(scope, None, expr)
        }

        val (newscope, cont) = stats.foldLeft((scope, (x: Boolean) ⇒ (t: Tree) ⇒ t)) {
          (acc, stat) ⇒
            val (scope, cont) = acc
            val (newscope, newcont) = contRewrite(scope)(stat)
            (newscope, (x: Boolean) ⇒ cont(true) compose newcont(x))
        }

        val (newerscope, newresult) = rewrite(newscope.enter)(result)
        val frame = newerscope.materialized.last
        val value = if (frame.isEmpty) cont(false)(newresult) else cont(true)(apply(frame)(newresult))
        (newerscope.leave, value)
    }

    //    def rewriteIf(scope: Scope): If ⇒ (Scope, Tree) = {
    //      case If(condition, consequent, alternative) if alternative != Literal(Constant(())) ⇒
    //        val (newscope, newcondition) = rewrite(scope)(condition)
    //        val (consscope, newconsequent) = rewrite(newscope)(consequent)
    //        val (altscope, newalternative) = rewrite(newscope)(alternative)
    //        (Scope.merge(consscope, altscope), If(newcondition, newconsequent, newalternative))
    //      case expr ⇒
    //        c.abort(expr.pos, "`if` expressions with missing alternative are not supported")
    //    }

    def rewrite(scope: Scope): Tree ⇒ (Scope, Tree) = {
      case Apply(fun, args) ⇒
        val (funscope,   newfun)  = rewrite(scope)(fun)
        val (argsscopes, newargs) = args.map(rewrite(funscope)).unzip
        extractBinds(Scope.merge(argsscopes), q"$newfun(..$newargs)")

      case Select(value, method) ⇒
        val (newscope, newvalue) = rewrite(scope)(value)
        extractBinds(newscope, q"$newvalue.$method")

      case block: Block ⇒ rewriteBlock(scope)(block)

      //      case condition: If ⇒ rewriteIf(scope)(condition)

      case expr @ (_ : Literal | _ : Ident | _ : New) ⇒ extractBinds(scope, expr)

      case expr ⇒
        c.abort(expr.pos, "Unsupported expression " + showRaw(expr))
    }

    def extractBinds(scope: Scope, expr: Tree) =
      typeCheck(expr, scope) match {
        case Success(tpt) ⇒
          resolveLiftedType(tpt.tpe) match {
            case Some(tpe) ⇒
              val name = newTermName(c.fresh("arg$"))
              val bind = Bind(name, TypeTree(tpe), expr)
              (scope materialize bind, q"$name")

            case None ⇒ (scope, expr)
          }
        case Failure(e) ⇒
          val binds = scope.materialized.flatten map {
            case Bind(name, _, value) ⇒ s"   $name = $value"
          }
          val message = s"Type error during rewriting of expression within $workflow context"
          val bindsList = if (binds.isEmpty) "" else s" where${binds mkString ("\n\n", "\n", "\n\n")}"
          c.abort(c.enclosingPosition, s"${e.getMessage}\n\n   $expr\n\n$bindsList$message")
      }

    def lambda(bind: Bind): Tree ⇒ Tree = {
      expr ⇒ q"(${bind.name}: ${bind.tpt}) ⇒ $expr"
    }

    def isIdentity: Tree ⇒ Boolean = {
      case Function(List(arg), Ident(ref)) if arg.name == ref ⇒ true
      case _ ⇒ false
    }

    def block(tree: Tree): Tree ⇒ Tree = {
      expr ⇒ q"{ $tree; $expr }"
    }

    def point: Tree ⇒ Tree = {
      assertImplements("scala.workflow.Pointing")
      expr ⇒ q"$instance.point($expr)"
    }

    def map(bind: Bind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Mapping")
      expr ⇒ if (isIdentity(expr)) bind.value else q"$instance.map($expr)(${bind.value})"
    }

    def app(bind: Bind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Applying")
      expr ⇒ q"$instance.app($expr)(${bind.value})"
    }

    def >>=(bind: Bind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Binding")
      expr ⇒ q"$instance.bind($expr)(${bind.value})"
    }

    def apply: Frame ⇒ Tree ⇒ Tree = {
      case Nil ⇒ point
      case bind :: Nil ⇒ map(bind) compose lambda(bind)
      case bind :: binds ⇒
        if (bind isUsedIn binds)
          >>=(bind) compose lambda(bind) compose apply(binds)
        else
          app(bind) compose apply(binds) compose lambda(bind)
    }

    /* Blocks and expressions are rewritten a bit differently
     * (block don't have materialized binds afterwards), hence two branches */
    code match {
      case block: Block ⇒
        val (_, expr) = rewrite(Scope.empty)(block)
        expr
      case _ ⇒
        val (scope, expr) = rewrite(Scope.empty)(code)
        apply(scope.materialized.flatten)(expr)
    }
  }
}
