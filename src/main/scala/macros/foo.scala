package org.hablapps.azucar.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.compileTimeOnly

@compileTimeOnly("enable macro paradise to expand macro annotations")
class foo extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro FooMacros.impl
}

class FooMacros(val c: Context) {
  import c.universe._
  
  def go(typeclass: ClassDef, companion: Option[ModuleDef] = None) = {

    val tparam: TypeDef = typeclass.tparams.head

    val tparamName = tparam.name

    val q"trait Foo[$outParam]" = q"trait Foo[X]"

    val tparamsTail = typeclass.tparams.tail

    val tparamsTailApplied = tparamsTail.map(td => Ident(td.name))

    val tmembers: List[TypeDef] =
      typeclass.impl.children.collect {
        case t: TypeDef => t
      }

    import scala.reflect.runtime.universe.Flag._
    val typeclassMethods = typeclass.impl.children.collect {
      case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) &&
                        !m.mods.hasFlag(Flag.PROTECTED) => m
    } filter {
      case DefDef(_, TermName("$init$"), _, _, _, _) => false
      case DefDef(Modifiers(DEFERRED, _, _), _, _, _, _, _) => true
      case DefDef(Modifiers(_, _, List(Apply(Select(New(Ident(TypeName("derived"))), _), _))), _, _, _, _, _) => true
      case _ => false
    }

    val typeclassValues = typeclass.impl.children.collect {
      case o: ValDef if !o.mods.hasFlag(Flag.PRIVATE) &&
                        !o.mods.hasFlag(Flag.PROTECTED) => o
    }

    val typeclassOptics = typeclassValues filter { valdef =>
      valdef.tpt match {
        case appliedTypeTree @ AppliedTypeTree(_, Ident(`tparamName`) :: _) =>
          !(appliedTypeTree exists {
            case Ident(TypeName("Monad")) => true
            case Select(_, TypeName("Monad")) => true
            case _ => false
          })
        case _ => false
      }
    }

    // Detects `optic` annotation
    // val typeclassOptics2 = typeclassValues filter {
    //   case v: ValDef =>
    //     v.mods.annotations match {
    //       case Apply(Select(New(Ident(TypeName("optic"))), _), _) :: rest => true
    //       case other => false
    //     }
    //   case _ => false
    // }

    def generateADT = {
      val valueCases = typeclassOptics map { optic =>
        val AppliedTypeTree(Ident(TypeName(opticName)), _ :: opticParams) = optic.tpt
        q"""
          case class ${capitalize(optic.name.toTypeName, TypeName(_))}[..${outParam :: tparamsTail}](
              internal: ${Ident(TermName(opticName))}.Σ[..${Ident(TypeName("X")) :: opticParams}])
            extends Σ[..$tparamsTail]
        """
      }
      val methodCases = typeclassMethods.map { method =>
        val AppliedTypeTree(_, List(arg)) = method.tpt
        q"""
          case class ${capitalize(method.name.toTypeName, TypeName(_))}[..${method.tparams ::: tparamsTail}](
              ..${method.vparamss.flatten})
            extends Σ[..${arg :: tparamsTailApplied}]
        """
      }
      q"sealed abstract class Σ[..${outParam :: tparamsTail}] { ..$tmembers }" :: valueCases ::: methodCases
    }

    def generateISO = {

      val q"class IsoClass extends $foo" =
        q"class IsoClass extends Iso[({type λ[α[_]] = ${typeclass.name}[α, ..${tparamsTailApplied}]})#λ]"

      // q"""val iso = new IsoClass
      //     class IsoClass extends Iso[${typeclass.name}] {
      //       type ADT[X] = Σ[X]
      //       type Ev[P[_]] = Monad[P]

      //       def to[P[_]](fp: ${typeclass.name}[P]): ADT ~> P = ???
      //       def from[P[_]: Ev](gp: ADT ~> P): ${typeclass.name}[P] = ???
      //     }"""

      q"""def iso[..$tparamsTail] = new IsoClass[..${tparamsTailApplied}]
          class IsoClass[..$tparamsTail] extends $foo {
            type ADT[X] = Σ[X, ..${tparamsTailApplied}]
            type Ev[P[_]] = Monad[P]

            def to[P[_]](fp: ${typeclass.name}[P, ..${tparamsTailApplied}]): ADT ~> P = ???
            def from[P[_]: Ev](gp: ADT ~> P): ${typeclass.name}[P, ..${tparamsTailApplied}] = ???
          }"""

    }

    q"""
      object ${typeclass.name.toTermName} {
        ..${companion.fold(List.empty[Tree])(_.impl.body.tail)}
        ..$generateADT
        ..$generateISO
      }"""

  }

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {

    annottees.map(_.tree) match {
      case (typeclass: ClassDef) :: Nil if typeclass.tparams.size > 0 =>
        val res = c.Expr(q"""
          $typeclass
          ..${go(typeclass)}""")
        println(s"RES:\n$res")
        res
      case (typeclass: ClassDef) :: (companion: ModuleDef) :: Nil if typeclass.tparams.size > 0 =>
        val res = c.Expr(q"""
          $typeclass
          ..${go(typeclass, Option(companion))}""")
        println(s"RES:\n$res")
        res
      case _ =>
        abort("@foo can't be applied here")
    }

  }

  def trace(s: => String) =
    c.info(c.enclosingPosition, s, false)

  def abort(s: => String) =
    c.abort(c.enclosingPosition, s)

  def capitalize[N <: Name](name: N, builder: String => N): N =
    builder(name match {
      case TermName(s) => s.capitalize
      case TypeName(s) => s.capitalize
    })

}
