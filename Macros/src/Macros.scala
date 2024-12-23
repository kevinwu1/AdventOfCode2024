package Aoc2024.macros

import scala.annotation.MacroAnnotation
import scala.annotation.experimental
import scala.quoted.*

object Macros {
  @experimental
  class logged extends MacroAnnotation:
    def transform(using Quotes)(
        definition: quotes.reflect.Definition
    ): List[quotes.reflect.Definition] =
      import quotes.reflect.*
      definition match
        case DefDef(
              name,
              paramss,
              tpt,
              Some(rhs)
            ) => {
          val (typeParams, termParams) =
            paramss.foldLeft(List[TypeParamClause](), List[TermParamClause]()) {
              case ((tyP, trP), clause) =>
                clause match {
                  case typeClause: TypeParamClause => (typeClause :: tyP, trP)
                  case termClause: TermParamClause => (tyP, termClause :: trP)
                }
            }
          val res = tpt.tpe.asType match {
            case '[t] =>
              val (paramNames, paramExprs) =
                termParams
                  .flatMap(_.params)
                  .map(x => (x.name, Ref(x.symbol).asExpr))
                  .unzip

              val fname = name.toString
              val rhsExpr = rhs.asExprOf[t]
              given Quotes = definition.symbol.asQuotes
              val res = '{
                {
                  println(
                    indent + ${ Expr(fname) } + ${
                      Expr(paramNames)
                    }.zip(${ Expr.ofList(paramExprs) })
                      .map(p => p._1 + "=" + p._2)
                      .mkString("(", ", ", ")")
                  )
                  Aoc2024.macros.Macros.indentation += 1
                  val macro_finalanswer = $rhsExpr
                  Aoc2024.macros.Macros.indentation -= 1
                  macro_finalanswer
                }
              }.asTerm
              DefDef.copy(definition)(name, paramss, tpt, rhs = Some(res))
          }
          println("Macro applied")
          println(res.show)
          List(res)
        }

        case _ =>
          report.error(
            "Annotation only supported on `def` with a single argument are supported"
          )
          List(definition)

  import scala.collection.concurrent

  @experimental
  class memoize extends MacroAnnotation:
    def transform(using Quotes)(
        definition: quotes.reflect.Definition
    ): List[quotes.reflect.Definition] =
      import quotes.reflect._
      definition match
        case DefDef(
              name,
              paramss,
              tpt,
              Some(rhs)
            ) =>
          (tpt.tpe.asType) match
            case ('[u]) =>
              val newRhs =
                given Quotes = definition.symbol.asQuotes
                val rhsExpr = rhs.asExprOf[u]
                '{
                  val x = $rhsExpr
                  x
                }.asTerm
              val newTree = DefDef.copy(definition)(
                name,
                paramss,
                tpt,
                Some(newRhs)
              )
              List(newTree)
        case _ =>
          report.error(
            "Annotation only supported on `def` with a single argument are supported"
          )
          List(definition)
  var indentation = 0
  def indent: String = "  " * indentation
}
