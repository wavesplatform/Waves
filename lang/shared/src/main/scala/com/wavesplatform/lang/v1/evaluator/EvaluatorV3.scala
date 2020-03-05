package com.wavesplatform.lang.v1.evaluator

import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment

object EvaluatorV3 extends App {

  def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[FUNC] = {
    parentBlocks match {
      case (l: LET_BLOCK) :: _                         => ???
      case (b @ BLOCK(f @ FUNC(`name`, _, _), _)) :: _ => Some(f)
      case _ :: xs                                     => findUserFunction(name, xs)
      case Nil                                         => None
    }
  }

  def visitRef(key: String, update: EVALUATED => Unit, limit: Int, parentBlocks: List[BLOCK_DEF]): Int =
    parentBlocks match {
      case (l: LET_BLOCK) :: _ => ???
      case (b @ BLOCK(l @ LET(`key`, _), _)) :: nextParentBlocks =>
        val unused = root(
          expr = l.value,
          update = {
            case ev: EVALUATED =>
              l.value = ev
              update(ev)
            case nonEvaluated =>
              l.value = nonEvaluated
          },
          limit = limit,
          parentBlocks = nextParentBlocks
        )
        if (unused < 0) throw new Error("Unused < 0")
        else if (l.value.isInstanceOf[EVALUATED]) {
          if (unused == 0) {
            println(s"Stopping because ref not evaluated: $unused, ")
            0
          } else {
            println(s"REF $key: reducing unused to ${unused - 1}")
            unused - 1
          }
        } else unused
      case _ :: nextParentBlocks => visitRef(key, update, limit, nextParentBlocks)
    }

  def root(expr: EXPR, update: EXPR => Unit, limit: Int, parentBlocks: List[BLOCK_DEF]): Int = {
    println(s"Visiting $expr")
    val dc = expr.deepCopy()
    val r= expr match {
      case b: BLOCK_DEF =>
        root(
          expr = b.body,
          update = {
            case ev: EVALUATED => update(ev)
            case nonEvaluated  => b.body = nonEvaluated
          },
          limit = limit,
          parentBlocks = b :: parentBlocks
        )
      case g: GETTER =>
        val unused = root(
          expr = g.expr,
          update = {
            case co: CaseObj   => update(co.fields(g.field))
            case ev: EVALUATED => throw new IllegalArgumentException(s"GETTER of non-case-object $ev")
            case nonEvaluated  => g.expr = nonEvaluated
          },
          limit = limit,
          parentBlocks = parentBlocks
        )
        if (unused < 0) throw new Error("Unused < 0")
        else if (unused == 0) {
          println(s"Stopping because getter not evaluated: $unused")
          0
        } else unused - 1
      case i: IF =>
        val unused = root(
          expr = i.cond,
          update = i.cond = _,
          limit = limit,
          parentBlocks = parentBlocks
        )
        if (unused < 0) throw new Error("Unused < 0")
        i.cond match {
          case TRUE =>
            if (unused == 0) 0
            else {
              val u2 = root(
                expr = i.ifTrue,
                update = update,
                limit = unused,
                parentBlocks = parentBlocks
              )
              if (u2 < 0) throw new Error("Unused < 0")
              else if (u2 == 0) {
                println(s"Stopping because true branch not evaluated: $u2")
                0
              } else u2 - 1
            }
          case FALSE =>
            if (unused == 0) 0
            else {
              val u2 = root(
                expr = i.ifFalse,
                update = update,
                limit = unused,
                parentBlocks = parentBlocks
              )
              if (u2 < 0) throw new Error("Unused < 0")
              else if (u2 == 0) {
                println(s"Stopping because false branch not evaluated: $u2")
                0
              } else u2 - 1
            }
          case e: EVALUATED => throw new IllegalArgumentException("Non-boolean result in cond")
          case nonEvaluated => {
            println(s"Stopping because condition not evaluated: $unused")
            0
          }
        }

      case REF(key) => visitRef(key, update, limit, parentBlocks)
      case fc: FUNCTION_CALL =>
        val unusedArgsEval = fc.args.indices.foldLeft(limit) {
          case (unused, argIndex) =>
            if (unused == 0) {
              println(s"Not evaluating next args: $unused")
              0
            }
            if (unused < 0) throw new Error("Unused < 0")
            root(
              expr = fc.args(argIndex),
              update = argValue => fc.args = fc.args.updated(argIndex, argValue),
              limit = unused,
              parentBlocks
            )
        }
        if (fc.args.forall(_.isInstanceOf[EVALUATED])) {
          fc.function match {
            case FunctionHeader.Native(_) =>
              val cost = 5
              if (unusedArgsEval < cost) {
                println(s"Stopping because not enough limit to evaluate function: $unusedArgsEval")
                0
              } else {
                update(CONST_LONG(fc.args(0).asInstanceOf[CONST_LONG].t + fc.args(1).asInstanceOf[CONST_LONG].t))
                println(s"FUNCTION CALL: reducing unused to ${unusedArgsEval - cost}")
                unusedArgsEval - cost
              }
            case FunctionHeader.User(_, name) =>
              findUserFunction(name, parentBlocks) match {
                case None => throw new IllegalArgumentException(s"Function $name not found")
                case Some(signature) =>
                  val argsWithExpr =
                    (signature.args zip fc.args)
                      .foldRight(signature.body.deepCopy()) {
                        case ((argName, argValue), argsWithExpr) =>
                          BLOCK(LET(argName, argValue), argsWithExpr)
                      }
                  update(argsWithExpr)
                  root(argsWithExpr, update, unusedArgsEval, parentBlocks)
              }
          }
        } else {
          println(s"Stopping because not all args evaluated, unused: $unusedArgsEval")
          0
        }
      case evaluated: EVALUATED =>
        update(evaluated)
        limit
    }
    println(s"Finished visiting $dc, result: $expr, Consumed: ${limit-r}")
    r
  }


  def expr() =
    BLOCK(
      LET("a", FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
      BLOCK(
        LET("b", REF("a")),
        BLOCK(
          FUNC(
            "g",
            Nil,
            BLOCK(
              LET(
                "a",
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.SUM_LONG),
                  List(
                    FUNCTION_CALL(
                      FunctionHeader.Native(FunctionIds.SUM_LONG),
                      List(CONST_LONG(2), CONST_LONG(2))
                    ),
                    CONST_LONG(2)
                  )
                )
              ),
              BLOCK(
                LET("c", REF("a")),
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.SUM_LONG),
                  List(
                    REF("c"),
                    FUNCTION_CALL(
                      FunctionHeader.Native(FunctionIds.SUM_LONG),
                      List(REF("b"), REF("a"))
                    )
                  )
                )
              )
            )
          ),
          FUNCTION_CALL(
            FunctionHeader.Native(FunctionIds.SUM_LONG),
            List(FUNCTION_CALL(FunctionHeader.User("g"), Nil), REF("a"))
          )
        )
      )
    )
  def buildSum(a: EXPR, b: EXPR) = FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(a, b))

  def expr2() = buildSum(
    buildSum(
      buildSum(CONST_LONG(1), CONST_LONG(2)),
      buildSum(CONST_LONG(3), CONST_LONG(4))
    ),
    CONST_LONG(5)
  )

  def expr3() = buildSum(
    buildSum(
      BLOCK(LET("a",buildSum(CONST_LONG(1), CONST_LONG(2))), buildSum(REF("a"),CONST_LONG(6))),
      buildSum(CONST_LONG(3), CONST_LONG(4))
    ),
    CONST_LONG(5)
  )

  def expr4() =
    BLOCK(
      LET("a", CONST_LONG(2)),
      BLOCK(
        LET("a", CONST_LONG(6)),
        REF("a")))

  def expr5() =
    BLOCK(
      LET("a", buildSum(CONST_LONG(1), CONST_LONG(1))),
      BLOCK(
        LET("b", REF("a")),
        buildSum(
        BLOCK(
          LET("a", CONST_LONG(6)),
          BLOCK(
            LET("c", CONST_LONG(6)),
            buildSum(CONST_LONG(6), buildSum(REF("b"), CONST_LONG(6)))
          )
        ), REF("a"))
      )
    )

  import cats.implicits._
  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val ctx =
    PureContext.build(Global, V3).withEnvironment[Environment] |+|
    WavesContext.build(DirectiveSet.contractDirectiveSet)

  Range(0, 100).foreach { c =>
    var e: EXPR = expr5()
    println(s"==== limit: $c ====")
    println("unused cost: " + root(e, e = _, c, List.empty))
    println(Decompiler(e, ctx.decompilerContext))
    println()

  }
}
