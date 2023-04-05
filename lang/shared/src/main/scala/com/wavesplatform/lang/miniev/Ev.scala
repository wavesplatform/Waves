package com.wavesplatform.lang.miniev

import cats.Id
import cats.syntax.either.*
import cats.syntax.flatMap.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, ExtendedInternalFunction, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, Log}
import com.wavesplatform.lang.{CommonError, EvaluationException, ExecutionError}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.NonFatal

object Ev {
  case class Closure(func: FUNC, scope: Scope)

  case class Scope(userFns: Map[String, Closure], names: Map[String, LazyVal])

  @tailrec
  private final def collectEvaluated(argsToProcess: List[EXPR], evaluatedArgs: List[EVALUATED]): (List[EVALUATED], List[EXPR]) =
    if (argsToProcess.isEmpty) (evaluatedArgs, Nil)
    else if (!argsToProcess.head.isInstanceOf[EVALUATED]) (evaluatedArgs, argsToProcess)
    else collectEvaluated(argsToProcess.tail, argsToProcess.head.asInstanceOf[EVALUATED] :: evaluatedArgs)

  private final def addArgs(argNames: List[String], argValues: List[EVALUATED], target: Map[String, LazyVal]) =
    target.concat(
      argNames.view.zip(argValues.view.map(ev => new LazyVal(Right(ev))))
    )

  @tailrec
  private def evalRoot(root: EXPR, state: State): Either[ExecutionError, EVALUATED] = {
    root match {
      case f: FAIL => Right(f)
      case GETTER(expr, field) =>
        state.recordComplexityOverhead() match {
          case Left(value)  => value.asLeft
          case Right(total) =>
//            println(s"GET $field: $total")
            evalRoot(expr, state.push(Op.Get(field)))
        }
      case LET_BLOCK(let, body) => evalRoot(body, state.addName(let.name, let.value))
      case BLOCK(dec, body) =>
        dec match {
          case LET(name, value) => evalRoot(body, state.addName(name, value))
          case f: FUNC          => evalRoot(body, state.addUserFunction(f))
          case FAILED_DEC       => Left(CommonError("Encountered failed declaration"))
        }
      case IF(cond, ifTrue, ifFalse) =>
        state.recordComplexityOverhead() match {
          case Left(value)  => value.asLeft
          case Right(total) =>
//            println(s"IF: $total")
            evalRoot(cond, state.push(Op.If(ifTrue, ifFalse)))
        }
      case REF(key) =>
        state.recordComplexityOverhead() match {
          case Left(value)  => value.asLeft
          case Right(total) =>
//            println(s"REF($key): $total")
            state.cachingEv(key) match {
              case Some(value) =>
                value.value match {
                  case Right(ev) => evalRoot(ev, state)
                  case Left((expr, exprScope)) =>
                    evalRoot(expr, state.push(Op.Value(key, value, state)).resetScope(exprScope))
                }
              case None =>
                state.evaluationContext.letDefs.get(key) match {
                  case None =>
                    Left(CommonError(s"A definition of '$key' not found"))
                  case Some(value) =>
                    value.value.value match {
                      case ee @ Left(_) => ee
                      case Right(v)     => evalRoot(v, state)
                    }
                }
            }
        }
      case FUNCTION_CALL(fh, args) =>
        val (reversedEvaluatedArgs, nonEvArgs) = collectEvaluated(args, Nil)
        if (nonEvArgs.nonEmpty) {
          evalRoot(nonEvArgs.head, state.push(Op.FuncArg(fh, reversedEvaluatedArgs, nonEvArgs.tail)))
        } else {
          val evaluatedArgs = args.asInstanceOf[List[EVALUATED]]
          fh match {
            case fh: FunctionHeader.Native =>
              state.evaluationContext.functions.get(fh) match {
                case Some(nf @ NativeFunction(_, _, _, ev, _)) =>
                  val callResult: Either[ExecutionError, EVALUATED] = ev match {
                    case simple: ContextfulNativeFunction.Simple =>
                      simple.evaluate(state.evaluationContext.environment, evaluatedArgs)
                  }
                  callResult match {
                    case l @ Left(_) => l
                    case Right(value) =>
                      state.spendComplexity(nf.costByLibVersion(state.stdlibVersion)) match {
                        case Left(e)      => e.asLeft
                        case Right(total) =>
//                          println(s"CALL ${evaluatedArgs.mkString(nf.funcName + "(", ",", ")")} => $value: $total")
                          evalRoot(value, state)
                      }

                  }
                case Some(eif: ExtendedInternalFunction) =>
                  eif.buildExpression(state, evaluatedArgs) match {
                    case Left(value) => value.asLeft[EVALUATED]
                    case Right(expr) => evalRoot(expr, state)
                  }
                case _ => Left(CommonError(s"function '$fh' not found"))
              }
            case FunctionHeader.User(internalName, _) =>
              state.evaluationContext.functions.get(fh) match {
                case Some(uf @ UserFunction(_, _, _, _, ev, args)) =>
                  val expr: EXPR = ev.apply[Id](state.evaluationContext.environment, evaluatedArgs)
                  val newNames =
                    args.view
                      .zip(evaluatedArgs)
                      .map { case (name, arg) =>
                        name -> new LazyVal(Right(arg))
                      }
                      .toMap
                  val predefinedComplexity = uf.costByLibVersion(state.stdlibVersion)
//                  if (state.newMode) {
//                    state.spendComplexity(predefinedComplexity)
//                  }
//                  println(s"USER($internalName), predefined=${predefinedComplexity} [newMode=${state.newMode}]: ${state.spentComplexity()}")
                  evalRoot(
                    expr,
                    state.callUserFunction(internalName, Scope(Map.empty, newNames), Some(predefinedComplexity))
                  )
                case None =>
                  state.currentScope().userFns.get(internalName) match {
                    case Some(scopedUserFn) =>
//                      println(s">> CALL '${scopedUserFn.func.name}'")
                      evalRoot(
                        scopedUserFn.func.body,
                        state.callUserFunction(
                          scopedUserFn.func.name,
                          scopedUserFn.scope.copy(names = addArgs(scopedUserFn.func.args, evaluatedArgs, scopedUserFn.scope.names)),
                          None
                        )
                      )

                    case None =>
                      state.evaluationContext.typeDefs.get(internalName) match {
                        case None => Left(CommonError(s"Function or type '$internalName' not found"))
                        case Some(ctr @ CASETYPEREF(_, fields, hideConstructor)) =>
                          if (state.newMode && hideConstructor) {
                            Left(CommonError(s"Constructor '$internalName' is not available"))
                          } else {
                            val constructorCost = if (state.newMode) 1 else 0
                            state.spendComplexity(constructorCost) match {
                              case Left(value)  => value.asLeft
                              case Right(total) =>
//                                println(s"$internalName: spent=$constructorCost; newMode=${state.newMode} total $total")
                                evalRoot(CaseObj(ctr, fields.map(_._1).zip(evaluatedArgs).toMap), state)
                            }
                          }
                        case other => Left(CommonError(s"Could not find constructor for type $other"))
                      }
                  }
                case other => Left(CommonError(s"Unexpected call to $other"))
              }
          }
        }

      case evaluated: EVALUATED =>
        state.pop() match {
          case None =>
//            println(s"END: $evaluated")
            Right(evaluated)

          case Some(op) =>
            op match {
              case fc: Op.FuncArg =>
                val newReversedEvaluatedArgs = evaluated :: fc.reversedEvaluatedArgs
                if (fc.argsToEvaluate.nonEmpty) {
                  evalRoot(
                    fc.argsToEvaluate.head,
                    state.push(fc.copy(reversedEvaluatedArgs = newReversedEvaluatedArgs, argsToEvaluate = fc.argsToEvaluate.tail))
                  )
                } else {
                  evalRoot(FUNCTION_CALL(fc.func, newReversedEvaluatedArgs.reverse), state)
                }
              case ps @ Op.Func(name, scope, maybePredefinedComplexity) =>
                val spentComplexity = state.popComplexity()
                val adjustedComplexity = maybePredefinedComplexity match {
                  case Some(pc) if state.newMode => pc
                  case _                         => if (state.newMode) spentComplexity.max(1L) else spentComplexity
                }

                state.spendComplexity(adjustedComplexity) match {
                  case Left(err)    => err.asLeft
                  case Right(total) =>
//                    println(s"$name: predeifined=$maybePredefinedComplexity, spent=$spentComplexity, adjusted=$adjustedComplexity, $total")
                    ps.ret(evaluated) match {
                      case (Left(err), _) => err.asLeft
                      case (Right(expr), maybeScope) =>
                        evalRoot(expr, maybeScope.fold(state)(s => state.resetScope(s)))
                    }
//                    println(s"POP($name, $maybePredefinedComplexity): ${state.totalSpentComplexity()}")

                }
              case op =>
//                println(s"OP> $evaluated: $op")
                op.ret(evaluated) match {
                  case (Left(err), _) => err.asLeft
                  case (Right(expr), maybeScope) =>
                    evalRoot(expr, maybeScope.fold(state)(s => state.resetScope(s)))
                }
            }
        }
      case FAILED_EXPR => Left(CommonError("Unexpected FAILED_EXPR"))
    }
  }

  def run(
      script: EXPR,
      ec: EvaluationContext[Id],
      complexityLimit: ComplexityLimit,
      newMode: Boolean,
      version: StdLibVersion
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    run(script, State(ec, complexityLimit, newMode, version))

  def run(script: EXPR, state: State): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = try {
    val resultE = evalRoot(script, state)
    val intComplexityE = state
      .totalSpentComplexity()
      .flatMap(comp => Try(Math.toIntExact(comp)).toEither.leftMap(_ => CommonError("Complexity overflow")))

//    println(s"\n\tComplexity: $intComplexityE, result: $resultE")

    (state.logEntries.toList, intComplexityE.getOrElse(Int.MaxValue), resultE.flatTap(_ => intComplexityE))
  } catch {
    case NonFatal(e) =>
      e.printStackTrace()
      (Nil, 0, Left(EvaluationException(e)))
  }
}
