package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContextfulNativeFunction.{Extended, Simple}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.*
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.LogKeys.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.traits.Environment
import monix.eval.Coeval

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

class EvaluatorV2(
    val ctx: LoggedEvaluationContext[Environment, Id],
    val stdLibVersion: StdLibVersion,
    val limit: Int,
    val correctFunctionCallScope: Boolean,
    val newMode: Boolean,
    val enableExecutionLog: Boolean,
    val checkConstructorArgsTypes: Boolean,
    val fixedThrownError: Boolean
) {
  private val overheadCost: Int = if (newMode) 0 else 1

  /** Evaluates `expr` in place, as far as `limit` allows. Returns the expression the evaluation left in its place (a value when it completed)
    * and the unused complexity, or the first error with the complexity left when it happened. A thrown non-fatal exception other than one from
    * a native function fails the returned `Coeval`.
    *
    * An `Extended` native function (`invoke` and friends) is not run here: the loop stops, Monix runs its `Coeval` and the loop resumes with the
    * result. An invoke evaluates the callee in a nested evaluation, so running it inside the loop would grow the call stack with every level of
    * an invoke chain; this way the whole chain runs in Monix's stack-safe loop.
    */
  def run(expr: EXPR): Coeval[Either[(ExecutionError, Int), (EXPR, Int)]] = {
    val root      = new RootSlot(expr)
    val execution = new Run(expr, root)
    def drive(): Coeval[Either[(ExecutionError, Int), (EXPR, Int)]] =
      Coeval.defer {
        execution.continue() match {
          case null =>
            execution.suspension.attempt.flatMap { outcome =>
              execution.resumeWith(outcome); drive()
            }
          case result => Coeval.now(result.map((root.value, _)))
        }
      }
    drive()
  }

  // One evaluation: the continuations of the evaluated subexpressions are frames on an explicit stack, see docs/notes/ride-evaluator.md.
  private final class Run(start: EXPR, root: Slot) {
    private val frames = new java.util.ArrayDeque[Frame]()

    // The next expression to evaluate; null when the last step finished with `unused` or `error`.
    private var expr: EXPR              = null
    private var slot: Slot              = null
    private var available: Int          = 0
    private var blocks: List[BLOCK_DEF] = Nil

    // Names of the LETs this run has entered. Every scope chain is made of entered blocks, so a name not here is in none of them and a
    // reference to it is a global variable: skipping the walk saves the longest walks, which are those for `nil`, `unit` or `this`.
    private val enteredLets = new java.util.HashSet[String]()

    private var unused: Int                  = 0
    private var error: (ExecutionError, Int) = null

    // An Extended native call waiting for its Coeval to run, between continue() returning null and resume().
    private var suspended: SuspendedCall = null

    evaluate(start, root, limit, Nil)

    def suspension: Coeval[(Either[ExecutionError, EVALUATED], Int)] = suspended.coeval

    // Runs until the evaluation finishes, or returns null when it stops at an Extended native call.
    def continue(): Either[(ExecutionError, Int), Int] = {
      var result: Either[(ExecutionError, Int), Int] = null
      while (result == null && suspended == null) {
        if (expr != null) step()
        else
          frames.poll() match {
            case null                         => result = if (error != null) Left(error) else Right(unused)
            case f: LetFrame if error != null => ctx.log(f.let, Left(error._1))
            case _ if error != null           =>
            case f                            => resume(f)
          }
      }
      result
    }

    // Completes the Extended native call the loop stopped at; continue() goes on from there.
    def resumeWith(outcome: Either[Throwable, (Either[ExecutionError, EVALUATED], Int)]): Unit = {
      val call = suspended
      suspended = null
      completeNativeCall(call.slot, nativeResult(call.function, outcome.fold(throw _, identity), limit, fixedThrownError))
    }

    private def evaluate(e: EXPR, s: Slot, l: Int, b: List[BLOCK_DEF]): Unit = {
      expr = e
      slot = s
      available = l
      blocks = b
    }

    private def done(u: Int): Unit = {
      expr = null
      unused = u
    }

    private def fail(message: String, l: Int): Unit = fail(CommonError(message), l)

    private def fail(e: ExecutionError, l: Int): Unit = {
      expr = null
      error = (e, l)
    }

    @tailrec
    private def write(s: Slot, value: EXPR): Unit = s match {
      case b: BlockSlot =>
        value match {
          case ev: EVALUATED => write(b.parent, ev)
          case _             => b.block.body = value
        }
      case a: ArgSlot    => a.fc.args = a.fc.args.updated(a.index, value)
      case g: GetterSlot => g.getter.expr = value
      case i: IfSlot     => i.ifExpr.cond = value
      case l: LetSlot =>
        l.let.value = value
        value match {
          case e: EVALUATED => ctx.log(l.let, Right(e))
          case _            =>
        }
      case r: RootSlot => r.value = value
    }

    private def step(): Unit = {
      val s = slot
      val l = available
      val b = blocks
      expr match {
        case block: BLOCK_DEF =>
          block.dec match {
            case let: LET => enteredLets.add(let.name)
            case _        =>
          }
          evaluate(block.body, BlockSlot(block, s), l, block :: b)
        case g: GETTER =>
          frames.push(GetterFrame(g, s))
          evaluate(g.expr, GetterSlot(g), l, b)
        case i: IF =>
          frames.push(IfFrame(i, s, b))
          evaluate(i.cond, IfSlot(i), l, b)
        case REF(key) if !enteredLets.contains(key) => evaluateGlobalVar(key, s, l)
        case REF(key) =>
          findLet(key, b) match {
            // findLet stops only at a block declaring a LET.
            case letBlock :: next => evaluateRef(letBlock.dec.asInstanceOf[LET], s, l, next)
            case Nil              => evaluateGlobalVar(key, s, l)
          }
        case fc: FUNCTION_CALL => continueCall(new CallFrame(fc, fc.args, s, b), l)
        case evaluated: EVALUATED =>
          write(s, evaluated)
          done(l)
        case f: FAILED_EXPR => fail(s"Unexpected $f", l)
      }
    }

    private def resume(frame: Frame): Unit = frame match {
      case f: CallFrame => continueCall(f, unused)
      case f: GetterFrame =>
        f.getter.expr match {
          case co: CaseObj if unused > 0 =>
            write(f.slot, co.fields(f.getter.field))
            done(unused - overheadCost)
          case _: CaseObj    => done(unused)
          case ev: EVALUATED => fail(s"GETTER of non-case-object $ev with field '${f.getter.field}", unused)
          case _             => done(unused)
        }
      case f: IfFrame =>
        f.ifExpr.cond match {
          case TRUE | FALSE if unused <= 0 => done(unused)
          case TRUE                        => evaluateBranch(f.ifExpr.ifTrue, f)
          case FALSE                       => evaluateBranch(f.ifExpr.ifFalse, f)
          case _: EVALUATED                => fail("Non-boolean result in cond", unused)
          case _                           => done(unused)
        }
      case f: LetFrame =>
        f.let.value match {
          case ev: EVALUATED if unused > 0 =>
            write(f.slot, ev)
            done(unused - overheadCost)
          case _ => done(unused)
        }
      case f: NewModeFrame =>
        if (unused == f.limit) {
          if (unused >= 1 && f.fc.function.isInstanceOf[FunctionHeader.User]) {
            ctx.log(LET(s"${f.fc.function.funcName}.$Complexity", TRUE), CONST_LONG(1).asRight[ExecutionError])
            ctx.log(LET(ComplexityLimit, TRUE), CONST_LONG(unused - 1).asRight[ExecutionError])
          }
          done(unused - 1)
        } else done(unused)
      case f: PrecalculatedFrame => done(f.unused)
    }

    private def evaluateBranch(branch: EXPR, f: IfFrame): Unit = {
      // Only the branch taken is copied: a `match` compiles to a chain of IFs, most of whose branches are never evaluated.
      val b = if (f.ifExpr.branchesShared) EvaluationCopy(branch) else branch
      write(f.slot, b)
      evaluate(b, f.slot, unused - overheadCost, f.blocks)
    }

    // Evaluates the arguments left to right while complexity remains, then the function itself.
    private def continueCall(f: CallFrame, u: Int): Unit =
      if (f.nextArg < f.argCount && u > 0) {
        val index = f.nextArg
        f.nextArg += 1
        frames.push(f)
        evaluate(f.fc.args(index), ArgSlot(f.fc, index), u, f.blocks)
      } else if (u > 0 && f.fc.args.forall(_.isInstanceOf[EVALUATED])) {
        logFunc(f.fc, ctx, stdLibVersion, u, enableExecutionLog)
        evaluateFunction(f, u)
      } else done(u)

    private def evaluateFunction(f: CallFrame, l: Int): Unit = {
      if (newMode) frames.push(NewModeFrame(f.fc, l))
      f.fc.function match {
        case FunctionHeader.Native(_) => evaluateNativeFunction(f.fc, f.slot, l)
        case FunctionHeader.User(_, name) =>
          findCallee(f, l, name) match {
            case Some((func, precalculatedLimit, scope)) => callUserFunction(f, l, func, precalculatedLimit, scope)
            case None                                    => evaluateConstructor(f.fc, f.slot, l, name)
          }
      }
    }

    private def evaluateNativeFunction(fc: FUNCTION_CALL, s: Slot, l: Int): Unit =
      ctx.ec.functions.get(fc.function) match {
        case None => fail(s"function '${fc.function}' not found", l)
        case Some(function) =>
          val cost = function.costByLibVersion(stdLibVersion).toInt
          if (l < cost) done(l)
          else {
            val native = function.asInstanceOf[NativeFunction[Environment]]
            val args   = fc.args.asInstanceOf[List[EVALUATED]]
            native.ev match {
              case f: Extended[Environment] =>
                // Deferred, so that what evaluate throws fails the Coeval and is handled like what running it throws.
                val coeval = Coeval
                  .defer(f.evaluate[Id](ctx.ec.environment, args, l - cost))
                  .map { case (result, unusedComplexity) =>
                    result.map { case (evaluated, log) =>
                      log.foreach { case (logItemName, logItemValue) => ctx.log(LET(logItemName, TRUE), logItemValue) }
                      evaluated
                    } -> unusedComplexity
                  }
                suspended = SuspendedCall(native, s, coeval)
              case f: Simple[Environment] =>
                completeNativeCall(s, nativeResult(native, (f.evaluate(ctx.ec.environment, args), l - cost), limit, fixedThrownError))
            }
          }
      }

    private def completeNativeCall(s: Slot, result: Either[(ExecutionError, Int), (EVALUATED, Int)]): Unit =
      result match {
        case Right((value, u)) =>
          write(s, value)
          done(u)
        case Left((e, u)) => fail(e, u)
      }

    // A function of the context, or one declared in the enclosing blocks, with its precalculated limit and the blocks it sees.
    private def findCallee(f: CallFrame, l: Int, name: String): Option[(FUNC, Option[Int], List[BLOCK_DEF])] =
      ctx.ec.functions
        .get(f.fc.function)
        .map(_.asInstanceOf[UserFunction[Environment]])
        .map { uf =>
          val func = FUNC(uf.name, uf.argList, uf.ev[Id](ctx.ec.environment, f.startArgs))
          val precalculatedLimit =
            if (newMode) Some(l - uf.costByLibVersion(stdLibVersion).toInt)
            else None
          (func, precalculatedLimit, f.blocks)
        }
        .orElse(findUserFunction(name, f.blocks).map { case (func, scope) => (func, None, scope) })

    // Replaces the call with a copy of the function body wrapped in blocks with the arguments, and evaluates it.
    private def callUserFunction(f: CallFrame, l: Int, func: FUNC, precalculatedLimit: Option[Int], functionScopeBlocks: List[BLOCK_DEF]): Unit = {
      val argsWithExpr =
        (func.args zip f.fc.args)
          .foldRight(EvaluationCopy(func.body)) { case ((argName, argValue), argsWithExpr) =>
            BLOCK(LET(argName, argValue), argsWithExpr)
          }
      write(f.slot, argsWithExpr)
      precalculatedLimit.foreach(p => frames.push(PrecalculatedFrame(p)))
      evaluate(argsWithExpr, f.slot, precalculatedLimit.getOrElse(l), if (correctFunctionCallScope) functionScopeBlocks else f.blocks)
    }

    private def evaluateConstructor(fc: FUNCTION_CALL, s: Slot, l: Int, name: String): Unit =
      ctx.ec.typeDefs.get(name) match {
        case Some(objectType: CASETYPEREF) =>
          val passedArgs = fc.args.asInstanceOf[List[EVALUATED]]
          val argsError  = if (checkConstructorArgsTypes) constructorArgsTypesError(objectType, passedArgs) else None
          argsError match {
            case Some(e) => fail(e, l)
            case None =>
              write(s, CaseObj(objectType, (objectType.fields.map(_._1) zip passedArgs).toMap))
              done(l)
          }
        case _ => fail(s"Function or type '$name' not found", l)
      }

    private def evaluateRef(let: LET, s: Slot, l: Int, nextParentBlocks: List[BLOCK_DEF]): Unit = {
      if (let.valueShared) {
        let.valueShared = false
        let.value = EvaluationCopy(let.value)
      }
      frames.push(LetFrame(let, s))
      evaluate(let.value, LetSlot(let), l, nextParentBlocks)
    }

    private def evaluateGlobalVar(key: String, s: Slot, l: Int): Unit =
      ctx.ec.letDefs.get(key) match {
        case Some(lazyVal) =>
          lazyVal.value.value.fold(
            e => fail(e.message, l),
            value => {
              write(s, value)
              done(l - overheadCost)
            }
          )
        case None => fail(s"A definition of '$key' not found", l)
      }
  }
}

object EvaluatorV2 {
  // Where the value of an evaluated subexpression is written.
  private sealed trait Slot
  private final class RootSlot(var value: EXPR)                      extends Slot
  private final case class BlockSlot(block: BLOCK_DEF, parent: Slot) extends Slot
  private final case class ArgSlot(fc: FUNCTION_CALL, index: Int)    extends Slot
  private final case class GetterSlot(getter: GETTER)                extends Slot
  private final case class IfSlot(ifExpr: IF)                        extends Slot
  private final case class LetSlot(let: LET)                         extends Slot

  // What to do with the unused complexity once the subexpression evaluated on top of it finishes.
  private sealed trait Frame
  private final class CallFrame(val fc: FUNCTION_CALL, val startArgs: List[EXPR], val slot: Slot, val blocks: List[BLOCK_DEF]) extends Frame {
    val argCount: Int = fc.args.size
    var nextArg: Int  = 0
  }
  private final case class GetterFrame(getter: GETTER, slot: Slot)                  extends Frame
  private final case class IfFrame(ifExpr: IF, slot: Slot, blocks: List[BLOCK_DEF]) extends Frame
  // Also logs an error raised while evaluating the LET.
  private final case class LetFrame(let: LET, slot: Slot)              extends Frame
  private final case class NewModeFrame(fc: FUNCTION_CALL, limit: Int) extends Frame
  private final case class PrecalculatedFrame(unused: Int)             extends Frame

  // An Extended native call the run stopped at: where its value goes and the Coeval that computes it.
  private final case class SuspendedCall(
      function: NativeFunction[Environment],
      slot: Slot,
      coeval: Coeval[(Either[ExecutionError, EVALUATED], Int)]
  )

  // The blocks starting with the one that declares `key`, or Nil: the caller needs both, and this allocates nothing on the hot path.
  @tailrec
  private def findLet(key: String, blocks: List[BLOCK_DEF]): List[BLOCK_DEF] =
    blocks match {
      case block :: next =>
        block.dec match {
          case LET(`key`, _) => blocks
          case _             => findLet(key, next)
        }
      case Nil => Nil
    }

  @tailrec
  private def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[(FUNC, List[BLOCK_DEF])] =
    parentBlocks match {
      case (_: LET_BLOCK) :: xs                            => findUserFunction(name, xs)
      case BLOCK(f @ FUNC(`name`, _, _), _) :: scopeBlocks => Some((f, scopeBlocks))
      case _ :: xs                                         => findUserFunction(name, xs)
      case Nil                                             => None
    }

  // What a native call leaves, with the unused complexity; `outcome` is evaluated here, so an exception it throws becomes the call's error.
  private[evaluator] def nativeResult(
      function: NativeFunction[Environment],
      outcome: => (Either[ExecutionError, EVALUATED], Int),
      limit: Int,
      fixedThrownError: Boolean
  ): Either[(ExecutionError, Int), (EVALUATED, Int)] =
    try {
      val (result, evaluatedComplexity) = outcome
      result.bimap(
        {
          // The whole limit of the evaluation, not the complexity left at this call: changing it would change past results.
          case e: ThrownError if !fixedThrownError && function.ev.isInstanceOf[Extended[Environment]] => (e, limit)
          case e: ThrownError if !fixedThrownError                                                    => (e, 0)
          case e                                                                                      => (e, evaluatedComplexity)
        },
        (_, evaluatedComplexity)
      )
    } catch {
      case _: SecurityException =>
        Left((CommonError(s"""An access to ${function.ev} is denied"""), 0))
      case NonFatal(e) =>
        val error = e.getMessage match {
          case null => e.toString
          case msg  => msg
        }
        Left((CommonError(s"""An error during run ${function.ev}: ${e.getClass} $error"""), 0))
    }

  private[evaluator] def constructorArgsTypesError(objectType: CASETYPEREF, passedArgs: List[EVALUATED]): Option[String] = {
    def str[T](l: List[T]) = l.mkString("(", ", ", ")")

    if (objectType.fields.size != passedArgs.size)
      Some(s"Constructor ${objectType.name} expected ${objectType.fields.size} args, but ${str(passedArgs)} was passed")
    else {
      val fieldTypes      = objectType.fields.map(_._2)
      val passedArgsTypes = passedArgs.map(_.getType)
      if (!(fieldTypes zip passedArgsTypes).forall { case (fieldType, passedType) => fieldType >= passedType })
        Some(s"Passed args ${str(passedArgs)} are unsuitable for constructor ${objectType.name}${str(fieldTypes)}")
      else None
    }
  }

  /** Evaluates `expr` with the evaluator `ScopeEvaluator.mode` names; they agree on every result, see docs/notes/ride-evaluator.md. */
  def applyLimitedCoeval(
      expr: EXPR,
      logExtraInfo: LogExtraInfo,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      checkConstructorArgsTypes: Boolean = false,
      enableExecutionLog: Boolean = false,
      fixedThrownError: Boolean
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (EXPR, Int, Log[Id])]] =
    ScopeEvaluator.mode match {
      case ScopeEvaluator.Mode.V2 =>
        applyLimitedCoevalV2(expr, logExtraInfo, limit, ctx, stdLibVersion, correctFunctionCallScope, newMode, checkConstructorArgsTypes, enableExecutionLog, fixedThrownError)
      case mode =>
        ScopeEvaluator.applyLimitedCoeval(
          expr,
          logExtraInfo,
          limit,
          ctx,
          stdLibVersion,
          correctFunctionCallScope,
          newMode,
          checkConstructorArgsTypes,
          enableExecutionLog,
          fixedThrownError,
          check = mode == ScopeEvaluator.Mode.Check
        )
    }

  private[evaluator] def applyLimitedCoevalV2(
      expr: EXPR,
      logExtraInfo: LogExtraInfo,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      checkConstructorArgsTypes: Boolean,
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (EXPR, Int, Log[Id])]] = {
    val log = ListBuffer[LogItem[Id]]()

    val loggedCtx = if (enableExecutionLog) {
      EnabledLogEvaluationContext[Environment, Id](name => value => log.append((name, value)), ctx)
    } else {
      DisabledLogEvaluationContext[Environment, Id](ctx)
    }
    val exprCopy = EvaluationCopy(expr)
    logCall(loggedCtx, logExtraInfo, exprCopy, enableExecutionLog)
    val evaluator = new EvaluatorV2(
      loggedCtx,
      stdLibVersion,
      limit,
      correctFunctionCallScope,
      newMode,
      enableExecutionLog,
      checkConstructorArgsTypes,
      fixedThrownError
    )
    Coeval
      .defer(evaluator.run(exprCopy))
      .redeem(
        e => Left((e.getMessage, limit, log.toList)),
        _.bimap(
          { case (err, limit) => (err, limit, log.toList) },
          { case (expr, limit) => (expr, limit, log.toList) }
        )
      )
  }

  def applyOrDefault(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      logExtraInfo: LogExtraInfo,
      stdLibVersion: StdLibVersion,
      complexityLimit: Int,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      handleExpr: EXPR => Either[ExecutionError, EVALUATED],
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    EvaluatorV2
      .applyLimitedCoeval(
        expr,
        logExtraInfo,
        complexityLimit,
        ctx,
        stdLibVersion,
        correctFunctionCallScope,
        newMode,
        enableExecutionLog = enableExecutionLog,
        fixedThrownError = fixedThrownError
      )
      .value()
      .fold(
        { case (error, complexity, log) => (log, complexity, Left(error)) },
        { case (result, complexity, log) =>
          result match {
            case evaluated: EVALUATED => (log, complexity, Right(evaluated))
            case expr: EXPR           => (log, complexity, handleExpr(expr))
          }
        }
      )

  def applyCompleted(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      logExtraInfo: LogExtraInfo,
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    applyOrDefault(
      ctx,
      expr,
      logExtraInfo,
      stdLibVersion,
      Int.MaxValue,
      correctFunctionCallScope,
      newMode,
      expr => Left(s"Unexpected incomplete evaluation result $expr"),
      enableExecutionLog,
      fixedThrownError
    )

  private[evaluator] def logCall(
      loggedCtx: LoggedEvaluationContext[Environment, Id],
      logExtraInfo: LogExtraInfo,
      exprCopy: EXPR,
      enableExecutionLog: Boolean
  ): Unit = {
    @tailrec
    def findInvArgLet(expr: EXPR, let: LET): Option[LET] = {
      expr match {
        case BLOCK(res @ LET(let.name, value), _) if value == let.value => Some(res)
        case BLOCK(_, body)                                             => findInvArgLet(body, let)
        case _                                                          => None
      }
    }

    if (enableExecutionLog) {
      logExtraInfo.dAppAddress.foreach { addr =>
        val addrObj = Bindings.senderObject(addr)
        loggedCtx.log(LET(InvokedDApp, addrObj), addrObj.asRight[ExecutionError])
      }

      logExtraInfo.invokedFuncName.foreach { funcName =>
        val invokedFuncName = CONST_STRING(funcName)
        invokedFuncName.foreach(name => loggedCtx.log(LET(InvokedFuncName, name), invokedFuncName))
      }

      logExtraInfo.invArg.flatMap(findInvArgLet(exprCopy, _)).foreach {
        case let @ LET(_, obj: CaseObj) => loggedCtx.log(let, obj.asRight[ExecutionError])
        case _                          =>
      }
    }
  }

  private[evaluator] def logFunc(
      fc: FUNCTION_CALL,
      ctx: LoggedEvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion,
      limit: Int,
      enableExecutionLog: Boolean
  ): Unit = {
    if (enableExecutionLog) {
      val func     = ctx.ec.functions.get(fc.function)
      val funcName = func.map(_.name).getOrElse(fc.function.funcName)
      func match {
        case Some(f) =>
          val cost = f.costByLibVersion(stdLibVersion)
          if (limit >= cost) {
            logFuncArgs(fc, funcName, ctx)
            ctx.log(LET(s"$funcName.$Complexity", TRUE), CONST_LONG(cost).asRight[ExecutionError])
            ctx.log(LET(ComplexityLimit, TRUE), CONST_LONG(limit - cost).asRight[ExecutionError])
          }
        case None =>
          logFuncArgs(fc, funcName, ctx)
      }
    }
  }

  private def logFuncArgs(fc: FUNCTION_CALL, name: String, ctx: LoggedEvaluationContext[Environment, Id]): Unit = {
    val argsArr = ARR(fc.args.collect { case arg: EVALUATED => arg }.toIndexedSeq, false)
    argsArr.foreach(_ => ctx.log(LET(s"$name.$Args", TRUE), argsArr))
  }

  object LogKeys {
    val InvokedDApp     = "@invokedDApp"
    val InvokedFuncName = "@invokedFuncName"
    val ComplexityLimit = "@complexityLimit"
    val Complexity      = "@complexity"
    val Args            = "@args"
    val StateChanges    = "@stateChanges"

    val TraceExcluded = Seq(InvokedDApp, InvokedFuncName, StateChanges)
  }
}
