package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContextfulNativeFunction.{Extended, Simple}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.LogKeys.*
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.{constructorArgsTypesError, logFunc, nativeError, nativeException, nativeResult}
import com.wavesplatform.lang.v1.evaluator.ScopeEvaluator.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.traits.Environment
import monix.eval.Coeval

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** Evaluates a script against chains of scopes instead of rewriting a copy of its tree, with the same results, complexity and log as
  * EvaluatorV2 for every evaluation that completes or fails. Where EvaluatorV2 would stop with an incomplete result, this one gives up
  * (`Incomplete`) and the caller reruns the evaluation on EvaluatorV2: see docs/notes/ride-evaluator.md.
  */
final class ScopeEvaluator(
    ctx: LoggedEvaluationContext[Environment, Id],
    stdLibVersion: StdLibVersion,
    limit: Int,
    correctFunctionCallScope: Boolean,
    newMode: Boolean,
    enableExecutionLog: Boolean,
    checkConstructorArgsTypes: Boolean,
    fixedThrownError: Boolean
) {
  private val overheadCost: Int = if (newMode) 0 else 1

  def run(expr: EXPR): Coeval[Outcome] = {
    val execution = new Run(expr)
    def drive(): Coeval[Outcome] =
      Coeval.defer {
        execution.continue() match {
          case null =>
            execution.suspension.attempt.flatMap { outcome =>
              execution.resumeWith(outcome); drive()
            }
          case result => Coeval.now(result)
        }
      }
    drive()
  }

  private final class Run(start: EXPR) {
    private var frames = new Array[Frame](64)
    private var depth  = 0

    // EVAL: evaluate `expr` in `scope` with `budget`; RETURN: hand `value` and `budget` to the top frame; ERROR: unwind with `error`.
    private var state: Int            = EVAL
    private var expr: EXPR            = start
    private var scope: Scope          = null
    private var budget: Int           = limit
    private var value: EVALUATED      = null
    private var error: ExecutionError = null

    private var suspended: Suspended = null

    // A bit per hash bucket of every name this run has bound. Every scope is made of cells this run created, so a name whose bit is clear
    // is in none of them and a reference to it is a global: that skips the longest walks, those for `this`, `height` or `unit`.
    private val boundNames = new Array[Long](BoundNamesWords)

    private def bind(cell: LetCell): LetCell = {
      boundNames((cell.hash >>> 6) & (BoundNamesWords - 1)) |= 1L << cell.hash
      cell
    }

    private def mayBeBound(hash: Int): Boolean = (boundNames((hash >>> 6) & (BoundNamesWords - 1)) & (1L << hash)) != 0

    // The log dedupes by LET node. EvaluatorV2 had one node per LET of the script tree outside function bodies, the one logCall logs the
    // invocation argument with, and a fresh one per call inside them.
    private val rootLets: java.util.IdentityHashMap[LET, Unit] = if (enableExecutionLog) letsOutsideFunctions(start) else null

    def suspension: Coeval[(Either[ExecutionError, EVALUATED], Int)] = suspended.coeval

    // Runs until the evaluation ends, or returns null when it stops at an Extended native call.
    def continue(): Outcome = {
      var result: Outcome = null
      while (result == null && suspended == null) {
        state match {
          case EVAL => step()
          case RETURN =>
            if (depth == 0) result = Right((value, budget))
            else resume(pop())
          case ERROR =>
            if (depth == 0) result = Left((error, budget))
            else
              pop() match {
                case f: LetFrame => if (enableExecutionLog) ctx.log(f.cell.logKey, Left(error))
                case _           =>
              }
          case _ => result = Incomplete
        }
      }
      result
    }

    def resumeWith(outcome: Either[Throwable, (Either[ExecutionError, EVALUATED], Int)]): Unit = {
      val call = suspended
      suspended = null
      completeNative(nativeResult(call.function, outcome.fold(throw _, identity), limit, fixedThrownError))
    }

    private def push(f: Frame): Unit = {
      if (depth == frames.length) frames = java.util.Arrays.copyOf(frames, depth * 2)
      frames(depth) = f
      depth += 1
    }

    private def pop(): Frame = {
      depth -= 1
      val f = frames(depth)
      frames(depth) = null
      f
    }

    private def returnValue(v: EVALUATED, u: Int): Unit = {
      state = RETURN
      value = v
      budget = u
    }

    private def fail(e: ExecutionError, u: Int): Unit = {
      state = ERROR
      error = e
      budget = u
    }

    private def fail(message: String, u: Int): Unit = fail(CommonError(message), u)

    private def incomplete(): Unit = state = INCOMPLETE

    private def evaluate(e: EXPR, s: Scope, u: Int): Unit = {
      state = EVAL
      expr = e
      scope = s
      budget = u
    }

    private def step(): Unit =
      expr match {
        case ev: EVALUATED => returnValue(ev, budget)
        case block: BLOCK_DEF =>
          block.dec match {
            case let: LET =>
              val cell = bind(new LetCell(let.name, let.value, scope))
              if (rootLets != null && rootLets.containsKey(let)) cell.logKey = let
              scope = cell
            case func: FUNC => scope = new FuncEntry(func, scope)
            case _          =>
          }
          expr = block.body
        case REF(key) =>
          val hash = key.hashCode
          if (!mayBeBound(hash)) evaluateGlobalVar(key)
          else
            findLet(key, hash, scope) match {
              case null => evaluateGlobalVar(key)
              case cell => evaluateRef(cell)
            }
        case i: IF =>
          push(new IfFrame(i, scope))
          expr = i.cond
        case g: GETTER =>
          push(new GetterFrame(g.field))
          expr = g.expr
        case fc: FUNCTION_CALL => continueCall(new CallFrame(fc, scope), budget)
        case f: FAILED_EXPR    => fail(s"Unexpected $f", budget)
      }

    private def resume(frame: Frame): Unit = frame match {
      case f: CallFrame =>
        f.values += value
        continueCall(f, budget)
      case f: LetFrame =>
        f.cell.value = value
        f.cell.evaluating = false
        letValue(f.cell)
      case f: IfFrame =>
        value match {
          case CONST_BOOLEAN(_) if budget <= 0 => incomplete()
          case CONST_BOOLEAN(true)             => evaluate(f.ifExpr.ifTrue, f.scope, budget - overheadCost)
          case CONST_BOOLEAN(false)            => evaluate(f.ifExpr.ifFalse, f.scope, budget - overheadCost)
          case _                               => fail("Non-boolean result in cond", budget)
        }
      case f: GetterFrame =>
        value match {
          case co: CaseObj if budget > 0 => returnValue(co.fields(f.field), budget - overheadCost)
          case _: CaseObj                => incomplete()
          case ev                        => fail(s"GETTER of non-case-object $ev with field '${f.field}", budget)
        }
      case f: NewModeFrame =>
        if (enableExecutionLog) logLetAhead()
        if (budget == f.limit) {
          if (budget >= 1 && f.function.isInstanceOf[FunctionHeader.User] && enableExecutionLog) {
            ctx.log(LET(s"${f.function.funcName}.$Complexity", TRUE), CONST_LONG(1).asRight[ExecutionError])
            ctx.log(LET(ComplexityLimit, TRUE), CONST_LONG(budget - 1).asRight[ExecutionError])
          }
          budget -= 1
        }
      case f: PrecalculatedFrame =>
        if (enableExecutionLog) logLetAhead()
        budget = f.unused
    }

    private def evaluateRef(cell: LetCell): Unit =
      if (cell.value != null) letValue(cell)
      else if (cell.evaluating) incomplete() // a cycle, which no compiled script has: left to EvaluatorV2
      else {
        cell.evaluating = true
        push(new LetFrame(cell))
        evaluate(cell.expr, cell.next, budget)
      }

    // EvaluatorV2 wrote a call's value into the LET it is the value of, which logs it, before the frames that only adjust complexity ran.
    private def logLetAhead(): Unit = {
      var i = depth - 1
      while (i >= 0 && (frames(i).isInstanceOf[NewModeFrame] || frames(i).isInstanceOf[PrecalculatedFrame])) i -= 1
      if (i >= 0) frames(i) match {
        case f: LetFrame => ctx.log(f.cell.logKey, Right(value))
        case _           =>
      }
    }

    // The value of a LET reaching the expression that referenced it, as EvaluatorV2's LetFrame passes it on.
    private def letValue(cell: LetCell): Unit = {
      if (enableExecutionLog) ctx.log(cell.logKey, Right(cell.value))
      if (budget > 0) returnValue(cell.value, budget - overheadCost) else incomplete()
    }

    private def evaluateGlobalVar(key: String): Unit =
      ctx.ec.letDefs.get(key) match {
        case Some(lazyVal) =>
          lazyVal.value.value.fold(e => fail(e.message, budget), v => returnValue(v, budget - overheadCost))
        case None => fail(s"A definition of '$key' not found", budget)
      }

    // Evaluates the arguments left to right while complexity remains, then the function itself; an evaluated argument costs nothing.
    private def continueCall(f: CallFrame, u: Int): Unit = {
      while (u > 0 && f.remaining.nonEmpty && f.remaining.head.isInstanceOf[EVALUATED]) {
        f.values += f.remaining.head.asInstanceOf[EVALUATED]
        f.remaining = f.remaining.tail
      }
      if (u <= 0) incomplete()
      else if (f.remaining.nonEmpty) {
        val next = f.remaining.head
        f.remaining = f.remaining.tail
        push(f)
        next match {
          case call: FUNCTION_CALL =>
            val child = new CallFrame(call, f.scope)
            f.recordArgCall(child)
            state = EVAL
            continueCall(child, u)
          case _ => evaluate(next, f.scope, u)
        }
      } else {
        val args = f.values.toList
        if (enableExecutionLog) logFunc(FUNCTION_CALL(f.fc.function, args), ctx, stdLibVersion, u, enableExecutionLog)
        evaluateFunction(f, args, u)
      }
    }

    private def evaluateFunction(f: CallFrame, args: List[EVALUATED], u: Int): Unit = {
      if (newMode) push(new NewModeFrame(f.fc.function, u))
      f.fc.function match {
        case FunctionHeader.Native(_) => evaluateNativeFunction(f.fc, args, u)
        case FunctionHeader.User(_, name) =>
          ctx.ec.functions.get(f.fc.function) match {
            case Some(function) =>
              val uf   = function.asInstanceOf[UserFunction[Environment]]
              val body = uf.ev[Id](ctx.ec.environment, f.startArgs)
              if (newMode) {
                val precalculated = u - uf.costByLibVersion(stdLibVersion).toInt
                push(new PrecalculatedFrame(precalculated))
                evaluate(body, bindArgs(uf.argList, args, f.scope), precalculated)
              } else evaluate(body, bindArgs(uf.argList, args, f.scope), u)
            case None =>
              findUserFunction(name, f.scope) match {
                case null => evaluateConstructor(args, u, name)
                case entry =>
                  val bodyScope = if (correctFunctionCallScope) entry.next else f.scope
                  evaluate(entry.func.body, bindArgs(entry.func.args, args, bodyScope), u)
              }
          }
      }
    }

    // Arguments are LETs holding their values, the later shadowing the earlier, as EvaluatorV2 wraps the function body in blocks.
    private def bindArgs(names: List[String], values: List[EVALUATED], scope: Scope): Scope = {
      var s  = scope
      var ns = names
      var vs = values
      while (ns.nonEmpty && vs.nonEmpty) {
        val cell = bind(new LetCell(ns.head, vs.head, s))
        cell.value = vs.head
        s = cell
        ns = ns.tail
        vs = vs.tail
      }
      s
    }

    private def evaluateNativeFunction(fc: FUNCTION_CALL, args: List[EVALUATED], u: Int): Unit =
      ctx.ec.functions.get(fc.function) match {
        case None => fail(s"function '${fc.function}' not found", u)
        case Some(function) =>
          val cost = function.costByLibVersion(stdLibVersion).toInt
          if (u < cost) incomplete()
          else {
            val native = function.asInstanceOf[NativeFunction[Environment]]
            native.ev match {
              case f: Extended[Environment] =>
                val coeval = Coeval
                  .defer(f.evaluate[Id](ctx.ec.environment, args, u - cost))
                  .map { case (result, unusedComplexity) =>
                    result.map { case (evaluated, log) =>
                      log.foreach { case (logItemName, logItemValue) => ctx.log(LET(logItemName, TRUE), logItemValue) }
                      evaluated
                    } -> unusedComplexity
                  }
                suspended = Suspended(native, coeval)
              case f: Simple[Environment] =>
                // nativeResult without the tuple and closure it takes the outcome in
                var thrown: ExecutionError = null
                val result =
                  try f.evaluate(ctx.ec.environment, args)
                  catch { case NonFatal(e) => thrown = nativeException(native, e); null }
                if (thrown != null) fail(thrown, 0)
                else
                  result match {
                    case Right(v) => returnValue(v, u - cost)
                    case Left(e) =>
                      val (error, unused) = nativeError(native, e, u - cost, limit, fixedThrownError)
                      fail(error, unused)
                  }
            }
          }
      }

    private def completeNative(result: Either[(ExecutionError, Int), (EVALUATED, Int)]): Unit =
      result match {
        case Right((v, u)) => returnValue(v, u)
        case Left((e, u))  => fail(e, u)
      }

    private def evaluateConstructor(args: List[EVALUATED], u: Int, name: String): Unit =
      ctx.ec.typeDefs.get(name) match {
        case Some(objectType: CASETYPEREF) =>
          val argsError = if (checkConstructorArgsTypes) constructorArgsTypesError(objectType, args) else None
          argsError match {
            case Some(e) => fail(e, u)
            case None    => returnValue(CaseObj(objectType, (objectType.fields.map(_._1) zip args).toMap), u)
          }
        case _ => fail(s"Function or type '$name' not found", u)
      }
  }
}

object ScopeEvaluator {
  enum Mode {
    // V2: EvaluatorV2 only. Scope: this evaluator. Check: this evaluator, then EvaluatorV2 on the same inputs, reporting any difference.
    case V2, Scope, Check
  }

  @volatile var mode: Mode = sys.props.get("waves.evaluator") match {
    case Some("scope") => Mode.Scope
    case Some("check") => Mode.Check
    case _             => Mode.V2
  }

  // Called with a description of a difference in check mode. The default also prints it: the exception may become a failed transaction.
  @volatile var onMismatch: String => Unit = { message =>
    System.err.println(s"EVALUATOR MISMATCH: $message")
    throw new IllegalStateException(message)
  }

  // Where EvaluatorV2 would stop with a partially evaluated tree.
  object Incomplete
  type Outcome = Either[(ExecutionError, Int), (EVALUATED, Int)] | Incomplete.type

  type Result = Either[(ExecutionError, Int, Log[Id]), (EXPR, Int, Log[Id])]

  def applyLimitedCoeval(
      expr: EXPR,
      logExtraInfo: ContractEvaluator.LogExtraInfo,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      checkConstructorArgsTypes: Boolean,
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean,
      check: Boolean
  ): Coeval[Result] = {
    val environmentLog = new EnvironmentLog
    val log            = ListBuffer[LogItem[Id]]()
    // Pure contexts (tests, the REPL) have no environment, just a placeholder.
    def withEnvironment(wrap: Environment[Id] => Environment[Id]) = (ctx.environment: Any) match {
      case environment: Environment[Id] @unchecked => ctx.copy(environment = wrap(environment))
      case _                                       => ctx
    }
    val recordingCtx = withEnvironment(new RecordingEnvironment(_, environmentLog))
    val loggedCtx =
      if (enableExecutionLog) EnabledLogEvaluationContext[Environment, Id](name => value => log.append((name, value)), recordingCtx)
      else DisabledLogEvaluationContext[Environment, Id](recordingCtx)

    // EvaluatorV2 on the inputs this evaluation has seen so far.
    def evaluateV2(strict: Boolean): Coeval[Result] = {
      var replaying: ReplayingEnvironment = null
      val replayingCtx = withEnvironment { environment =>
        replaying = new ReplayingEnvironment(environment, environmentLog, strict)
        replaying
      }
      EvaluatorV2.applyLimitedCoevalV2(
        expr,
        logExtraInfo,
        limit,
        replayingCtx,
        stdLibVersion,
        correctFunctionCallScope,
        newMode,
        checkConstructorArgsTypes,
        enableExecutionLog,
        fixedThrownError
      ).map { result =>
        if (replaying != null && strict && replaying.unreplayed != 0)
          onMismatch(s"EvaluatorV2 made ${replaying.unreplayed} environment calls fewer for $expr")
        result
      }
    }

    val evaluator =
      new ScopeEvaluator(loggedCtx, stdLibVersion, limit, correctFunctionCallScope, newMode, enableExecutionLog, checkConstructorArgsTypes, fixedThrownError)
    Coeval
      .defer {
        EvaluatorV2.logCall(loggedCtx, logExtraInfo, expr, enableExecutionLog)
        evaluator.run(expr)
      }
      .attempt
      .flatMap {
        case Right(Incomplete) => evaluateV2(strict = false)
        case outcome =>
          val result: Result = outcome match {
            case Left(e)              => Left((e.getMessage, limit, log.toList))
            case Right(Left((e, u)))  => Left((e, u, log.toList))
            case Right(Right((v, u))) => Right((v, u, log.toList))
            case Right(_)             => throw new IllegalStateException("unreachable")
          }
          if (!check) Coeval.now(result)
          else {
            evaluateV2(strict = true).map { expected =>
              if (expected != result) onMismatch(s"${difference(result, expected)} for $expr")
              result
            }
          }
      }
  }

  private def difference(actual: Result, expected: Result): String = {
    def parts(r: Result) = r.fold({ case (e, u, l) => (Left(e), u, l) }, { case (v, u, l) => (Right(v), u, l) })
    val (actualValue, actualUnused, actualLog)       = parts(actual)
    val (expectedValue, expectedUnused, expectedLog) = parts(expected)
    if (actualValue != expectedValue) s"ScopeEvaluator returned $actualValue where EvaluatorV2 returned $expectedValue"
    else if (actualUnused != expectedUnused) s"ScopeEvaluator left $actualUnused complexity where EvaluatorV2 left $expectedUnused"
    else {
      val i = actualLog.lazyZip(expectedLog).toSeq.indexWhere { case (a, b) => a != b } match {
        case -1 => actualLog.length min expectedLog.length
        case i  => i
      }
      s"ScopeEvaluator logged ${actualLog.lift(i)} where EvaluatorV2 logged ${expectedLog.lift(i)} at log entry $i " +
        s"(${actualLog.length} and ${expectedLog.length} entries)"
    }
  }

  private final val BoundNamesWords = 16 // a power of 2

  private final val EVAL       = 0
  private final val RETURN     = 1
  private final val ERROR      = 2
  private final val INCOMPLETE = 3

  // A scope is a chain of declarations, innermost first, like EvaluatorV2's list of enclosing blocks; null is the empty scope.
  private sealed abstract class Scope(val next: Scope)

  // A LET entered in a scope: evaluated at most once, in the scope it was declared in, like the LET node EvaluatorV2 writes its value into.
  private final class LetCell(val name: String, val expr: EXPR, next: Scope) extends Scope(next) {
    val hash: Int           = name.hashCode
    var value: EVALUATED    = null
    var evaluating: Boolean = false

    private var key: LET = null
    def logKey: LET = {
      if (key == null) key = LET(name, expr)
      key
    }
    def logKey_=(let: LET): Unit = key = let
  }

  private final class FuncEntry(val func: FUNC, next: Scope) extends Scope(next) {
    val hash: Int = func.name.hashCode
  }

  private sealed abstract class Frame
  private final class CallFrame(val fc: FUNCTION_CALL, val scope: Scope) extends Frame {
    var remaining: List[EXPR]         = fc.args
    val values: ListBuffer[EVALUATED] = ListBuffer.empty

    // The calls among the arguments, by position, once they are being evaluated.
    private var argCalls: Array[CallFrame] = null

    def recordArgCall(child: CallFrame): Unit = {
      if (argCalls == null) argCalls = new Array[CallFrame](fc.args.length)
      argCalls(values.length) = child
    }

    // The arguments as EvaluatorV2 passed them to a context function: the original expressions, except that it had written the evaluated
    // arguments into every call among them (`value` builds its error message from those).
    def startArgs: List[EXPR] =
      if (argCalls == null) fc.args
      else
        fc.args.zipWithIndex.map { case (arg, i) =>
          argCalls(i) match {
            case null  => arg
            case child => FUNCTION_CALL(child.fc.function, child.values.toList)
          }
        }
  }
  private final class LetFrame(val cell: LetCell)                                extends Frame
  private final class IfFrame(val ifExpr: IF, val scope: Scope)                  extends Frame
  private final class GetterFrame(val field: String)                             extends Frame
  private final class NewModeFrame(val function: FunctionHeader, val limit: Int) extends Frame
  private final class PrecalculatedFrame(val unused: Int)                        extends Frame

  private final case class Suspended(function: NativeFunction[Environment], coeval: Coeval[(Either[ExecutionError, EVALUATED], Int)])

  private def letsOutsideFunctions(root: EXPR): java.util.IdentityHashMap[LET, Unit] = {
    val lets = new java.util.IdentityHashMap[LET, Unit]()
    val work = new java.util.ArrayDeque[EXPR]()
    work.push(root)
    while (!work.isEmpty) {
      work.pop() match {
        case b: BLOCK_DEF =>
          b.dec match {
            case let: LET =>
              lets.put(let, ())
              work.push(let.value)
            case _ =>
          }
          work.push(b.body)
        case i: IF             => work.push(i.cond); work.push(i.ifTrue); work.push(i.ifFalse)
        case g: GETTER         => work.push(g.expr)
        case fc: FUNCTION_CALL => fc.args.foreach(work.push)
        case _                 =>
      }
    }
    lets
  }

  private def findLet(key: String, hash: Int, scope: Scope): LetCell = {
    var s = scope
    while (s != null) {
      s match {
        case cell: LetCell if cell.hash == hash && cell.name.equals(key) => return cell
        case _                                 => s = s.next
      }
    }
    null
  }

  private def findUserFunction(name: String, scope: Scope): FuncEntry = {
    val hash = name.hashCode
    var s    = scope
    while (s != null) {
      s match {
        case entry: FuncEntry if entry.hash == hash && entry.func.name.equals(name) => return entry
        case _                                           => s = s.next
      }
    }
    null
  }

}
