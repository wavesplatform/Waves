package com.wavesplatform.lang.v1.compiler

import cats.Show
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.parser.Expressions.Pos

sealed trait CompilationError {
  def start: Int
  def end: Int
  def message: String
}

object CompilationError {

  implicit val show: Show[CompilationError] = (ce: CompilationError) => {
    s"${ce.message} in ${ce.start}-${ce.end}"
  }

  final case object TooManyExpressions extends CompilationError {

    override def start: Int = 0

    override def end: Int = 0

    val message = "Too many expressions"
  }
  final case object NoExpressions extends CompilationError {
    override def start: Int = 0

    override def end: Int = 0

    val message = "No expressions"
  }

  final case class MatchOnlyUnion(start: Int, end: Int) extends CompilationError {
    val message = "Only union type can be matched"
  }

  final case class MatchNotExhaustive(start: Int, end: Int, possible: List[TYPE], matched: List[TYPE]) extends CompilationError {
    val message = s"Matching not exhaustive: possibleTypes are $possible, while matched are $matched"
  }
  final case class AlreadyDefined(start: Int, end: Int, name: String, isFunction: Boolean) extends CompilationError {
    val message =
      if (isFunction) s"Value '$name' can't be defined because function with this name is already defined"
      else s"Value '$name' already defined in the scope"
  }
  final case class NonExistingType(start: Int, end: Int, name: String, existing: List[String]) extends CompilationError {
    val message = s"Value '$name' declared as non-existing type, while all possible types are $existing"
  }

  final case class BadFunctionSignatureSameArgNames(start: Int, end: Int, name: String) extends CompilationError {
    val message = s"Function '$name' declared with duplicating argument names"
  }

  final case class FunctionNotFound(start: Int, end: Int, name: String, argTypes: List[String]) extends CompilationError {
    val message = s"Can't find a function '$name'(${argTypes.mkString(", ")}) or it is @Callable"
  }

  final case class OverloadNotFound(start: Int, end: Int, name: String, argTypes: List[String]) extends CompilationError {
    val message = s"Can't find a function overload '$name'(${argTypes.mkString(", ")})"
  }

  final case class AmbiguousOverloading(start: Int, end: Int, name: String, candidates: List[FunctionTypeSignature]) extends CompilationError {
    val message = {
      val stringRepr = candidates.map(sig => s"'$name'(${sig.args.mkString(", ")})").mkString("; ")
      s"Can't choose an overloaded function. Candidates: $stringRepr"
    }
  }
  final case class DefNotFound(start: Int, end: Int, key: String) extends CompilationError {
    val message = s"A definition of '$key' is not found"
  }
  final case class WrongArgumentsNumber(start: Int, end: Int, name: String, required: Int, found: Int) extends CompilationError {
    val message = s"Function '$name' requires $required arguments, but $found are provided"
  }
  final case class WrongArgumentType(start: Int, end: Int, funcName: String, typeName: String, required: List[String]) extends CompilationError {
    val message = s"Unexpected argument type in function '$funcName', required: (${required.mkString(", ")}), but $typeName type found"
  }
  final case class UnexpectedType(start: Int, end: Int, required: String, found: String) extends CompilationError {
    val message = s"Unexpected type, required: $required, but $found found"
  }
  final case class TypeNotFound(
                                 start:         Int,
                                 end:           Int,
                                 name:          String,
                                 expectedTypes: List[String],
                                 varName:       Option[String]
                               ) extends CompilationError {
    val message = {
      val varStr = varName.fold("")(v => s" of variable `$v`")
      val expectedTypesStr =
        if (expectedTypes.nonEmpty) s", expected: ${expectedTypes.mkString(", ")}"
        else ""
      s"Undefined type: `$name`" + varStr + expectedTypesStr
    }
  }

  final case class GenericTypeNotFound(start: Int, end: Int, t: String) extends CompilationError {
    val message = s"Undefined generic type `$t`"
  }

  final case class FieldNotFound(start: Int, end: Int, name: String, typeName: String) extends CompilationError {
    val message = s"Undefined field `$name` of variable of type `$typeName`"
  }
  final case class Generic(start: Int, end: Int, message: String) extends CompilationError
}
