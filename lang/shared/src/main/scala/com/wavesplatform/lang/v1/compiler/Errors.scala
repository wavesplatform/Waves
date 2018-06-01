package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Terms.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import supertagged.TaggedType

object Errors {

  object CompilationError extends TaggedType[String]
  type CompilationError = CompilationError.Type

  def Generic(msg: String): CompilationError = CompilationError @@ msg

  val TooManyExpressions: CompilationError = Generic("Too many expressions")
  val NoExpressions: CompilationError      = Generic("No expressions")

  val MatchOnlyUnion: CompilationError = Generic("Only union type can be matched")

  def MatchNotExhaustive(possible: List[CASETYPEREF], matched: List[CASETYPEREF]): CompilationError =
    Generic(s"Matching not exhaustive: possibleTypes are $possible, while matched are $matched")

  def AlreadyDefined(name: String, isFunction: Boolean): CompilationError =
    if (isFunction) Generic(s"Value '$name' can't be defined because function with such name is predefined")
    else Generic(s"Value '$name' already defined in the scope")

  def NonExistingType(name: String, existing: List[String]): CompilationError =
    Generic(s"Value '$name' declared as non-existing type, while all possible types are $existing")

  def FunctionNotFound(name: String, argTypes: List[String]): CompilationError =
    Generic(s"Can't find a function '$name'(${argTypes.mkString(", ")})")

  def AmbiguousOverloading(name: String, candidates: List[FunctionTypeSignature]): CompilationError = {
    val stringRepr = candidates.map(sig => s"'$name'(${sig.args.mkString(", ")})").mkString("; ")
    Generic(s"Can't choose an overloaded function. Candidates: $stringRepr")
  }

  def DefNotFound(key: String): CompilationError =
    Generic(s"A definition of '$key' is not found")

  def WrongArgumentsNumber(name: String, required: Int, found: Int): CompilationError =
    Generic(s"Function '$name' requires $required arguments, but $found are provided")

  def UnexpectedType(required: String, found: String): CompilationError =
    Generic(s"Unexpected type, required: $required, but $found found")

  def TypeNotFound(name: String): CompilationError =
    Generic(s"Undefined type: `$name`")

  def FieldNotFound(name: String, typeName: String): CompilationError =
    Generic(s"Undefined field `$name` of variable of type `$typeName`")
}
