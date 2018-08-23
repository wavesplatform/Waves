package com.wavesplatform.lang.v1.compiler

import cats.Show
import cats.implicits._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.compiler.CompilationError._
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, _}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, MATCH_CASE, PART, Pos}
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._

class CompilerV1(ctx: CompilerContext) extends ExprCompiler {
  override type V = V1.type
  override val version: V = V1

  override def compile(input: String, directives: List[Directive]): Either[String, version.ExprT] = {
    Parser(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        if (xs.size > 1) Left("Too many expressions")
        else if (xs.isEmpty) Left("No expression")
        else {
          CompilerV1(ctx, xs.head) match {
            case Left(err)              => Left(err.toString)
            case Right((expr, BOOLEAN)) => Right(expr)
            case Right((_, _))          => Left("Script should return boolean")
          }
        }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
}

object CompilerV1 {

  def compileExpr(expr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL)] = {
    expr match {
      case x: Expressions.CONST_LONG                => (CONST_LONG(x.value): EXPR, LONG: FINAL).pure[CompileM]
      case x: Expressions.CONST_BYTEVECTOR          => handlePart(x.value).map(v => (CONST_BYTEVECTOR(v), BYTEVECTOR: FINAL))
      case x: Expressions.CONST_STRING              => handlePart(x.value).map(v => (CONST_STRING(v), STRING: FINAL))
      case _: Expressions.TRUE                      => (TRUE: EXPR, BOOLEAN: FINAL).pure[CompileM]
      case _: Expressions.FALSE                     => (FALSE: EXPR, BOOLEAN: FINAL).pure[CompileM]
      case Expressions.GETTER(p, ref, field)        => compileGetter(p, field, ref)
      case Expressions.BLOCK(p, let, body)          => compileBlock(p, let, body)
      case Expressions.IF(p, cond, ifTrue, ifFalse) => compileIf(p, cond, ifTrue, ifFalse)
      case Expressions.REF(p, key)                  => compileRef(p, key)
      case Expressions.FUNCTION_CALL(p, name, args) => compileFunctionCall(p, name, args)
      case Expressions.MATCH(p, ex, cases)          => compileMatch(p, ex, cases.toList)
      case Expressions.INVALID(p, message)          => raiseError(Generic(p.start, p.end, message))
      case Expressions.BINARY_OP(p, a, op, b) =>
        op match {
          case AND_OP => compileIf(p, a, b, Expressions.FALSE(p))
          case OR_OP  => compileIf(p, a, Expressions.TRUE(p), b)
          case _      => compileFunctionCall(p, PART.VALID(p, BinaryOperation.opsToFunctions(op)), List(a, b))
        }
    }
  }

  private def compileIf(p: Pos,
                        condExpr: Expressions.EXPR,
                        ifTrueExpr: Expressions.EXPR,
                        ifFalseExpr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL)] = {
    get[CompilerContext, CompilationError].flatMap(ctx => {
      (for {
        cond       <- compileExpr(condExpr).run(ctx).map(_._2)()
        _          <- Either.cond(cond._2 equivalent BOOLEAN, (), UnexpectedType(p.start, p.end, BOOLEAN.toString, cond._2.toString))
        ifTrue     <- compileExpr(ifTrueExpr).run(ctx).map(_._2)()
        ifFalse    <- compileExpr(ifFalseExpr).run(ctx).map(_._2)()
        compiledIf <- mkIf(p, cond._1, ifTrue, ifFalse)
      } yield compiledIf).toCompileM
    })
  }

  def flat(typeDefs: Map[String, DefinedType], tl: List[String]): List[FINAL] =
    tl.flatMap(typeName =>
      typeDefs.get(typeName) match {
        case Some(UnionType(_, unionTypes)) => unionTypes
        case Some(realType)                 => List(realType.typeRef)
        case None                           => List.empty
    })

  private def compileMatch(p: Pos, expr: Expressions.EXPR, cases: List[Expressions.MATCH_CASE]): CompileM[(Terms.EXPR, FINAL)] = {
    for {
      ctx       <- get[CompilerContext, CompilationError]
      typedExpr <- compileExpr(expr)
      exprTypes <- typedExpr._2 match {
        case u: UNION => u.pure[CompileM]
        case _        => raiseError[CompilerContext, CompilationError, UNION](MatchOnlyUnion(p.start, p.end))
      }
      refTmpKey = "$match" + ctx.tmpArgsIdx
      _ <- set[CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = ctx.tmpArgsIdx + 1))
      allowShadowVarName = typedExpr._1 match {
        case REF(k) => Some(k)
        case _      => None
      }
      ifCases <- inspectFlat[CompilerContext, CompilationError, Expressions.EXPR](updatedCtx => {
        mkIfCases(updatedCtx, cases, Expressions.REF(p, PART.VALID(AnyPos, refTmpKey)), allowShadowVarName).toCompileM
      })
      compiledMatch <- compileBlock(p, Expressions.LET(AnyPos, PART.VALID(AnyPos, refTmpKey), expr, Seq.empty), ifCases)
      _ <- cases
        .flatMap(_.types)
        .traverse[CompileM, String](handlePart)
        .map(tl => UNION.create(flat(ctx.predefTypes, tl)))
        .flatMap(matchedTypes => {
          Either
            .cond(
              (cases.last.types.isEmpty && (exprTypes >= matchedTypes)) || (exprTypes equivalent matchedTypes),
              (),
              MatchNotExhaustive(p.start, p.end, exprTypes.l, matchedTypes.l)
            )
            .toCompileM
        })
    } yield compiledMatch
  }

  private def compileBlock(p: Pos, let: Expressions.LET, body: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL)] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      letName <- handlePart(let.name)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = false))(n => !ctx.varDefs.contains(n) || let.allowShadowing)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = true))(n => !ctx.functionDefs.contains(n))
      compiledLet <- compileExpr(let.value)
      letTypes <- let.types.toList
        .traverse[CompileM, String](handlePart)
        .ensure(NonExistingType(p.start, p.end, letName, ctx.predefTypes.keys.toList))(_.forall(ctx.predefTypes.contains))
      typeUnion = if (letTypes.isEmpty) compiledLet._2 else UNION.create(letTypes.map(ctx.predefTypes).map(_.typeRef))
      _            <- modify[CompilerContext, CompilationError](vars.modify(_)(_ + (letName -> typeUnion)))
      compiledBody <- compileExpr(body)
    } yield (BLOCK(LET(letName, compiledLet._1), compiledBody._1), compiledBody._2)
  }

  private def compileGetter(p: Pos, fieldPart: PART[String], refExpr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL)] = {
    for {
      ctx         <- get[CompilerContext, CompilationError]
      field       <- handlePart(fieldPart)
      compiledRef <- compileExpr(refExpr)
      result <- (compiledRef._2 match {
        case (t @ CASETYPEREF(_, fielsd)) => mkGetter(p, ctx, List(t), field, compiledRef._1)
        case UNION(tl)                    => mkGetter(p, ctx, tl, field, compiledRef._1)
        case _                            => Generic(p.start, p.end, "Unexpected ref type: neither simple type nor union type").asLeft[(EXPR, FINAL)]
      }).toCompileM
    } yield result
  }

  private def compileFunctionCall(p: Pos, namePart: PART[String], args: List[Expressions.EXPR]): CompileM[(EXPR, FINAL)] = {
    for {
      ctx          <- get[CompilerContext, CompilationError]
      name         <- handlePart(namePart)
      signatures   <- get[CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
      compiledArgs <- args.traverse(compileExpr)
      result <- (signatures match {
        case Nil           => FunctionNotFound(p.start, p.end, name, compiledArgs.map(_._2.toString)).asLeft[(EXPR, FINAL)]
        case single :: Nil => matchFuncOverload(p, name, args, compiledArgs, ctx.predefTypes, single)
        case many =>
          val matchedSigs = many
            .map(matchFuncOverload(p, name, args, compiledArgs, ctx.predefTypes, _))
            .collect({ case Right(ex) => ex })

          matchedSigs match {
            case Nil         => FunctionNotFound(p.start, p.end, name, compiledArgs.map(_._2.toString)).asLeft[(EXPR, FINAL)]
            case call :: Nil => call.asRight[CompilationError]
            case _           => AmbiguousOverloading(p.start, p.end, name, signatures).asLeft[(EXPR, FINAL)]
          }
      }).toCompileM
    } yield result
  }

  private def compileRef(p: Pos, keyPart: PART[String]): CompileM[(EXPR, FINAL)] = {
    for {
      key <- handlePart(keyPart)
      ctx <- get[CompilerContext, CompilationError]
      result <- ctx.varDefs
        .get(key)
        .fold(raiseError[CompilerContext, CompilationError, (EXPR, FINAL)](DefNotFound(p.start, p.end, key)))(t =>
          (REF(key): EXPR, t: FINAL).pure[CompileM])
    } yield result
  }

  private def matchFuncOverload(p: Pos,
                                funcName: String,
                                funcArgs: List[Expressions.EXPR],
                                resolvedArgs: List[(EXPR, FINAL)],
                                predefTypes: Map[String, DefinedType],
                                f: FunctionTypeSignature): Either[CompilationError, (EXPR, FINAL)] = {
    val argTypes = f.args
    if (funcArgs.lengthCompare(argTypes.size) != 0)
      Left(WrongArgumentsNumber(p.start, p.end, funcName, argTypes.size, funcArgs.size))
    else {
      val typedExpressionArgumentsAndTypedPlaceholders = resolvedArgs.zip(argTypes)

      val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case (typedExpr, tph) => (typedExpr._2, tph) }
      for {
        resolvedTypeParams <- TypeInferrer(typePairs, predefTypes).leftMap(Generic(p.start, p.end, _))
        args = typedExpressionArgumentsAndTypedPlaceholders.map(_._1._1)
      } yield {
        val resultType = toFinal(f.result, resolvedTypeParams)
        (FUNCTION_CALL(f.header, args): EXPR, resultType)
      }
    }
  }

  def mkIf(p: Pos, cond: EXPR, ifTrue: (EXPR, FINAL), ifFalse: (EXPR, FINAL)): Either[CompilationError, (EXPR, FINAL)] = {
    val t = TypeInferrer.findCommonType(ifTrue._2, ifFalse._2)
    (IF(cond, ifTrue._1, ifFalse._1), t).asRight
  }

  def mkIfCases(ctx: CompilerContext,
                cases: List[MATCH_CASE],
                refTmp: Expressions.REF,
                allowShadowVarName: Option[String]): Either[CompilationError, Expressions.EXPR] = {

    def f(mc: MATCH_CASE, further: Expressions.EXPR): Either[CompilationError, Expressions.EXPR] = {
      val blockWithNewVar = mc.newVarName.fold(mc.expr) { nv =>
        val allowShadowing = nv match {
          case PART.VALID(_, x) => allowShadowVarName.contains(x)
          case _                => false
        }
        Expressions.BLOCK(mc.position, Expressions.LET(mc.position, nv, refTmp, mc.types, allowShadowing), mc.expr)
      }
      mc.types.toList match {
        case Nil => Right(blockWithNewVar)
        case types =>
          def isInst(matchType: String): Expressions.EXPR =
            Expressions
              .FUNCTION_CALL(
                mc.position,
                PART.VALID(mc.position, PureContext._isInstanceOf.name),
                List(refTmp, Expressions.CONST_STRING(mc.position, PART.VALID(mc.position, matchType)))
              )

          val flatTypes = flat(ctx.predefTypes, types.map(_.asInstanceOf[PART.VALID[String]].v)).map(_.name)
          flatTypes match {
            case Nil => Left(NonExistingType(mc.position.start, mc.position.end, mc.types.toString(), List.empty))
            case hType :: tTypes =>
              val typeIf =
                tTypes.foldLeft(isInst(hType))((other, matchType) => BINARY_OP(mc.position, isInst(matchType), BinaryOperation.OR_OP, other))
              Right(Expressions.IF(mc.position, typeIf, blockWithNewVar, further))
          }
      }
    }

    val default: Either[CompilationError, Expressions.EXPR] = Right(
      Expressions.FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "throw"), List.empty))
    cases.foldRight(default) {
      case (mc, furtherEi) =>
        furtherEi match {
          case Right(further) => f(mc, further)
          case Left(e)        => Left(e)
        }
    }

  }

  private def mkGetter(p: Pos, ctx: CompilerContext, types: List[FINAL], fieldName: String, expr: EXPR): Either[CompilationError, (GETTER, FINAL)] = {
    types
      .traverse[Option, FINAL](ctr => {
        ctr.fields.find(_._1 == fieldName).map(_._2)
      })
      .fold((FieldNotFound(p.start, p.end, fieldName, s"Union($types)"): CompilationError).asLeft[(GETTER, FINAL)])(ts => {
        val ct = TypeInferrer.findCommonType(ts)
        (GETTER(expr, fieldName), ct).asRight[CompilationError]
      })
  }

  private def handlePart[T](part: PART[T]): CompileM[T] = part match {
    case PART.VALID(_, x)         => x.pure[CompileM]
    case PART.INVALID(p, message) => raiseError(Generic(p.start, p.end, message))
  }
  def apply(c: CompilerContext, expr: Expressions.EXPR): Either[String, (EXPR, FINAL)] = {
    compileExpr(expr)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value
  }
}
