package com.wavesplatform.lang.v1.compiler

import cats.Show
import cats.implicits._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.CompilationError._
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
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

  def compileExpr(expr: Expressions.EXPR): CompileM[(Terms.EXPR, TYPE)] = {
    expr match {
      case x: Expressions.CONST_LONG                => (CONST_LONG(x.value): EXPR, LONG: TYPE).pure[CompileM]
      case x: Expressions.CONST_BYTEVECTOR          => handlePart(x.value).map(v => (CONST_BYTEVECTOR(v), BYTEVECTOR: TYPE))
      case x: Expressions.CONST_STRING              => handlePart(x.value).map(v => (CONST_STRING(v), STRING: TYPE))
      case _: Expressions.TRUE                      => (TRUE: EXPR, BOOLEAN: TYPE).pure[CompileM]
      case _: Expressions.FALSE                     => (FALSE: EXPR, BOOLEAN: TYPE).pure[CompileM]
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
                        ifFalseExpr: Expressions.EXPR): CompileM[(Terms.EXPR, TYPE)] = {
    get[CompilerContext, CompilationError].flatMap(ctx => {
      (for {
        cond       <- compileExpr(condExpr).run(ctx).map(_._2)()
        _          <- Either.cond(cond._2 == BOOLEAN, (), UnexpectedType(p.start, p.end, "BOOLEAN", cond._2.toString))
        ifTrue     <- compileExpr(ifTrueExpr).run(ctx).map(_._2)()
        ifFalse    <- compileExpr(ifFalseExpr).run(ctx).map(_._2)()
        compiledIf <- mkIf(p, cond._1, ifTrue, ifFalse)
      } yield compiledIf).toCompileM
    })
  }

  def flat(typeDefs: Map[String, DefinedType], tl: List[String]): List[String] =
    tl.flatMap(typeName =>
      typeDefs.get(typeName).fold(List(typeName)) {
        case UnionType(_, types) => types.map(_.name)
        case simple              => List(simple.name)
    })

  private def compileMatch(p: Pos, expr: Expressions.EXPR, cases: List[Expressions.MATCH_CASE]): CompileM[(Terms.EXPR, TYPE)] = {
    for {
      ctx       <- get[CompilerContext, CompilationError]
      typedExpr <- compileExpr(expr)
      exprTypes <- typedExpr._2 match {
        case u: UNION => u.pure[CompileM]
        case _        => raiseError[CompilerContext, CompilationError, UNION](MatchOnlyUnion(p.start, p.end))
      }
      _ <- cases
        .flatMap(_.types)
        .traverse[CompileM, String](handlePart)
        .map(tl => UNION(flat(ctx.predefTypes, tl).map(CASETYPEREF)))
        .flatMap(matchedTypes => {
          Either
            .cond(
              (cases.last.types.isEmpty && (exprTypes >= matchedTypes)) || (exprTypes equivalent matchedTypes),
              (),
              MatchNotExhaustive(p.start, p.end, exprTypes.l, matchedTypes.l)
            )
            .toCompileM
        })
      refTmpKey = "$match" + ctx.tmpArgsIdx
      _ <- set[CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = ctx.tmpArgsIdx + 1))
      allowShadowVarName = typedExpr._1 match {
        case REF(k) => Some(k)
        case _      => None
      }
      ifCases <- inspect[CompilerContext, CompilationError, Expressions.EXPR](updatedCtx => {
        mkIfCases(updatedCtx, cases, Expressions.REF(Pos(1, 1), PART.VALID(Pos(1, 1), refTmpKey)), allowShadowVarName)
      })
      compiledMatch <- compileBlock(p, Expressions.LET(Pos(1, 1), PART.VALID(Pos(1, 1), refTmpKey), expr, Seq.empty), ifCases)
    } yield compiledMatch
  }

  private def compileBlock(p: Pos, let: Expressions.LET, body: Expressions.EXPR): CompileM[(Terms.EXPR, TYPE)] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      letName <- handlePart(let.name)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, false))(n => !ctx.varDefs.contains(n) || let.allowShadowing)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, true))(n => !ctx.functionDefs.contains(n))
      compiledLet <- compileExpr(let.value)
      letTypes <- let.types.toList
        .traverse[CompileM, String](handlePart)
        .ensure(NonExistingType(p.start, p.end, letName, ctx.predefTypes.keys.toList))(_.forall(ctx.predefTypes.contains))
      typeUnion = if (letTypes.isEmpty) compiledLet._2 else UNION(letTypes.map(CASETYPEREF))
      _            <- modify[CompilerContext, CompilationError](vars.modify(_)(_ + (letName -> typeUnion)))
      compiledBody <- compileExpr(body)
    } yield (BLOCK(LET(letName, compiledLet._1), compiledBody._1), compiledBody._2)
  }

  private def compileGetter(p: Pos, fieldPart: PART[String], refExpr: Expressions.EXPR): CompileM[(Terms.EXPR, TYPE)] = {
    for {
      ctx         <- get[CompilerContext, CompilationError]
      field       <- handlePart(fieldPart)
      compiledRef <- compileExpr(refExpr)
      result <- (compiledRef._2 match {
        case CASETYPEREF(name) => mkGetter(p, ctx, name, field, compiledRef._1)
        case UNION(tl)         => mkGetter(p, ctx, tl, field, compiledRef._1)
        case _                 => Generic(p.start, p.end, "Unexpected ref type: neither simple type nor union type").asLeft[(EXPR, TYPE)]
      }).toCompileM
    } yield result
  }

  private def compileFunctionCall(p: Pos, namePart: PART[String], args: List[Expressions.EXPR]): CompileM[(EXPR, TYPE)] = {
    for {
      name         <- handlePart(namePart)
      signatures   <- get[CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
      compiledArgs <- args.traverse(compileExpr)
      result <- (signatures match {
        case Nil           => FunctionNotFound(p.start, p.end, name, compiledArgs.map(_._2.toString)).asLeft[(EXPR, TYPE)]
        case single :: Nil => matchFuncOverload(p, name, args, compiledArgs, single)
        case many => {
          val matchedSigs = many
            .map(matchFuncOverload(p, name, args, compiledArgs, _))
            .collect({ case Right(ex) => ex })

          matchedSigs match {
            case Nil         => FunctionNotFound(p.start, p.end, name, compiledArgs.map(_._2.toString)).asLeft[(EXPR, TYPE)]
            case call :: Nil => call.asRight[CompilationError]
            case _           => AmbiguousOverloading(p.start, p.end, name, signatures.toList).asLeft[(EXPR, TYPE)]
          }
        }
      }).toCompileM
    } yield result
  }

  private def compileRef(p: Pos, keyPart: PART[String]): CompileM[(EXPR, TYPE)] = {
    for {
      key <- handlePart(keyPart)
      ctx <- get[CompilerContext, CompilationError]
      result <- ctx.varDefs
        .get(key)
        .fold(raiseError[CompilerContext, CompilationError, (EXPR, TYPE)](DefNotFound(p.start, p.end, key)))(t => (REF(key): EXPR, t).pure[CompileM])
    } yield result
  }

  private def matchFuncOverload(p: Pos,
                                funcName: String,
                                funcArgs: List[Expressions.EXPR],
                                resolvedArgs: List[(EXPR, TYPE)],
                                f: FunctionTypeSignature): Either[CompilationError, (EXPR, TYPE)] = {
    val argTypes = f.args
    if (funcArgs.lengthCompare(argTypes.size) != 0)
      Left(WrongArgumentsNumber(p.start, p.end, funcName, argTypes.size, funcArgs.size))
    else {
      val typedExpressionArgumentsAndTypedPlaceholders: List[((EXPR, TYPE), TYPEPLACEHOLDER)] = resolvedArgs.zip(argTypes)

      val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case (typedExpr, tph) => (typedExpr._2, tph) }
      for {
        resolvedTypeParams <- TypeInferrer(typePairs).leftMap(Generic(p.start, p.end, _))
        resolvedResultType <- TypeInferrer.inferResultType(f.result, resolvedTypeParams).leftMap(Generic(p.start, p.end, _))
        header = FunctionHeader.Predef(f.internalName)
        args   = typedExpressionArgumentsAndTypedPlaceholders.map(_._1._1)
      } yield (FUNCTION_CALL(header, args): EXPR, resolvedResultType)
    }
  }

  def resolveFields(ctx: CompilerContext, tpe: DefinedType): List[(String, TYPE)] = tpe match {
    case CaseType(_, fields) => fields
    case UnionType(_, types) => types.map(n => resolveFields(ctx, ctx.predefTypes(n.name)).toSet).reduce(_ intersect _).toList
  }

  def mkIf(p: Pos, cond: EXPR, ifTrue: (EXPR, TYPE), ifFalse: (EXPR, TYPE)): Either[CompilationError, (EXPR, TYPE)] = {
    TypeInferrer
      .findCommonType(ifTrue._2, ifFalse._2)
      .fold(UnexpectedType(p.start, p.end, ifTrue._2.toString, ifFalse._2.toString).asLeft[(IF, TYPE)])(t =>
        (IF(cond, ifTrue._1, ifFalse._1), t).asRight)
  }

  def mkIfCases(ctx: CompilerContext, cases: List[MATCH_CASE], refTmp: Expressions.REF, allowShadowVarName: Option[String]): Expressions.EXPR = {
    cases.foldRight(Expressions.REF(Pos(1, 1), PART.VALID(Pos(1, 1), PureContext.errRef)): Expressions.EXPR)((mc, further) => {
      val blockWithNewVar = mc.newVarName.fold(mc.expr) { nv =>
        val allowShadowing = nv match {
          case PART.VALID(_, x) => allowShadowVarName.contains(x)
          case _                => false
        }
        Expressions.BLOCK(Pos(1, 1), Expressions.LET(Pos(1, 1), nv, refTmp, mc.types, allowShadowing), mc.expr)
      }
      mc.types.toList match {
        case Nil => blockWithNewVar
        case types =>
          def isInst(matchType: String): Expressions.EXPR =
            Expressions
              .FUNCTION_CALL(Pos(1, 1),
                             PART.VALID(Pos(1, 1), PureContext._isInstanceOf.name),
                             List(refTmp, Expressions.CONST_STRING(Pos(1, 1), PART.VALID(Pos(1, 1), matchType))))
          val hType :: tTypes = flat(ctx.predefTypes, types.map(_.asInstanceOf[PART.VALID[String]].v))
          val typeIf          = tTypes.foldLeft(isInst(hType))((other, matchType) => BINARY_OP(Pos(1, 1), isInst(matchType), BinaryOperation.OR_OP, other))
          Expressions.IF(Pos(1, 1), typeIf, blockWithNewVar, further)
      }
    })
  }

  private def mkGetter(p: Pos, ctx: CompilerContext, typeName: String, fieldName: String, expr: EXPR): Either[CompilationError, (GETTER, TYPE)] = {
    for {
      refTpe <- ctx.predefTypes
        .get(typeName)
        .fold(TypeNotFound(p.start, p.end, typeName).asLeft[DefinedType])(_.asRight)
      fieldTpe <- resolveFields(ctx, refTpe)
        .collectFirst({ case (field, tpe) if fieldName == field => tpe })
        .fold(FieldNotFound(p.start, p.end, fieldName, typeName).asLeft[TYPE])(_.asRight)
    } yield (GETTER(expr, fieldName), fieldTpe)
  }

  private def mkGetter(p: Pos,
                       ctx: CompilerContext,
                       types: List[CASETYPEREF],
                       fieldName: String,
                       expr: EXPR): Either[CompilationError, (GETTER, TYPE)] = {
    types
      .traverse[Option, TYPE](ctr => {
        ctx.predefTypes
          .get(ctr.name)
          .flatMap(resolveFields(ctx, _).find(_._1 == fieldName).map(_._2))
      })
      .fold((FieldNotFound(p.start, p.end, fieldName, s"Union($types)"): CompilationError).asLeft[(GETTER, TYPE)])(ts => {
        TypeInferrer.findCommonType(ts) match {
          case Some(ct) => (GETTER(expr, fieldName), ct).asRight[CompilationError]
          case None     => UnexpectedType(p.start, p.end, s"UNION($types)", s"UNION($ts)").asLeft[(GETTER, TYPE)]
        }
      })
  }

  private def handlePart[T](part: PART[T]): CompileM[T] = part match {
    case PART.VALID(_, x)         => x.pure[CompileM]
    case PART.INVALID(p, message) => raiseError(Generic(p.start, p.end, message))
  }
  def apply(c: CompilerContext, expr: Expressions.EXPR): Either[String, (EXPR, TYPE)] = {
    compileExpr(expr)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value
  }
}
