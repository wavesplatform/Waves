package com.wavesplatform.lang.v1.compiler

import cats.data._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.all._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
//import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, PART}
//import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.parser.Expressions.PART
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import monix.eval.Coeval

import scala.util.Try

class CompilerV1(ctx: CompilerContext) extends ExprCompiler {
  override type V = V1.type
  override val version: V = V1

  override def compile(input: String, directives: List[Directive]): Either[String, version.ExprT] = {
    Parser(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        if (xs.size > 1) Left("Too many expressions")
        else if (xs.isEmpty) Left("No expression")
        else
          CompilerV1(ctx, xs.head) match {
            case Left(err)   => Left(err.toString)
            case Right(expr) => Right(expr)
          }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
}

object CompilerV1 {

  type TypeResolutionError      = String
  type CompilationResult[T]     = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  type ResolvedArgsResult = EitherT[Coeval, String, List[EXPR]]

  private def compile(ctx: CompilerContext, expr: Expressions.EXPR): SetTypeResult[EXPR] = expr match {
    case x: Expressions.CONST_LONG       => EitherT.pure(CONST_LONG(x.value))
    case x: Expressions.CONST_BYTEVECTOR => handlePart(x.value)(CONST_BYTEVECTOR)
    case x: Expressions.CONST_STRING     => handlePart(x.value)(CONST_STRING)
    case _: Expressions.TRUE             => EitherT.pure(TRUE)
    case _: Expressions.FALSE            => EitherT.pure(FALSE)
    case getter: Expressions.GETTER      => compileGetter(ctx, getter)
    case fc: Expressions.FUNCTION_CALL   => compileFunctionCall(ctx, fc)
    case block: Expressions.BLOCK        => compileBlock(ctx, block)
    case Expressions.TBLOCK(_, _, PART.INVALID(start,end,message), _, _) =>  EitherT.leftT[Coeval, EXPR](s"$message in $start-$end")
    case Expressions.TBLOCK(_, _, PART.VALID(_,_,typeName), typeDef, body) =>
         val types = ctx.predefTypes
         val union = typeDef.types.flatMap({
            t => types.get(t).fold(List(CASETYPEREF(t))) {
              case UnionType(_, tl, _) => tl
              case t: CASETYPEREF => List(t)
              case _ => ???
            }
         }).toSet.toList
         compile(ctx.copy(predefTypes = types + (typeName -> UnionType(typeName, union, ctx))), body)
    case ifExpr: Expressions.IF          => compileIf(ctx, ifExpr)
    case ref: Expressions.REF            => compileRef(ctx, ref)
    case m: Expressions.MATCH            => compileMatch(ctx, m)
    case Expressions.TYPELIST(start, end, tl)  => EitherT.pure(TYPELIST(tl.map(CONST_STRING)))
    case Expressions.BINARY_OP(start, end, a, op, b) =>
      op match {
        case AND_OP => compileIf(ctx, Expressions.IF(start, end, a, b, Expressions.FALSE(start, end)))
        case OR_OP  => compileIf(ctx, Expressions.IF(start, end, a, Expressions.TRUE(start, end), b))
        case _      => compileFunctionCall(ctx, Expressions.FUNCTION_CALL(start, end, PART.VALID(start, end, opsToFunctions(op)), List(a, b)))
      }
    case Expressions.INVALID(start, end, message, _) =>  EitherT.leftT[Coeval, EXPR](s"$message in $start-$end")
  }

  private def compileGetter(ctx: CompilerContext, getter: Expressions.GETTER): SetTypeResult[EXPR] =
    for {
      field <- EitherT.fromEither[Coeval](getter.field.toEither)
      r <- compile(ctx, getter.ref)
        .subflatMap { subExpr =>
          def getField(typeName: String): Either[String, GETTER] = {
            val refTpe = ctx.predefTypes.get(typeName).map(Right(_)).getOrElse(Left(s"Undefined type: $typeName"))
            val fieldTpe = refTpe.flatMap { ct =>
              val fieldTpe = ct.fields.collectFirst { case (fieldName, tpe) if fieldName == field => tpe }
              fieldTpe.map(Right(_)).getOrElse(Left(s"Undefined field `$field` of variable of type `$typeName`"))
            }
            fieldTpe.right.map(tpe => GETTER(expr = subExpr, field = field, tpe = tpe))
          }

          subExpr.tpe match {
            case typeRef: CASETYPEREF => getField(typeRef.name)
            case union: UNION =>
              val x1 = union.l
                .map(k => ctx.predefTypes(k.name))
                .map(predefType => predefType.fields.find(_._1 == field))
              if (x1.contains(None)) Left(s"Undefined field `$field` on $union")
              else
                TypeInferrer.findCommonType(x1.map(_.get._2)) match {
                  case Some(cT) => Right(GETTER(expr = subExpr, field = field, tpe = cT))
                  case None     => Left(s"Undefined common type for field `$field` on $union")
                }

            case x => Left(s"Can't access to '$field' of a primitive type $x")
          }
        }
    } yield r

  private def compileIf(ctx: CompilerContext, ifExpr: Expressions.IF): SetTypeResult[EXPR] =
    (compile(ctx, ifExpr.cond), compile(ctx, ifExpr.ifTrue), compile(ctx, ifExpr.ifFalse)).tupled
      .subflatMap[String, EXPR] {
        case (resolvedCond: EXPR, resolvedIfTrue, resolvedIfFalse) =>
          if (resolvedCond.tpe != BOOLEAN)
            Left(s"IF clause is expected to be BOOLEAN, acutal: ${resolvedCond.tpe}")
          else {
            val ifTrueTpe  = resolvedIfTrue.tpe
            val ifFalseTpe = resolvedIfFalse.tpe
            TypeInferrer.findCommonType(ifTrueTpe, ifFalseTpe) match {
              case Some(tpe) =>
                Right(
                  IF(
                    cond = resolvedCond,
                    ifTrue = resolvedIfTrue,
                    ifFalse = resolvedIfFalse,
                    tpe = tpe
                  ))
              case None => Left(s"Can't find common type for $ifTrueTpe and $ifFalseTpe")
            }
          }
      }

  private def compileFunctionCall(ctx: CompilerContext, fc: Expressions.FUNCTION_CALL): SetTypeResult[EXPR] = {
    val Expressions.FUNCTION_CALL(_, _, name, args) = fc
    for {
      name <- EitherT.fromEither[Coeval](name.toEither)
      r <- ctx.functionTypeSignaturesByName(name) match {
        case Nil                   => EitherT.fromEither[Coeval](Left(s"Function '$name' not found"))
        case singleOverload :: Nil => resolvedFuncArguments(ctx, args).subflatMap(matchFuncOverload(name, args, _, singleOverload))
        case many =>
          resolvedFuncArguments(ctx, args).subflatMap { resolvedArgs =>
            val matchedSignatures = many
              .zip(many.map(matchFuncOverload(name, args, resolvedArgs, _)))
              .collect {
                case (sig, result) if result.isRight => (sig, result)
              }

            matchedSignatures match {
              case Nil                       => Left(s"Can't find a function '$name'(${resolvedArgs.map(_.tpe.typeInfo).mkString(", ")})")
              case (_, oneFuncResult) :: Nil => oneFuncResult
              case manyPairs =>
                val candidates = manyPairs.map { case (sig, _) => s"'$name'(${sig.args.mkString(", ")})" }
                Left(s"Can't choose an overloaded function. Candidates: ${candidates.mkString("; ")}")
            }
          }
      }
    } yield r
  }

  private def compileBlock(ctx: CompilerContext, block: Expressions.BLOCK): SetTypeResult[EXPR] =
    for {
      letName <- EitherT.fromEither[Coeval](block.let.name.toEither)
      r <- (ctx.varDefs.get(letName), ctx.functionDefs.get(letName)) match {
        case (Some(_), _) => EitherT.leftT[Coeval, EXPR](s"Value '$letName' already defined in the scope")
        case (_, Some(_)) =>
          EitherT.leftT[Coeval, EXPR](s"Value '$letName' can't be defined because function with such name is predefined")
        case (None, None) =>
          import block.let
          for {
            exprTpe  <- compile(ctx, let.value)
            letTypes <- EitherT.fromEither[Coeval](let.types.map(_.toEither).toList.sequence[CompilationResult, String])
            _ <- EitherT.cond[Coeval](
              letTypes.forall(ctx.predefTypes.contains),
              (),
              s"Value '$letName' declared as non-existing type $letTypes, while all possible types are ${ctx.predefTypes}"
            )
            desiredUnion = if (let.types.isEmpty) exprTpe.tpe else UNION(letTypes.map(CASETYPEREF))
            updatedCtx   = ctx.copy(varDefs = ctx.varDefs + (letName -> desiredUnion))
            inExpr <- compile(updatedCtx, block.body)
          } yield
            BLOCK(
              let = LET(letName, exprTpe),
              body = inExpr,
              tpe = inExpr.tpe
            )
      }
    } yield r

  private def compileRef(ctx: CompilerContext, ref: Expressions.REF): SetTypeResult[EXPR] = EitherT.fromEither {
    ref.key.toEither.flatMap { key =>
      ctx.varDefs
        .get(key)
        .map(REF(key, _))
        .toRight(s"A definition of '$key' is not found")
    }
  }

  private def compileMatch(ctx: CompilerContext, m: Expressions.MATCH): SetTypeResult[EXPR] = {
    val Expressions.MATCH(_, _, expr, cases) = m
    val rootMatchTmpArg                      = "$match" + ctx.tmpArgsIdx
    val updatedCtx                           = ctx.copy(tmpArgsIdx = ctx.tmpArgsIdx + 1)
    val typeDefs = ctx.predefTypes
    def flat(tl: List[String]): List[String] = {
      tl.flatMap {
        t => typeDefs.get(t).fold(List(t)) {
          case UnionType(_, tl, _) => tl.map(_.name)
          case t => List(t.name)
        }
      }
    }

    for {
      typedExpr <- compile(ctx, expr)
      possibleExpressionTypes <- EitherT.fromEither[Coeval](typedExpr.tpe match {
        case u: UNION => Right(u)
        case _        => Left("Only union type can be matched")
      })
      matchingTypes <- EitherT.fromEither[Coeval](cases.flatMap(_.types).map(_.toEither).toList.sequence[CompilationResult, String])
      matchedTypes = UNION(flat(matchingTypes).map(CASETYPEREF))
      lastEmpty    = cases.last.types.isEmpty
      _ <- EitherT.cond[Coeval](
        lastEmpty && (possibleExpressionTypes >= matchedTypes) || (possibleExpressionTypes equivalent matchedTypes),
        (),
        s"Matching not exhaustive: possibleTypes are ${possibleExpressionTypes.l}, while matched are $matchingTypes"
      )
      refTmp = Expressions.REF(1, 1, PART.VALID(1, 1, rootMatchTmpArg))
      ifBasedCases: Expressions.EXPR =
       cases.foldRight(Expressions.REF(1, 1, PART.VALID(1, 1, PureContext.errRef)): Expressions.EXPR) {
        case (mc, further) =>
          val blockWithNewVar = mc.newVarName match {
            case Some(newVal) => Expressions.BLOCK(1, 1, Expressions.LET(1, 1, newVal, refTmp, mc.types), mc.expr)
            case None         => mc.expr
          }
           mc.types.toList match {
             case List() => blockWithNewVar
             case types =>
                val cond =
                 Expressions.FUNCTION_CALL(1, 1, PART.VALID(1, 1, PureContext._isInstanceOf.name), List(refTmp, Expressions.TYPELIST(1, 1, flat(types.map(_.asInstanceOf[PART.VALID[String]].v)))))
                Expressions.IF(1, 1, cond, blockWithNewVar, further) 
           }
      }
      compiled <- compileBlock(updatedCtx,
                               Expressions.BLOCK(1, 1, Expressions.LET(1, 1, PART.VALID(1, 1, rootMatchTmpArg), expr, Seq.empty), ifBasedCases))
    } yield compiled
  }

  private def resolvedFuncArguments(ctx: CompilerContext, args: List[Expressions.EXPR]): ResolvedArgsResult = {
    import cats.instances.list._
    val r: List[SetTypeResult[EXPR]] = args.map(arg => compile(ctx, arg))(collection.breakOut)
    r.sequence[SetTypeResult, EXPR]
  }

  private def matchFuncOverload(funcName: String,
                                funcArgs: List[Expressions.EXPR],
                                resolvedArgs: List[EXPR],
                                f: FunctionTypeSignature): Either[String, EXPR] = {
    val argTypes   = f.args
    val resultType = f.result
    if (funcArgs.lengthCompare(argTypes.size) != 0)
      Left(s"Function '$funcName' requires ${argTypes.size} arguments, but ${funcArgs.size} are provided")
    else {
      val typedExpressionArgumentsAndTypedPlaceholders: List[(EXPR, TYPEPLACEHOLDER)] = resolvedArgs.zip(argTypes)

      val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case (typedExpr, tph) => (typedExpr.tpe, tph) }
      for {
        resolvedTypeParams <- TypeInferrer(typePairs)
        resolvedResultType <- TypeInferrer.inferResultType(resultType, resolvedTypeParams)
      } yield
        FUNCTION_CALL(
          FunctionHeader(funcName, f.args.map(FunctionHeaderType.fromTypePlaceholder)),
          typedExpressionArgumentsAndTypedPlaceholders.map(_._1),
          resolvedResultType
        )
    }
  }

  private def handlePart[T](part: PART[T])(f: T => EXPR): SetTypeResult[EXPR] = part match {
    case PART.VALID(_, _, x)               => EitherT.pure(f(x))
    case PART.INVALID(start, end, message) => EitherT.leftT[Coeval, EXPR](s"$message in $start-$end")
  }

  def apply(c: CompilerContext, expr: Expressions.EXPR): CompilationResult[EXPR] = {
    def result = compile(c, expr).value().left.map { e =>
      s"Typecheck failed: $e"
    }
    Try(result) match {
      case scala.util.Failure(ex)  => Left(ex.toString)
      case scala.util.Success(res) => res
    }
  }
}
