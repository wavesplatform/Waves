package com.wavesplatform.protobuf.dapp

import com.google.protobuf.ByteString
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader._
import com.wavesplatform.lang.v1.compiler.Terms._

object PBDApps {

  def pbEvaluated(evaluated: EVALUATED): PBEvaluated = {
    val internalValue = evaluated match {
      case CONST_LONG(v) => PBEvaluated.Value.ConstLong(v)
      case CONST_BIGINT(v) => PBEvaluated.Value.ConstBigInt(ByteString.copyFrom(v.toByteArray))
      case CONST_BOOLEAN(v) => PBEvaluated.Value.ConstBoolean(v)
      case CONST_BYTESTR(v) => PBEvaluated.Value.ConstByteStr(ByteString.copyFrom(v.arr))
      case CONST_STRING(v) => PBEvaluated.Value.ConstString(v)
      case ARR(v) => PBEvaluated.Value.Arr(PBArr(v.map(pbEvaluated)))
      case CaseObj(caseType, fields) => PBEvaluated.Value.CaseObj(PBCaseObj(caseType.name, fields.view.mapValues(pbEvaluated).toMap))
      case FAIL(v) => PBEvaluated.Value.Fail(v)
    }
    PBEvaluated(internalValue)
  }

  def pbExpr(expr: EXPR): PBExpr = {
    val internalValue = expr match {
      case ev: EVALUATED => PBExpr.Value.Evaluated(pbEvaluated(ev))
      case BLOCK(dec, body) => PBExpr.Value.Block(PBBlock(Some(pbDeclaration(dec)), Some(pbExpr(body))))
      case FAILED_EXPR() => PBExpr.Value.Empty
      case FUNCTION_CALL(function, args) => PBExpr.Value.FunctionCall(PBFunctionCall(Some(pbFunctionHeader(function)), args.map(pbExpr)))
      case GETTER(expr, field) => PBExpr.Value.Getter(PBGetter(Some(pbExpr(expr)), field))
      case IF(cond, ifTrue, ifFalse) => PBExpr.Value.If(PBIf(Some(pbExpr(cond)), Some(pbExpr(ifTrue)), Some(pbExpr(ifFalse))))
      case LET_BLOCK(let, body) => PBExpr.Value.LetBlock(PBLetBlock(Some(PBLet(let.name, Some(pbExpr(let.value)))), Some(pbExpr(body))))
      case REF(key) => PBExpr.Value.Ref(key)
    }
    PBExpr(internalValue)
  }

  def pbFunctionHeader(functionHeader: FunctionHeader): PBFunctionCall.FunctionHeader = {
    val internalValue = functionHeader match {
      case Native(name) => PBFunctionCall.FunctionHeader.Value.Native(name)
      case User(internalName, name) => PBFunctionCall.FunctionHeader.Value.User(PBFunctionCall.FunctionHeader.User(internalName, name))
    }
    PBFunctionCall.FunctionHeader(internalValue)
  }

  def pbDeclaration(dec: DECLARATION): PBDeclaration = {
    val internalValue = dec match {
      case FAILED_DEC() => PBDeclaration.Value.Empty
      case FUNC(name, args, body) => PBDeclaration.Value.Func(PBFunc(name, args, Some(pbExpr(body))))
      case LET(name, value) => PBDeclaration.Value.Let(PBLet(name, Some(pbExpr(value))))
    }
    PBDeclaration(internalValue)
  }

  def pbDApp(dApp: DApp): PBDapp =
    PBDapp(
      meta = dApp.meta.toByteString,
      decs = dApp.decs.map(pbDeclaration),
      callableFuncs = dApp.callableFuncs.map { func =>
        PBFunction(func.annotation.invocationArgName, Some(PBFunc(func.u.name, func.u.args, Some(pbExpr(func.u.body)))))
      },
      verifierFuncOpt = dApp.verifierFuncOpt.map { vFunc =>
        PBFunction(vFunc.annotation.invocationArgName, Some(PBFunc(vFunc.u.name, vFunc.u.args, Some(pbExpr(vFunc.u.body)))))
      }
    )
}
