package com.wavesplatform.lang.contract.meta

import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, FINAL, LONG, REAL, STRING, UNION}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import cats.implicits._
import com.wavesplatform.lang.contract.DApp

import scala.collection.immutable.ListMap

object MetaMapperStrategyV1 extends MetaMapperStrategy[V1.type] {
  def toProto(funcTypes: List[List[FINAL]]): Either[String, DAppMeta] =
    funcTypes
      .traverse(funcToProto)
      .map(DAppMeta(1, _))

  private def funcToProto(types: List[FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse(mapType)
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(_))

  private def mapType(t: FINAL): Either[String, Byte] = {
    val result = t match {
      case UNION(types, _) => types.traverse(mapSingleType).map(_.sum)
      case argType         => mapSingleType(argType)
    }
    result.map(_.toByte)
  }

  private def mapSingleType(t: FINAL): Either[String, Int] =
    t match {
      case LONG    => Right(1 << typeOrder(LONG))
      case BYTESTR => Right(1 << typeOrder(BYTESTR))
      case BOOLEAN => Right(1 << typeOrder(BOOLEAN))
      case STRING  => Right(1 << typeOrder(STRING))
      case argType => Left(s"Unexpected callable func arg type: $argType")
    }

  def fromProto(meta: DAppMeta): Either[String, List[List[FINAL]]] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, List[FINAL]] =
    funcs.types.toByteArray.toList
      .traverse(buildType)

  private def buildType(b: Byte): Either[String, FINAL] = {
    if (b > 15 || b < 1) {
      Left("Illegal callable func arg type bits")
    } else {
      val existingTypes = definedTypes
        .map(checkTypeExistence(b, _))
        .flatMap(_.toList)

      existingTypes match {
        case List(single)  => Right(single)
        case l@List(_, _*) => Right(UNION(l, None))
        case Nil           => Left("Unexpected callable func arg type absence")
      }
    }
  }

  private lazy val definedTypes = List(LONG, BYTESTR, BOOLEAN, STRING)

  private lazy val typeOrder: Map[REAL, Int] =
    definedTypes
      .mapWithIndex((_, _))
      .toMap

  private def checkTypeExistence(b: Byte, t: REAL): Option[REAL] = {
    val order = typeOrder(t)
    if (((b & Math.pow(10, order).toInt) >> order) == 1) Some(t)
    else None
  }

  val FieldName = "callableFuncTypes"

  override def textMap(data: List[List[FINAL]], dapp: DApp): Dic = {
    val argsWithTypes = (data zip dapp.callableFuncs.map(_.u.args))
      .map { case (types, args) => args zip types }

    val funcTypesJson = argsWithTypes.map(typedArgs =>
      Dic(
        ListMap(typedArgs : _*).mapValues(t => Single(t.name))
      )
    )
    Dic(Map(FieldName -> Chain(funcTypesJson)))
  }
}
