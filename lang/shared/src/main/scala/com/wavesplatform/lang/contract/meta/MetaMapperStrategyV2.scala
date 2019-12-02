package com.wavesplatform.lang.contract.meta
import com.google.protobuf.ByteString
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import cats.implicits._

import scala.collection.immutable.ListMap

object MetaMapperStrategyV2 extends MetaMapperStrategy[V2.type] {
  def toProto(funcTypes: List[List[FINAL]]): Either[String, DAppMeta] =
    funcTypes
      .traverse(funcToProto)
      .map(DAppMeta(2, _))

  private def funcToProto(types: List[FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse(mapType)
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(_))

  private def mapType(t: FINAL): Either[String, Byte] = {
    val result = t match {
      case LIST(t) => mapSingleType(t).map(_ + (1 << 4))
      case t       => mapSingleType(t)
    }
    result.map(_.toByte)
  }

  private def mapSingleType(t: FINAL): Either[String, Int] =
    t match {
      case UNION(types, _) => types.traverse(mapPrimitiveType).map(_.sum)
      case argType         => mapPrimitiveType(argType)
    }

  private def mapPrimitiveType(t: FINAL): Either[String, Int] =
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
    if (b > 31 || b < 1) {
      Left("Illegal callable func arg type bits")
    } else {
      val existingTypes = definedTypes.flatMap(checkTypeExistence(b, _))
      existingTypes match {
        case List(single)  => Right(single)
        case l@List(_, _*) => Right(UNION(l, None))
        case Nil           => Left("Unexpected callable func arg type absence")
      }
    }
  }

  private lazy val definedTypes = List(LONG, BYTESTR, BOOLEAN, STRING)

  private lazy val typeOrder: Map[REAL, Int] =
    definedTypes.zipWithIndex.toMap

  private def checkTypeExistence(b: Byte, t: REAL): Option[REAL] = {
    val order = typeOrder(t)
    if ((b >> order) % 2 == 1)
      if ((b >> 4) % 2 == 1) Some(LIST(t))
      else Some(t)
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
