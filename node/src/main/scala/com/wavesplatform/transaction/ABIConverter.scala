package com.wavesplatform.transaction

import com.esaulpaugh.headlong.abi.{Function, Tuple}
import com.esaulpaugh.headlong.util.FastHex
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.{Terms, Types}
import com.wavesplatform.transaction.ABIConverter.WavesByteRepr
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.web3j.abi.TypeReference
import org.web3j.abi.datatypes.Type
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import scala.jdk.CollectionConverters._

object ABIConverter {
  val WavesByteRepr: ByteStr      = ByteStr(new Array[Byte](32))
  val PaymentListType: Types.LIST = Types.LIST(Types.TUPLE(List(Types.BYTESTR, Types.LONG)))
  val PaymentArgSignature: String = "(bytes32,int64)[]"
  val PaymentArgJson: JsObject = Json.obj(
    "name" -> "payments",
    "type" -> "tuple[]",
    "components" -> Json.arr(
      Json.obj("name" -> "assetId", "type" -> "bytes32"),
      Json.obj("name" -> "amount", "type"  -> "int64")
    )
  )

  private def buildMethodId(str: String): String = {
    val cls    = Class.forName("org.web3j.abi.FunctionEncoder")
    val method = cls.getDeclaredMethod("buildMethodId", classOf[String])
    method.setAccessible(true)
    method.invoke(null, str).asInstanceOf[String]
  }

  def ethType(argType: Types.FINAL): String =
    (ethTypeObj(argType) \ "type").as[String]

  def ethTypeObj(argType: Types.FINAL): JsObject = {
    def t(s: String) = Json.obj("type" -> s)

    argType match {
      case Types.BOOLEAN => t("bool")
      case Types.LONG    => t("int64")
      case Types.BYTESTR => t("bytes")
      case Types.STRING  => t("string")
      case Types.LIST(innerType) =>
        val base = ethTypeObj(innerType)
        if (base.value("type").asInstanceOf[JsString].value == "tuple") {
          Json.obj(
            "type"       -> "tuple[]",
            "components" -> base.value("components")
          )
        } else {
          Json.obj("type" -> (base.value("type").as[String] + "[]"))
        }

      case Types.UNION(typeList, _) =>
        t("tuple") ++ Json.obj(
          "components" -> {
            // Index start from 0 and count from element after typeIndex (from second element of the tuple)
            Json.obj("name" -> s"typeIndex", "type" -> "uint8") ::
              typeList.map(t => Json.obj("name" -> t.name) ++ ethTypeObj(t))
          }
        )

      // only for payments
      case Types.TUPLE(types) =>
        t("tuple") ++ Json.obj(
          "components" -> types.map(t => Json.obj("name" -> t.name) ++ ethTypeObj(t))
        )

      case other => throw new IllegalArgumentException(s"ethTypeObj: Unexpected type: $other")
    }
  }

  def ethFuncSignatureTypeName(argType: Types.FINAL): String = argType match {
    case Types.BOOLEAN         => "bool"
    case Types.LONG            => "int64"
    case Types.BYTESTR         => "bytes"
    case Types.STRING          => "string"
    case Types.LIST(innerType) => s"${ethFuncSignatureTypeName(innerType)}[]"
    case Types.UNION(typeList, _) =>
      val unionElementIdxType = "uint8"
      val typeNameList        = unionElementIdxType :: typeList.map(ethFuncSignatureTypeName)
      s"(${typeNameList.mkString(",")})"
    case Types.TUPLE(types) => s"(${types.map(ethFuncSignatureTypeName).mkString(",")})"
    case other              => throw new IllegalArgumentException(s"ethFuncSignatureTypeName: Unexpected type: $other")
  }

  def toRideValue(ethArg: Any, rideType: Types.FINAL): EVALUATED = ethArg match {
    case bool: Boolean        => Terms.CONST_BOOLEAN(bool)
    case i: Int               => Terms.CONST_LONG(i)
    case l: Long              => Terms.CONST_LONG(l)
    case byteArr: Array[Byte] => Terms.CONST_BYTESTR(ByteStr(byteArr)).explicitGet() //FastHex.encodeToString(byteArr, 0, byteArr.length)
    case str: String          => Terms.CONST_STRING(str).explicitGet()

    case arr: Array[_] => {
      val innerType = rideType match {
        case list: Types.LIST =>
          list.innerType
        case _ =>
          Types.ANY
      }
      Terms
        .ARR(
          arr.toVector.map(el => toRideValue(el, innerType)),
          limited = true
        )
        .explicitGet()
    }

    case t: Tuple if rideType.isInstanceOf[Types.UNION] => {
      val tupleValList = t.asScala.toVector
      if (tupleValList.nonEmpty) {
        val unionSubtypeIdx: Int = tupleValList.head.asInstanceOf[Int]
        if (unionSubtypeIdx < tupleValList.length) {
          toRideValue(tupleValList(unionSubtypeIdx + 1), rideType.asInstanceOf[Types.UNION].typeList(unionSubtypeIdx))
        } else {
          throw new UnsupportedOperationException(s"Incorrect tuple size for Union type.")
        }
      } else {
        throw new UnsupportedOperationException(s"Incorrect tuple size for Union type. Empty tuple.")
      }
    }

    case t: Tuple =>
      Terms
        .ARR(
          t.asScala.toVector.map(el => toRideValue(el, Types.ANY)),
          limited = true
        )
        .explicitGet()

    case _ => throw new UnsupportedOperationException(s"Type not supported: $ethArg")
  }
}

final case class ABIConverter(script: Script) {
  case class FunctionArg(name: String, rideType: Types.FINAL) {
    lazy val ethType: String               = ABIConverter.ethType(rideType)
    def ethTypeRef: TypeReference[Type[_]] = TypeReference.makeTypeReference(ethType).asInstanceOf[TypeReference[Type[_]]]
  }

  case class FunctionRef(name: String, args: Seq[FunctionArg]) {

    def decodeArgs(data: String): (List[EVALUATED], Seq[InvokeScriptTransaction.Payment]) = {
      val ethFunc     = new Function(ethSignature)
      val ethArgsList = ethFunc.decodeCall(FastHex.decode(data)).asScala.toList

      val result =
        ethArgsList.zip(args.map(_.rideType) :+ ABIConverter.PaymentListType).map { case (ethArg, rideT) => ABIConverter.toRideValue(ethArg, rideT) }

      val payment = result.last match {
        case Terms.ARR(xs) =>
          xs.map {
            case Terms.ARR(fields) =>
              fields match {
                case Seq(Terms.CONST_BYTESTR(assetId), Terms.CONST_LONG(amount)) =>
                  InvokeScriptTransaction.Payment(amount, assetId match {
                    case `WavesByteRepr` => Asset.Waves
                    case assetId         => Asset.IssuedAsset(assetId)
                  })

                case other => throw new IllegalArgumentException(s"decodeArgs: unexpected term in payment: $other")
              }
            case other => throw new IllegalArgumentException(s"decodeArgs: unexpected term in payment: $other")
          }

        case _ => Nil
      }
      (result.init, payment)
    }

    lazy val ethSignature: String = {
      val argTypes = args.map(_.rideType).map(ABIConverter.ethFuncSignatureTypeName) :+ ABIConverter.PaymentArgSignature
      s"$name(${argTypes.mkString(",")})"
    }

    lazy val ethMethodId: String = ABIConverter.buildMethodId(ethSignature)
  }

  private[this] lazy val funcsWithTypes = Global.dAppFuncTypes(script)

  private[this] def functionsWithArgs: Seq[(String, List[(String, Types.FINAL)])] = {
    funcsWithTypes match {
      case Right(signatures) => signatures.argsWithFuncName
      case Left(_)           => Nil
    }
  }

  lazy val funcByMethodId: Map[String, FunctionRef] =
    functionsWithArgs
      .map {
        case (funcName, args) =>
          FunctionRef(funcName, args.map { case (name, argType) => FunctionArg(name, argType) })
      }
      .map(func => func.ethMethodId -> func)
      .toMap

  def jsonABI: JsArray =
    JsArray(functionsWithArgs.map {
      case (funcName, args) =>
        val inputs = args.map {
          case (argName, argType) =>
            Json.obj("name" -> argName) ++ ABIConverter.ethTypeObj(argType)
        } :+ ABIConverter.PaymentArgJson

        Json.obj(
          "name"            -> funcName,
          "type"            -> "function",
          "constant"        -> false,
          "payable"         -> false,
          "stateMutability" -> "nonpayable",
          "inputs"          -> inputs,
          "outputs"         -> JsArray.empty
        )
    })

  def decodeFunctionCall(data: String): (FUNCTION_CALL, Seq[InvokeScriptTransaction.Payment]) = {
    val methodId        = data.substring(0, 8)
    val function        = funcByMethodId.getOrElse("0x" + methodId, throw new NoSuchElementException(s"Function not defined: $methodId"))
    val (args, payment) = function.decodeArgs(data)
    (FUNCTION_CALL(FunctionHeader.User(function.name), args), payment)
  }
}
