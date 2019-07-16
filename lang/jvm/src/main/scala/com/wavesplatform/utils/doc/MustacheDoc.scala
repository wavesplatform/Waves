package com.wavesplatform.utils.doc

import com.wavesplatform.lang.v1.compiler.Types._

import scala.collection.JavaConverters._

abstract class TypeDoc {
  val name: String
  val isUnion: Boolean   = false
  val isObj: Boolean     = false
  val isNative: Boolean  = false
  val haveParam: Boolean = false
  val isComplex: Boolean = false
  val needLink: Boolean  = false
  def noLink: Boolean    = !needLink

  val verticalLineSymbol  = "&#124;"
  lazy val mdName: String = name.replace("|", verticalLineSymbol)
}
object TypeDoc {
  def apply(t: TYPE): TypeDoc = t match {
    case t: FINAL                         => typeRepr(t)()
    case TYPEPARAM(char: Byte)            => new TypeDoc { val name: String = char.toChar.toString; override val isComplex: Boolean = true }
    case PARAMETERIZEDLIST(l)             => ListOf(apply(l))
    case PARAMETERIZEDUNION(Seq(UNIT, l)) => OptionOf(apply(l))
    case PARAMETERIZEDUNION(Seq(l, UNIT)) => OptionOf(apply(l))
    case PARAMETERIZEDUNION(l)            => UnionDoc(t.toString, l.map(apply).asJava)
    case t                                => new TypeDoc { val name: String = t.toString; override val isComplex: Boolean = true }
  }

  def typeRepr(t: FINAL)(name: String = t.name): TypeDoc = t match {
    case UNION(Seq(UNIT, l), _) => OptionOf(typeRepr(l)())
    case UNION(Seq(l, UNIT), _) => OptionOf(typeRepr(l)())
    case UNION(l, _)            => UnionDoc(name, l.map(t => typeRepr(t)()).asJava)
    case CASETYPEREF(_, fields) =>
      objDoc(name, fields.map(f => Field(f._1, typeRepr(f._2)())).asJava)
    case LIST(t) => ListOf(typeRepr(t)())
    case t       => nativeTypeDoc(t.toString)
  }
}

case class Field(name: String, `type`: TypeDoc)
case class UnionDoc(override val name: String, union: java.util.List[TypeDoc]) extends TypeDoc {
  override val isUnion: Boolean = true
}
case class objDoc(override val name: String, fields: java.util.List[Field]) extends TypeDoc {
  override val isObj: Boolean    = true
  override val needLink: Boolean = true
}
case class ListOf(param: TypeDoc)   extends TypeDoc { override val name: String = s"List[${param.name}]"; val hasParam: Boolean = true }
case class OptionOf(param: TypeDoc) extends TypeDoc { override val name: String = s"${param.name}|Unit"; val hasParam: Boolean  = true }
case class nativeTypeDoc(override val name: String) extends TypeDoc {
  override val isNative: Boolean = true
  override val needLink: Boolean = true
}

case class VarDoc(private val nameRaw: String, `type`: TypeDoc, doc: String) {
  val name = nameRaw.replace("@", "")
}
case class FuncDoc(name: String, `type`: TypeDoc, doc: String, params: java.util.List[VarDoc], cost: String)

case class TransactionDoc(name: String, fields: java.util.List[TransactionField])
case class TransactionField(absend: Boolean, `type`: java.util.List[TypeDoc])
case class FieldTypes(name: String, types: java.util.List[TransactionField])
case class CaseDoc(types: java.util.List[TransactionDoc], fields: java.util.List[FieldTypes])
case class Special(`class`: String, descr: CaseDoc)
case class Doc(
    types: java.util.List[TypeDoc],
    vars: java.util.List[VarDoc],
    funcs: java.util.List[FuncDoc],
    transactionDoc: java.util.List[TransactionDoc],
    transactionFields: java.util.List[FieldTypes],
    commonFields: CaseDoc,
    specials: java.util.List[Special]
)
object Doc {
  def apply(d: (Seq[TransactionDoc], Seq[FieldTypes])) = CaseDoc(d._1.asJava, d._2.asJava)
}

case class FuncDocV3(funcDoc: FuncDoc, index: Int) {
  val name          = funcDoc.name
  val `type`        = funcDoc.`type`
  val doc           = funcDoc.doc
  val params        = funcDoc.params
  val cost          = funcDoc.cost
  val paramTypes    = params.asScala.map(p => p.`type`.mdName).mkString(", ")
  val paramArgTypes = params.asScala.map(p => s"${p.name}: ${p.`type`.name}").mkString(", ")
  val anchor        = name + params.asScala.map(_.`type`.mdName).mkString
}
case class CategorizedFuncsDoc(funcs: java.util.List[FuncDocV3], category: String)
