case class DocSourceData(vars: List[VarSourceData], funcs: List[FuncSourceData])

case class VarSourceData(name: String, doc: String)

case class FuncSourceData(name: String, params: List[String], doc: String, paramsDoc: List[String])
