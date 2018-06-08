package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF

case class CaseObj(caseType: CASETYPEREF, fields: Map[String, Any])
