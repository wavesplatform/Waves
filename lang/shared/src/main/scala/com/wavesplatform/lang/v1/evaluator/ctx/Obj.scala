package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.CASETYPEREF

case class CaseObj(caseType: CASETYPEREF, fields: Map[String, Any])
