package settings

import scorex.consensus.qora.QoraBlockGenerationData

/*
  System constants here.
 */

object Constants {
  type ConsensusInjectedBlockPart = QoraBlockGenerationData
  
  val Product = "Scorex"
  val Release = "Lagonaki"
  val Version = (0,0,2)

  val AgentName = s"$Product - $Release v. ${Version._1}.${Version._2}.${Version._3}"
}
