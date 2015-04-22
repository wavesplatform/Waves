package settings

import scorex.consensus._

/*
  System constants here.
 */

object Constants {
  
  val Product = "Scorex"
  val Release = "Lagonaki"
  val Version = (0,0,2)

  val AgentName = s"$Product - $Release v. ${Version._1}.${Version._2}.${Version._3}"

  val ConsensusAlgo:ConsensusModule = ConsensusModuleNxt
}
