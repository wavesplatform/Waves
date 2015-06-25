package scorex.settings

import scorex.consensus._

/*
  System constants here.
 */

object Constants {

  val Product = "Scorex"
  val Release = "Lagonaki"
  val Version = (1, 0, 0)
  val VersionString = s"${Version._1}.${Version._2}.${Version._3}"

  val AgentName = s"$Product - $Release v. $VersionString"

  //Change to ConsensusModuleNxt to have Nxt-like forging
  val ConsensusAlgo: ConsensusModule = ConsensusModuleNxt
}
