package settings

import java.io.File
import java.net.InetAddress
import java.util

import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.JSONValue

import network.Peer
import scala.util.Try
import scala.collection.JavaConversions._


object Settings {

	//NETWORK
	private val DEFAULT_MIN_CONNECTIONS = 5
	private val DEFAULT_MAX_CONNECTIONS = 20
	private val DEFAULT_CONNECTION_TIMEOUT = 60000
	private val DEFAULT_PING_INTERVAL = 30000
	
	//RPC
	private val DEFAULT_RPC_PORT = 9085
	private val DEFAULT_RPC_ALLOWED = "127.0.0.1"
	
	//DATA
	private val DEFAULT_DATA_DIR = "data"
	private val DEFAULT_WALLET_DIR = "wallet"
	private val DEFAULT_GENERATOR_KEY_CACHING = false
	private val DEFAULT_MAX_BYTE_PER_FEE = 512

	private lazy val settingsJSONTry = Try{
			val jsonString = scala.io.Source.fromFile("settings.json").mkString
			//CREATE JSON OBJECT
			JSONValue.parse(jsonString).asInstanceOf[JSONObject]
	}

	settingsJSONTry.recover{case _:Throwable =>
			//STOP
			System.out.println("ERROR reading settings.json, closing")
			System.exit(0)
	}

	private lazy val settingsJSON = settingsJSONTry.get

	def getKnownPeers = Try{
			//GET PEERS FROM JSON
		settingsJSON.get("knownpeers").asInstanceOf[util.ArrayList[_]].flatMap{addr =>
				val address = InetAddress.getByName(addr.asInstanceOf[String])
				if (address == InetAddress.getLocalHost) None else Some(new Peer(address))
			}
		}.getOrElse(Seq[Peer]())

	def getMaxConnections = settingsJSON.containsKey("maxconnections") match {
		case true =>  settingsJSON.get("maxconnections").asInstanceOf[Long].intValue()
		case false => DEFAULT_MAX_CONNECTIONS
	}
	
	def getMinConnections = settingsJSON.containsKey("minconnections") match {
		case true => settingsJSON.get("minconnections").asInstanceOf[Long].intValue()
		case false => DEFAULT_MIN_CONNECTIONS
	}

	def getConnectionTimeout = settingsJSON.containsKey("connectiontimeout") match {
		case true =>  settingsJSON.get("connectiontimeout").asInstanceOf[Long].intValue()
		case false => DEFAULT_CONNECTION_TIMEOUT
	}

	def getRpcPort = settingsJSON.containsKey("rpcport") match {
		case true =>  settingsJSON.get("rpcport").asInstanceOf[Long].intValue()
		case false => DEFAULT_RPC_PORT
	}

	def getRpcAllowed:Seq[String] = {
		settingsJSON.containsKey("rpcallowed") match{
			case true => Try {
				settingsJSON.get("rpcallowed").asInstanceOf[util.ArrayList[_]].map(_.asInstanceOf[String])
			}.getOrElse(Seq[String]())
			case false => DEFAULT_RPC_ALLOWED.split("")
		}
	}
	
	def getWalletDir = settingsJSON.containsKey("walletdir") match {
		case true =>  settingsJSON.get("walletdir").asInstanceOf[String]
		case false => DEFAULT_WALLET_DIR
	}
	
	def getDataDir = settingsJSON.containsKey("datadir") match {
		case true =>  settingsJSON.get("datadir").asInstanceOf[String]
		case false => DEFAULT_DATA_DIR
	}
	
	def getPingInterval = settingsJSON.containsKey("pinginterval") match {
		case true =>  settingsJSON.get("pinginterval").asInstanceOf[Long].intValue()
		case false => DEFAULT_PING_INTERVAL
	}

	def isGeneratorKeyCachingEnabled = settingsJSON.containsKey("generatorkeycaching") match {
		case true =>  settingsJSON.get("generatorkeycaching").asInstanceOf[Boolean]
		case false => DEFAULT_GENERATOR_KEY_CACHING
	}

	def getMaxBytePerFee = settingsJSON.containsKey("maxbyteperfee") match {
		case true =>  settingsJSON.get("maxbyteperfee").asInstanceOf[Long].intValue()
		case false => DEFAULT_MAX_BYTE_PER_FEE
	}
}