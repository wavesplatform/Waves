On the Way to a Modular Cryptocurrency, Part 2: Stackable API
=============================================================


Introduction
------------

The previous chapter, [Generic Block Structure](modular1.md) described how to split a blockchain-related
 core design of a cryptocurrency into two separate modules to wire concrete implementations in an application
 then.
 
 A cryptocurrency core application provides some API for its user. In this chapter I will show how to 
 split API implementation into pieces to wire it in an application then. 

Gluing Things Together
----------------------

In the first place, some wrapper for Akka-http route is needed:

    trait ApiRoute {
       val route: akka.http.scaladsl.server.Route
    }

Then an actor composing routes:

    class CompositeHttpServiceActor(val routes: ApiRoute*) extends Actor with HttpService {
     
      override def actorRefFactory = context
           
      override def receive = routes.map(_.route).reduce(_ ~ _)
    }
    
And then to create a new piece of API, instance of ApiRoute overriding `route` value is needed, see "Longer
     Example" in [akka-http documentation](http://doc.akka.io/docs/akka/2.4.4/scala/http/routing-dsl/index.html#Longer_Example) for example of
     a route definition(or Scorex Lagonaki sources, scorex.api.http package).
      
Then to glue all things together, we just create concrete actor implementation and bind it to a port. Example from
Scorex Lagonaki:
      
    lazy val routes = Seq(
      AddressApiRoute()(wallet, storedState),
      BlocksApiRoute()(blockchainImpl, wallet),
      TransactionsApiRoute(storedState),
      WalletApiRoute()(wallet),
      PaymentApiRoute(this),
      PaymentApiRoute(this),
      ScorexApiRoute(this),
      SeedApiRoute
    )

    lazy val apiActor = actorSystem.actorOf(Props(classOf[CompositeHttpServiceActor], routes), "api")      
    
    IO(Http) ! Http.Bind(apiActor, interface = "0.0.0.0", port = settings.rpcPort)       

