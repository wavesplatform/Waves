package api;
/*

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.IPAccessHandler;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;

import settings.Settings;

public class ApiService {

	public Server server;
	
	public ApiService()
	{
		//CREATE CONFIG
		Set<Class<?>> s = new HashSet<Class<?>>();
        s.add(QoraResource.class);     
        s.add(SeedResource.class);  
        s.add(PeersResource.class);    
        s.add(TransactionsResource.class);
        s.add(BlocksResource.class);
        s.add(AddressesResource.class);
        s.add(WalletResource.class);
        s.add(PaymentResource.class);
        ResourceConfig config = new ResourceConfig(s);
		
        //CREATE CONTAINER
        ServletContainer container = new ServletContainer(config);
		
		//CREATE CONTEXT
        ServletContextHandler context = new ServletContextHandler();
        context.setContextPath("/");
        context.addServlet(new ServletHolder(container),"/*");
        
        //CREATE WHITELIST
        IPAccessHandler accessHandler = new IPAccessHandler();
        //todo: kushti: uncomment! accessHandler.setWhite(Settings.getRpcAllowed());
        accessHandler.setHandler(context);
        
        //CREATE RPC SERVER
      	this.server = new Server(Settings.getRpcPort());
      	this.server.setHandler(accessHandler);
	}
	
	public void start()
	{
		try
        {
        	//START RPC 
			server.start();
		} 
        catch (Exception e) 
		{
        	//FAILED TO START RPC
		}
	}
	
	public void stop()
	{
		try 
        {
			//STOP RPC  
			server.stop();
		} 
        catch (Exception e) 
		{
        	//FAILED TO STOP RPC
		}
	}
}
  */