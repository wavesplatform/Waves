package database.wallet;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Observable;

import org.mapdb.DB;

import scorex.account.Account;
import utils.ObserverMessage;

public class AccountMap extends Observable {

	private static final String ADDRESSES = "addresses";
	
	private Map<String, BigDecimal> addressMap;
	
	private List<Account> accounts;
	
	public AccountMap(WalletDatabase walletDatabase, DB database) 
	{
		//OPEN MAP
		this.addressMap = database.getTreeMap(ADDRESSES);
	}
	
	private void loadAccounts()
	{
		//RESET ACCOUNTS LIST
		this.accounts = new ArrayList<Account>();
		
		synchronized(this.accounts)
		{	
			
			for(String address: this.addressMap.keySet())
			{
				//CREATE ACCOUNT FROM ADDRESS
				Account account = new Account(address);
					
				//ADD TO LIST
				this.accounts.add(account);
			}	
		}
	}

	public List<Account> getAccounts() 
	{
		if(this.accounts == null)
		{
			this.loadAccounts();
		}
		
		return this.accounts;
	}
	
	public boolean exists(String address)
	{
		return this.addressMap.containsKey(address);
	}
	
	public Account getAccount(String address) 
	{
		if(this.accounts == null)
		{
			this.loadAccounts();
		}
		
		synchronized(this.accounts)
		{
			for(Account account: this.accounts)
			{
				if(account.address().equals(address))
				{
					return account;
				}
			}
		}
		
		return null;
	}

	public BigDecimal getUnconfirmedBalance(String address) 
	{
		if(this.addressMap.containsKey(address))
		{
			return this.addressMap.get(address);
		}
		
		return BigDecimal.ZERO.setScale(8);
	}
	
	public void add(Account account)
	{
		this.addressMap.put(account.address(), account.getConfirmedBalance());
		
		if(this.accounts == null)
		{
			this.loadAccounts();
		}
		
		synchronized(this.accounts)
		{
			if(!this.accounts.contains(account))
			{
				this.accounts.add(account);
				
				this.notifyObservers(new ObserverMessage(ObserverMessage.ADD_ACCOUNT_TYPE, account));
			}
		}
	}
	
	public void update(Account account, BigDecimal unconfirmedBalance) 
	{		
		this.addressMap.put(account.address(), unconfirmedBalance);	
		
		this.notifyObservers(new ObserverMessage(ObserverMessage.ADD_ACCOUNT_TYPE, account));
		
	}
	
	public void delete(Account account)
	{
		this.addressMap.remove(account.address());
		
		if(this.accounts == null)
		{
			this.loadAccounts();
		}
		
		synchronized(this.accounts)
		{
			this.accounts.remove(account);
			
			this.notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_ACCOUNT_TYPE, account));
		}
	}

	
}
