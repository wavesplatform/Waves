package utils;

public class Pair<T, U> {

	private T a;
	private U b;
	
	public Pair(T a, U b)
	{
		this.a = a;
		this.b = b;
	}
	
	public T getA()
	{
		return a;
	}
	
	public U getB()
	{
		return b;
	}
	
}
