
using System;

public class External {

  public int size = 0;
  public string name = "";

  public External(int size, string name) {
	this.size = size;
	this.name = name;
  }

  public class Constructor {
	
	public int size = 0;
	public string name = "";

	public Constructor(int size, string name) {
	  this.size = size;
	  this.name = name;
	}
  }

  public static int x = 5;

  public static int func() {
	Console.WriteLine("CALLED FUNC!");
	return 42;
  }

  public static string func2(string s) {
	Console.WriteLine("CALLED FUNC2!");
	return s.ToUpper();
  }
}