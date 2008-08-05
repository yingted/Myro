// Utility Functions for Running Scheme in CSharp

using System;
using System.Collections.Generic; // List
using Microsoft.VisualBasic.CompilerServices;

public class Scheme {

  public delegate void Function();

  public static SchemeSymbol EmptyList = new SchemeSymbol("()");

  public static void display(object obj) {
	Console.Write(obj);
  }

  public static void display(object obj, object port) {
	// FIXME: add output port type
	Console.Write(obj);
  }

  public static object make_cont(params object[] args) {
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return ((object)new Cons("continuation", result));
  }

  public static List<object> makeList(params object[] args) {
	return new List<object>(args);
  }

  public static void error(string code, string msg, params object[] rest) {
	Console.WriteLine("Error in {0}: {1}", code, format(msg, rest));
  }

  public static void newline() {
	Console.WriteLine("");
  }

  public static string format(string msg, params object[] rest) {
	// FIXME: replace ~codes with {codes}
	//String.Format(msg, rest);
	return String.Format(msg);
  }

  public static object list_ref(object obj, object pos) {
	Cons result = ((Cons)obj);
	for (int i = 0; i < ((int)pos); i++) {
	  result = (Cons) cdr(result);
	}
	return (object)car(result);
  }

  public static object cdr(object obj) {
	return ((Cons)obj).cdr;
  }

  public static object car(object obj) {
	return ((Cons)obj).car;
  }

  public static bool Compare(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) == 0);
  }

  public static object Add(object obj1, object obj2) {
	return (ObjectType.AddObj(obj1, obj2));
  }

  public static object Subtract(object obj1, object obj2) {
	return (ObjectType.SubObj(obj1, obj2));
  }

  public static object Multiply(object obj1, object obj2) {
	return (ObjectType.MulObj(obj1, obj2));
  }

  public static object Divide(object obj1, object obj2) {
	return (ObjectType.DivObj(obj1, obj2));
  }

  public class Cons {
	public object car;
	public object cdr;
	
	public Cons(object a, object b) {
	  this.car = a;
	  this.cdr = b;
	}
	
	public override string ToString() {
	  if (this.car == new SchemeSymbol("quote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format("'{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new SchemeSymbol("quasiquote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format("`{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new SchemeSymbol("unquote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format(",{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new SchemeSymbol("unquote-splicing") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format(",@{0}", ((Cons)this.cdr).car);
	  } else {
		string s = String.Format("({0}", this.car);
		object sexp = this.cdr;
		while (sexp is Cons) {
		  s += String.Format(" {0}", ((Cons)sexp).car);
		  sexp = ((Cons)sexp).cdr;
		}
		if (sexp == EmptyList) {
		  s += ")";
		} else {
		  s += String.Format(" . {0})", sexp);
		}
		return s;
	  }
	}
  }
  
  public class SchemeSymbol {
	string id;
	
	public SchemeSymbol(string id) {
	  this.id = id;
	}
	
	public override bool Equals(object other) {
	  return (other is SchemeSymbol && this.id == ((SchemeSymbol)other).id);
	}
	
	public override int GetHashCode() {
	  return id.GetHashCode();
	}
	
	public override string ToString() {
	  return this.id;
	}
  }
  
  public class ExactNumber {
	int numerator;
	int denominator;
	
	public ExactNumber(int num) {
	  this.numerator = num;
	  this.denominator = 1;
	}
	
	public ExactNumber(int numerator, int denominator) {
	  int gcd = GCD(numerator, denominator);
	  this.numerator = numerator/gcd;
	  this.denominator = denominator/gcd;
	}
	
	public static int GCD(int n1, int n2) {
	  // Greatest Common Denominator
	  n1 = Math.Abs(n1);
	  n2 = Math.Abs(n2);
	  if (n1 == 0) return n2;
	  if (n2 == 0) return n1;
	  if (n1 > n2) return GCD(n2, n1 % n2);
	  else         return GCD(n1, n2 % n1);
	}
	
	public static int LCM(int n1, int n2) {
	  // Least Common Multiple
	  n1 = Math.Abs(n1);
	  n2 = Math.Abs(n2);
	  if (n1 > n2) return ((n2 / GCD(n1, n2)) * n1);
	  else         return ((n1 / GCD(n1, n2)) * n2);
	}

	public override bool Equals(object other) {
	  return (other is ExactNumber &&
		  (this.numerator == ((ExactNumber)other).numerator &&
			  this.denominator == ((ExactNumber)other).denominator));
	}
	
	public override int GetHashCode() {
	  double d = ((double) numerator) / denominator;
	  return d.GetHashCode();
	}
	
	public override string ToString() {
	  if (denominator != 1)
		return string.Format("{0}/{1}", numerator, denominator);
	  else
		return numerator.ToString();
	}

	public static ExactNumber operator +(ExactNumber f1, ExactNumber f2) {
	  int lcm = LCM(f1.denominator, f2.denominator);
	  return new ExactNumber((f1.numerator * lcm/f1.denominator +
			                  f2.numerator * lcm/f2.denominator),
		                     lcm);
	}
  }
  
  public class InexactNumber {
	double num;
	
	public InexactNumber(double num) {
	  this.num = num;
	}
	
	public override bool Equals(object other) {
	  return (other is InexactNumber && this.num == ((InexactNumber)other).num);
	}
	
	public override int GetHashCode() {
	  return num.GetHashCode();
	}
	
	public override string ToString() {
	  return this.num.ToString();
	}
  }
  
  public class Vector {

	List<object> elements;

	public Vector(object cell) {
	  elements = new List<object>();
	  while (cell is Cons) {
		elements.Add(((Cons)cell).car);
		cell = ((Cons)cell).cdr;
	  }
	}

	public override string ToString() {
	  string retval = "";
	  foreach (object s in elements) {
		if (retval != "") 
		  retval += " ";
		retval += s.ToString();
	  }
	  return String.Format("#({0})", retval);
	}
	
  }

  public class SchemeBoolean {
	bool val;
	
	public SchemeBoolean(bool val) {
	  this.val = val;
	}
	
	public override bool Equals(object other) {
	  return (other is SchemeBoolean && this.val == ((SchemeBoolean)other).val);
	}
	
	public override int GetHashCode() {
	  return val.GetHashCode();
	}
	
	public override string ToString() {
	  if (this.val) {
		return "#t";
	  } else {
		return "#f";
	  }
	}
  }
  
  public class SchemeCharacter {
	char ch;
	
	public SchemeCharacter(char ch) {
	  this.ch = ch;
	}
	
	public override bool Equals(object other) {
	  return (other is SchemeCharacter && this.ch == ((SchemeCharacter)other).ch);
	}
	
	public override int GetHashCode() {
	  return ch.GetHashCode();
	}
	
	public override string ToString() {
	  if (this.ch == '\0') return "#\\nul";
	  else if (this.ch == ' ') return "#\\space";
	  else if (this.ch == '\t') return "#\\tab";
	  else if (this.ch == '\n') return "#\\newline";
	  else if (this.ch == '\b') return "#\\backspace";
	  else if (this.ch == '\r') return "#\\return";
	  else if (this.ch == '\f') return "#\\page";
	  else return String.Format("#\\{0}", this.ch);    // not quite right
	}
  }
  
  public class SchemeString {
	string val;
	
	public SchemeString(string val) {
	  this.val = val;
	}
	
	public override bool Equals(object other) {
	  return (other is SchemeString && this.val == ((SchemeString)other).val);
	}
	
	public override int GetHashCode() {
	  return val.GetHashCode();
	}
	
	public override string ToString() {
	  return '"' + this.val + '"';
	}
  }
  
  
  // () is represented as EmptyList
  
  public static string prettyPrint(object obj) {
	string retval = "";
	if (obj is List<object>) {
	  foreach (object item in (List<object>) obj) {
		if (retval != "")
		  retval += " ";
		if (item is List<object>)
		  retval += prettyPrint((List<object>) item);
		else
		  retval += item.ToString();
	  }
	  return "(" + retval + ")";
	} else {
	  return obj.ToString();
	}
  }
  
  public static bool isTokenType(List<object> token, string tokenType) {
	return (((string) token[0]) == tokenType);
  }
  
  public static string arrayToString(object[] array) {
	string retval = "";
	foreach (object item in array) {
	  if (retval != "")
		retval += " ";
	  retval += item.ToString();
	}
	return retval;
  }

  public static void rest_of(List<object> list) {
	list.RemoveAt(0);
  }

  public static string first(List<object> list) {
	return (string) list[0];
  }

}