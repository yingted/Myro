// Utility Functions for Running Scheme in CSharp

using System;
using System.Collections.Generic; // List
using Microsoft.VisualBasic.CompilerServices;

public class Scheme {

  public static char NULL = '\0';
  public static char BACKSPACE = '\b';
  public static char SLASH = '/';
  static char[] SPLITSLASH = {SLASH};

  public static string chars_to_scan = "";
  
  public static char First(int n) {
	return chars_to_scan.Substring(n, 1)[0];
  }

  public static int remaining(int n) {
	return (1 + n);
  }
  
  public delegate void Function();

  public static Symbol EmptyList = new Symbol("()");

  public static void display(object obj) {
	Console.Write(obj);
  }

  public static void display(object obj, object port) {
	// FIXME: add output port type
	Console.Write(obj);
  }

  public static bool symbol_q(object x) {
	return (x is Symbol);
  }

  public static bool constant_q(object x) {
    return (!pattern_variable_q(x) && !pair_q(x));
  }

  public static bool pair_q(object x) {
    return (x is Cons);
  }

  public static object reverse(object lyst) {
	if (lyst is Cons) {
	  object result = EmptyList;
	  object current = ((Cons)lyst);
	  while (current != EmptyList) {
		result = new Cons(car(current), result);
		current = cdr(current);
	  }
	  return result;
	} else {
	  return EmptyList;
	}
  }

  public static bool char_is__q(object c1, object c2) {
	return ((c1 is char) && (c2 is char) && ((char)c1) == ((char)c2));
	
  }

  public static bool string_is__q(object o1, object o2) {
	return ((o1 is string) && (o2 is string) && ((string)o1) == ((string)o2));
  }
  
  public static bool null_q(object o1) {
	return ((o1 is Symbol) && (((Symbol)o1) == EmptyList));
  }

  public static bool list_q(object o1) {
	return ((o1 is Cons) || null_q(o1));
  }

  public static object list_to_string(object lyst) {
	String retval = "";
	object current = (Cons)lyst;
	while (current != EmptyList) {
	  retval += car(current).ToString();
	  current = cdr((Cons)current);
	}
	return retval;
  }

  public static object string_to_symbol(object s) {
	return new Symbol((string)s);
  }

  public static bool pattern_variable_q(object x) {
    return (symbol_q(x) && ("?" == ((string)x).Substring(0, 1)));
  }

  public static bool token_type_q(object token, object myclass) {
	return (car(token) == myclass);
  }

  public static object make_cont2(params object[] args) {
	return make_list("continuation2", args);
  }
  
  public static object make_sub(params object[] args) {
	return make_list("substitution", args);
  }
  
  public static object make_cont(params object[] args) {
	return make_list("continuation", args);
  }

  public static object list(params object[] args) {
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return result;
  }
  
  public static object make_list(string kind, params object[] args) {
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return new Cons(kind, result);
  }

  public static object string_to_integer(object str) {
	return int.Parse((string)str);
  }

  public static object string_to_decimal(object str) {
	return double.Parse((string)str);
  }

  public static object string_to_rational(object str) {
	string[] part = ((string)str).Split(SPLITSLASH);
	return new Rational(int.Parse(part[0]), int.Parse(part[1]));
  }

  public static void error(object code, object msg, params object[] rest) {
	Console.WriteLine("Error in {0}: {1}", ((string)code), format(msg, rest));
  }

  public static void newline() {
	Console.WriteLine("");
  }

  public static string format(object msg, params object[] rest) {
	// FIXME: replace ~codes with {codes}
	//String.Format(msg, rest);
	return String.Format(((string)msg), rest);
  }

  public static object list_ref(object obj, object pos) {
	if (obj is Cons) {
	  Cons result = ((Cons)obj);
	  for (int i = 0; i < ((int)pos); i++) {
		result = (Cons) cdr(result);
	  }
	  return (object)car(result);
	} else
	  throw new Exception("error in list_ref: object is not a list");
  }

  public static object cdr(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).cdr;
	else
	  throw new Exception("error in cdr: object is not a list");
  }

  public static object car(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).car;
	else
	  throw new Exception("error in car: object is not a list");
  }

  public static object   caar(object x) {	return car(car(x));   }
  public static object   cadr(object x) { 	return car(cdr(x));   }
  public static object   cdar(object x) {	return cdr(car(x));   }
  public static object   cddr(object x) {	return cdr(cdr(x));   }
  public static object  caaar(object x) {	return car(car(car(x)));   }
  public static object  caadr(object x) { 	return car(car(cdr(x)));   }
  public static object  cadar(object x) {	return car(cdr(car(x)));   }
  public static object  caddr(object x) {	return car(cdr(cdr(x)));   }
  public static object  cdaar(object x) {	return cdr(car(car(x)));   }
  public static object  cdadr(object x) { 	return cdr(car(cdr(x)));   }
  public static object  cddar(object x) {	return cdr(cdr(car(x)));   }
  public static object  cdddr(object x) {	return cdr(cdr(cdr(x)));   }
  public static object caaaar(object x) {	return car(car(car(car(x))));   }
  public static object caaadr(object x) {	return car(car(car(cdr(x))));   }
  public static object caadar(object x) {	return car(car(cdr(car(x))));   }
  public static object caaddr(object x) {	return car(car(cdr(cdr(x))));   }
  public static object cadaar(object x) {	return car(cdr(car(car(x))));   }
  public static object cadadr(object x) {	return car(cdr(car(cdr(x))));   }
  public static object caddar(object x) {	return car(cdr(cdr(car(x))));   }
  public static object cadddr(object x) {	return car(cdr(cdr(cdr(x))));   }
  public static object cdaaar(object x) {	return cdr(car(car(car(x))));   }
  public static object cdaadr(object x) {	return cdr(car(car(cdr(x))));   }
  public static object cdadar(object x) {	return cdr(car(cdr(car(x))));   }
  public static object cdaddr(object x) {	return cdr(car(cdr(cdr(x))));   }
  public static object cddaar(object x) {	return cdr(cdr(car(car(x))));   }
  public static object cddadr(object x) {	return cdr(cdr(car(cdr(x))));   }
  public static object cdddar(object x) {	return cdr(cdr(cdr(car(x))));   }
  public static object cddddr(object x) {	return cdr(cdr(cdr(cdr(x))));   }

  public static bool Compare(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) == 0);
  }

  public static object Add(object obj1, object obj2) {
	try {
	  return (ObjectType.AddObj(obj1, obj2));
	} catch {
	  try {
		if (obj1 is Rational) {
		  if (obj2 is Rational) {
			return (((Rational)obj1) + ((Rational)obj2));
		  } else if (obj2 is int) {
			return (((Rational)obj1) + ((int)obj2));
		  } else if (obj2 is double) {
			return (((double)((Rational)obj1)) + ((double)obj2));
		  }
		} else if (obj2 is Rational) {
		  if (obj1 is Rational) {
			return (((Rational)obj1) + ((Rational)obj2));
		  } else if (obj1 is int) {
			return (((Rational)obj2) + ((int)obj1));
		  } else if (obj1 is double) {
			return (((double)((Rational)obj2)) + ((double)obj1));
		  }
		}
	  } catch {
		throw new Exception(String.Format("unable to add {0} and {1}", 
				obj1.GetType().ToString(), obj2.GetType().ToString()));
	  }
	}
	throw new Exception(String.Format("unable to add {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
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
	  if (this.car == new Symbol("quote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format("'{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new Symbol("quasiquote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format("`{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new Symbol("unquote") &&
		  (this.cdr is Cons) &&
		  ((Cons)this.cdr).cdr == EmptyList) {
		return String.Format(",{0}", ((Cons)this.cdr).car);
	  } else if (this.car == new Symbol("unquote-splicing") &&
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
  
  public class Symbol {
	string id;
	
	public Symbol(string id) {
	  this.id = id;
	}
	
	public override bool Equals(object other) {
	  return (other is Symbol && this.id == ((Symbol)other).id);
	}
	
	public override int GetHashCode() {
	  return id.GetHashCode();
	}
	
	public override string ToString() {
	  return this.id;
	}
  }
  
  public class Rational {
	int numerator;
	int denominator;
	
	public Rational(int num) {
	  this.numerator = num;
	  this.denominator = 1;
	}
	
	public Rational(int numerator, int denominator) {
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
	  return (other is Rational &&
		  (this.numerator == ((Rational)other).numerator &&
			  this.denominator == ((Rational)other).denominator));
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

	public static implicit operator double(Rational f) {
	  return (((double) f.numerator) / ((double) f.denominator));
	}

	public static implicit operator float(Rational f) {
	  return (((float) f.numerator) / ((float) f.denominator));
	}

	public static Rational operator +(Rational f1, Rational f2) {
	  int lcm = LCM(f1.denominator, f2.denominator);
	  return new Rational((f1.numerator * lcm/f1.denominator +
			                  f2.numerator * lcm/f2.denominator),
		                     lcm);
	}

	public static Rational operator +(Rational f1, int i) {
	  int lcm = LCM(f1.denominator, 1);
	  return new Rational((f1.numerator * lcm/f1.denominator +
			  i * lcm/1),
		  lcm);
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

  public static object rest_of(object list) {
	return cdr(list);
  }

  public static object first(object list) {
	return car(list);
  }

  public static bool true_q(object obj) {
	return ((bool)obj);
  }
  
  public static bool memq(object item1, object list) {
	if (list is Cons) {
	  object current = list;
	  while (current != EmptyList) {
		if (Compare(item1, car(current))) {
		  return true;
		}
	  }
	}
	return false;
  }

  public static bool try_q (object o1, object key, object op, object size) {
	return true;
  }
  public static object application_q(object obj) {
	return null;
  }
  public static object length(object obj) {
	return null;
  }
  public static object not(object obj) {
	return null;
  }
  public static object quasiquote_q(object obj) {
	return null;
  }
  public static object raise_q(object obj) {
	return null;
  }
  public static object string_append(object obj) {
	return null;
  }
  public static object try_body(object obj) {
	return null;
  }
  public static object unquote_q(object obj) {
	return null;
  }
  public static object unquote_splicing_q(object obj) {
	return null;
  }
  public static object vector_q(object obj) {
	return null;
  }
  public static object vector_to_list(object obj) {
	return null;
  }

}
