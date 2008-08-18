// Utility Functions for Running Scheme in CSharp

using System;
using System.IO; // File
using System.Collections.Generic; // List
using Microsoft.VisualBasic.CompilerServices;

public class Scheme {

  public delegate void Function();
  public delegate bool Predicate(object obj);

  public static char TILDE = '~';
  public static char NULL = '\0';
  public static char NEWLINE = '\n';
  public static char SINGLEQUOTE = '\'';
  public static char DOUBLEQUOTE = '"';
  public static char BACKQUOTE = '`';
  public static char BACKSPACE = '\b';
  public static char BACKSLASH = '\\';
  public static char SLASH = '/';
  static char[] SPLITSLASH = {SLASH};

  public static object get_current_time() {
	return 0.0;
  }

  public static QPredicate tagged_list(object test_string, object pred, object value) {
	return new QPredicate(test_string, pred, value);
  }

  public class QPredicate {
	object test_string;
	object pred; 
	object value;

	public QPredicate(object test_string, object pred, object value) {
	  this.test_string = test_string;
	  this.pred = pred; 
	  this.value = value;
	}

	public bool Call(object test) {
	  if ((car(test) == test_string) &&
		  (length(test) == test_string)) {
		return true;
	  }
	  return false;
	}
	
  }

  public static object list_to_vector(object lyst) {
	int len = (int) length(lyst);
	object current = lyst;
	object[] retval = new object[len];
	for (int i = 0; i < len; i++) {
	  retval[i] = current;
	  current = cdr(current);
	}
	return retval;
  }

  public static object string_ref(object s, object i) {
	return ((string)s)[(int)i];
  }

  public static object make_string(object obj) {
	if (obj == null || obj == (object) NULL)
	  return (object) "\0";
	return obj.ToString();
  }

  public static bool number_q(object datum) {
	return ((datum is int) ||
		(datum is double) ||
		(datum is Rational));
  }

  public static bool boolean_q(object datum) {
	return (datum is bool);
  }

  public static bool char_q(object datum) {
	return (datum is char);
  }

  public static bool string_q(object datum) {
	return (datum is string);
  }

  public static bool vector_q(object obj) {
	return (obj is object[]);
  }

  public static bool char_numeric_q(object c) {
	if (c == null) return false;
	return (('0' <= ((char)c)) && (((char)c) <= '9'));
  }

  public static void printf(object fmt, params object[] objs) {
	// replace ~a ~s ... with {0} {1} ...
	// replace ~% with \n
	Console.Write((string)fmt, objs);
  }

  public static void display(object obj) {
	try {
	  Console.Write(obj);
	} catch {
	  Console.Write("<?>");
	}
  }

  public static void display(object obj, object port) {
	// FIXME: add output port type
	Console.Write(obj);
  }

  public static bool symbol_q(object x) {
	return (x is Symbol);
  }

  public static bool char_alphabetic_q(object o) {
	if (o is char) {
	  char c = (char) o;
	  return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
	} 
	return false;
  }

  public static bool char_whitespace_q(object o) {
	if (o is char) {
	  char c = (char) o;
	  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
	}
	return false;
  }

  public static bool char_is__q(object c1, object c2) {
	return ((c1 is char) && (c2 is char) && ((char)c1) == ((char)c2));
	
  }

  public static bool string_is__q(object o1, object o2) {
	return ((o1 is string) && (o2 is string) && ((string)o1) == ((string)o2));
  }
  
  public static object string_to_symbol(object s) {
	return new Symbol((string)s);
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

  public static bool Compare(object obj1, object obj2) {
	if (obj1 is Symbol) {
	  if (obj2 is Symbol) {
		return (((Symbol)obj1) == ((Symbol)obj2));
	  } else 
		return false;
	} else {
	  try {
		return (ObjectType.ObjTst(obj1, obj2, false) == 0);
	  } catch {
		return false;
	  }
	}
  }

  public static bool LessThan(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) < 0);
  }

  public static bool GreaterThan(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) > 0);
  }

  public static bool false_q(object obj) {
	return ((obj is bool) && !((bool)obj));
  }

  public static object not(object obj) {
	return (! ((bool)obj));
  }

  public static bool Compare(object obj1, object op, object obj2) {
	if (((string)op) == "=") {
	  return (ObjectType.ObjTst(obj1, obj2, false) == 0);
	} else if (((string)op) == "<") {
	  return (ObjectType.ObjTst(obj1, obj2, false) < 0);
	} else if (((string)op) == ">") {
	  return (ObjectType.ObjTst(obj1, obj2, false) > 0);
	} else if (((string)op) == "<=") {
	  return (ObjectType.ObjTst(obj1, obj2, false) <= 0);
	} else if (((string)op) == ">=") {
	  return (ObjectType.ObjTst(obj1, obj2, false) >= 0);
	} 
	throw new Exception(String.Format("unknown compare operator: '{0}'", op));
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
	try {
	  return (ObjectType.MulObj(obj1, obj2));
	} catch {
	  throw new Exception(String.Format("multiply: '{0}' * '{1}'\n",
			  obj1, obj2));
	}
  }

  public static object Divide(object obj1, object obj2) {
	return (ObjectType.DivObj(obj1, obj2));
  }

  // List functions -----------------------------------------------

  public static object member(object obj1, object obj2) {
	if (pair_q(obj2)) {
	  object current = (Cons)obj2;
	  while (!Compare(current, EmptyList)) {
		if (Compare(obj1, car(current)))
		  return current;
		current = cdr(current);
	  }
	  return false;
	}
	throw new Exception("member takes an object and a list");
  }

  public static object array_ref(object array, object pos) {
	return ((object [])array)[(int) pos];
  }

  public static object list_ref(object obj, object pos) {
	//printf("calling list-ref({0}, {1})\n", obj, pos);
	if (pair_q(obj)) {
	  Cons result = ((Cons)obj);
	  for (int i = 0; i < ((int)pos); i++) {
		try {
		  result = (Cons) cdr(result);
		} catch {
		  throw new Exception(string.Format("error in list_ref: improper access (list_ref {0} {1})",
				  obj, pos));
		}
	  }
	  return (object)car(result);
	} else
	  throw new Exception(string.Format("list_ref: object is not a list: list-ref({0},{1})",
			  obj, pos));
  }

  public static Symbol EmptyList = new Symbol("()");

  public static bool null_q(object o1) {
	return ((o1 is Symbol) && (((Symbol)o1) == EmptyList));
  }

  public static bool pair_q(object x) {
    return (x is Cons);
  }

  public static object length(object obj) {
	if (list_q(obj)) {
	  if (null_q(obj)) 
		return 0;
	  else {
		int len = 0;
		object current = (Cons)obj;
		while (!Compare(current, EmptyList)) {
		  len++;
		  current = cdr(current);
		}
		return len;
	  }
	} else
	  throw new Exception("attempt to take length of a non-list");
  }

  public static bool list_q(object o1) {
	return (pair_q(o1) || null_q(o1));
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

  public static object array_to_string(object[] args) {
	string retval = "";
	if (args != null) {
	  int count = ((Array)args).Length;
	  for (int i = 0; i < count; i++) {
		  if (args[i] is object[]) {
			retval += array_to_string((object[])args[i]);
		  } else {
			if (retval != "")
			  retval += " ";
			retval += args[i];
		  }
	  }
	}
	return "[" + retval + "]";
  }
  

  // cons/list needs to work with object[]

  public static object list(params object[] args) {
	//printf("calling list({0})\n", array_to_string(args));
	Object result = EmptyList;
	if (args != null) {
	  int count = ((Array)args).Length;
	  for (int i = 0; i < count; i++) {
		Object item = args[count - i - 1];
		if (item == null) 
		  result = EmptyList;
		else if (item is object[])
		  result = append( list((object[]) item), result);
		else
		  result = new Cons(item, result);
	  }
	}
	//printf("returning list: {0}\n", pretty_print(result));	
	return result;
  }

  public static object rdc(object lyst) {
	if (null_q(cdr(lyst)))
	  return lyst;
	else
	  return rdc(cdr(lyst));
  }
  
  public static object set_cdr_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.cdr = item;
	return null;
  }

  public static object set_car_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.car = item;
	return null;
  }

  public static object append(object obj1, object obj2) {
	if (obj1 is object[]) {
	  Object lyst = list(obj1);
	  Cons cell = (Cons) rdc(lyst);
	  set_cdr_b(cell, obj2);
	  return lyst;
	} else if (obj1 is Cons) {
	  Cons cell = (Cons) rdc(obj1);
	  set_cdr_b(cell, obj2);
	  return obj1;
	} else {
	  return new Cons(obj1, obj2);
	}
  }

  public static object cons(object obj1, object obj2) {
	return (object) new Cons(obj1, obj2);
  }

  public static object cdr(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).cdr;
	else
	  throw new Exception(string.Format("cdr: object is not a list: cdr({0})",
			  obj));
  }

  public static object car(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).car;
	else
	  throw new Exception(string.Format("car: object is not a list: cdr({0})",
			  obj));
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

  public class Cons {
	public object car;
	public object cdr;
	
	public Cons(object a, object b) {
	  this.car = a;
	  if (b is object[] || b == null) 
		this.cdr = list(b);
	  else
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
		string s = String.Format("({0}", 
			pretty_print(this.car));
		object sexp = this.cdr;
		while (sexp is Cons) {
		  s += String.Format(" {0}", 
			  pretty_print(((Cons)sexp).car));
		  sexp = ((Cons)sexp).cdr;
		}
		if (sexp == EmptyList) {
		  s += ")";
		} else {
		  s += String.Format(" . {0})", pretty_print(sexp));
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
  
  public static string pretty_print(object obj) {
	string retval = "";
	if (obj is String) {
	  return String.Format("\"{0}\"", obj);
	} else if (obj is Symbol && ((Symbol)obj) == EmptyList) {
	  return "()";
	} else if (obj is Cons) {
	  object current = (Cons)obj;
	  while (!Compare(current, EmptyList)) {
		if (retval != "")
		  retval += " ";
		retval += pretty_print(car(current));
		current = cdr(current);
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


//   public static bool memq(object x, object list) {
// 	if (null_q(x)) return false;
// 	else if (Compare(x, list)) return true;
// 	else return (memq(x, cdr(list)));
//   }

  public static bool memq(object item1, object list) {
	if (list is Cons) {
	  object current = list;
	  while (! Compare(current, EmptyList)) {
		if (Compare(item1, car(current))) {
		  return true;
		}
	  }
	}
	return false;
  }

  public static object vector_to_list(object obj) {
	return list(obj);
  }

  public static object read_content(object filename) {
	return File.OpenText((string)filename).ReadToEnd();
  }

  public static object string_append(object obj1, object obj2) {
	return (obj1.ToString() + obj2.ToString());
  }

  public static void Main(string [] args) {
	// ----------------------------------
	// Math:
	printf ("  Add(1,1), Result: {0}, Should be: {1}\n", 
		Add(1, 1), 1 + 1);
	printf ("  Multiply(10,2), Result: {0}, Should be: {1}\n", 
		Multiply(10, 2), 10 * 2);
	printf ("  Divide(5,2), Result: {0}, Should be: {1}\n", 
		Divide(5, 2), 5/2.0);
	printf ("  Subtract(22,7), Result: {0}, Should be: {1}\n", 
		Subtract(22, 7), 22 - 7);
	// -----------------------------------
	// Compare tests:
	printf("hello == hello: {0}\n", Compare("hello", "hello"));
	printf("hello == hel: {0}\n", Compare("hello", "hel"));
	printf("hello == helloo: {0}\n", Compare("hello", "helloo"));
	printf("4.1 == 4: {0}\n", Compare(4.1, 4));
	printf("hello == true: {0}\n", Compare("hello", true));
	
	printf("() == (): {0}\n", Compare(list(), list()));

	object t = list(2);
	printf("t = list(2): {0}\n", t);
	printf("list? t: {0}\n", list_q(t));
	printf("null? t: {0}\n", null_q(t));

	//cons("a", EmptyList).ToString();
	printf("cons('a', ()): {0}\n", cons("a", EmptyList));
	
	t = cons("b", cons("a", t));
	t = cons("c", cons("a", t));
	t = cons("d", cons("a", t));
	printf("t = : {0}\n", t);

	printf("null? cdr(t): {0} {1}\n", 
		null_q(cdr(t)), 
		cdr(t));
	printf("null? cddr(t): {0}\n", null_q(cddr(t)));

	//	printf("null? cdddr(t): {0}\n", null_q(cdddr(t)));
	printf("Member test: \n");
	t = cons("hello", t);
	printf("t = {0}\n", pretty_print(t));
	printf("member(hello, t) : {0}\n", pretty_print(member("hello", t)));
	printf("member(a, t) : {0}\n", pretty_print(member("a", t)));
	printf("member(c, t) : {0}\n", pretty_print(member("c", t)));
	printf("(): {0}\n", pretty_print(list()));
	printf("list(t): {0}\n", pretty_print(list(t)));
	printf("length(list(t)): {0}\n", length(list(t)));
	printf("length(cdr(list(t))): {0}\n", length(cdr(list(t))));
	printf("length(car(list(t))): {0}\n", length(car(list(t))));
	printf("cons(\"X\", list(t))): {0}\n", pretty_print(cons("X", list(t))));
	printf("x is: {0}\n", pretty_print("x"));
	printf("t is: {0}\n", pretty_print(t));
	printf("list(): {0}\n", list());
	printf("cons('a', list()): {0}\n", cons("a", list()));
	printf("cons('a', 'b'): {0}\n", cons("a", "b"));

	printf("cons('a', null): {0}\n", cons("a", null));

	printf("string-append('test', NULL): \"{0}\"\n", 	 
		string_append ((object) "test",
			(object) make_string ((object) NULL)));
  }
  
}
