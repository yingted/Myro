// Utility Functions for Running Scheme in CSharp

using System;
using System.IO; // File
using System.Collections.Generic; // List
using Microsoft.VisualBasic.CompilerServices;

public class Scheme {

  public static char TILDE = '~';
  public static char NULL = '\0';
  public static char DOUBLEQUOTE = '"';
  public static char QUOTE = '\'';
  public static char BACKSPACE = '\b';
  public static char SLASH = '/';
  static char[] SPLITSLASH = {SLASH};

  public static string chars_to_scan = "";
  
  public static char First(object n) {
	return chars_to_scan[(int)n];
  }

  public static int remaining(object n) {
	return (1 + ((int)n));
  }
  
  public delegate void Function();
  public delegate bool Predicate(object obj);

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

  public static bool char_alphabetic_q(object o) {
	return false;
  }

  public static bool char_whitespace_q(object o) {
	return false;
  }

  public static bool char_special_subsequent_q(object o) {
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

  public static bool pattern_variable_q(object x) {
    return (symbol_q(x) && ("?" == ((string)x).Substring(0, 1)));
  }

  public static bool token_type_q(object token, object myclass) {
	return (car(token) == myclass);
  }

  
  public static object make_cont2(params object[] args) {
	return list2("continuation2", args);
  }
  
  public static object make_sub(params object[] args) {
	return list2("substitution", args);
  }
  
  public static object make_cont(params object[] args) {
	return list2("continuation", args);
  }

  public static object make_handler(params object[] args) {
	return list2("handler", args);
  }
  

  //;;make-binding
  //;;make-cont
  //;;make-cont2
  //;;make-empty-environment
  //;;make-frame
  //;;make-handler
  //;;make-initial-environment
  //;;make-macro-env
  //;;make-proc
  //;;make-toplevel-env


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
	try {
	  return (ObjectType.ObjTst(obj1, obj2, false) == 0);
	} catch {
	  return false;
	}
  }

  public static bool LessThan(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) < 0);
  }

  public static bool GreaterThan(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) > 0);
  }

  public static bool true_q(object obj) {
	return ((obj is bool) && ((bool)obj));
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
	return (ObjectType.MulObj(obj1, obj2));
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

  public static object list_ref(object obj, object pos) {
	if (pair_q(obj)) {
	  Cons result = ((Cons)obj);
	  for (int i = 0; i < ((int)pos); i++) {
		result = (Cons) cdr(result);
	  }
	  return (object)car(result);
	} else
	  throw new Exception("error in list_ref: object is not a list");
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

  public static object list2(object obj, params object[] args) {
	// first is an object, second is an array. all to be in list
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return new Cons(obj, result);
  }

  public static object list(params object[] args) {
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return result;
  }
  
  public static object cons(object obj1, object obj2) {
	if (obj2 is object[])
	  return new Cons(obj1, list(obj2));
	else
	  return new Cons(obj1, obj2);
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
	  return "[" + retval + "]";
	} else if (obj is String) {
	  return String.Format("\"{0}\"", obj);
	} else if (obj is Symbol && ((Symbol)obj) == EmptyList) {
	  return "()";
	} else if (obj is Cons) {
	  object current = (Cons)obj;
	  while (!Compare(current, EmptyList)) {
		if (retval != "")
		  retval += " ";
		retval += prettyPrint(car(current));
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

  public static object rest_of(object list) {
	return cdr(list);
  }

  public static object first(object list) {
	return car(list);
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

  public static bool application_q(object datum) {
    return (list_q(datum) && 
		(! null_q(datum)) && 
		(! reserved_keyword_q(car(datum))));
  }
  public static bool reserved_keyword_q(object x) {
    return (symbol_q(x) &&
		memq(x, list("quote", "quasiquote", "lambda", "if", "set!",
				"define",  "begin", "cond", "and", "or", "let", "let*",
				"letrec", "record-case", ";;", "do", "delay", "case",
				"try", "catch", "finally", "raise")));
  }

  public static bool quasiquote_q(object obj) {
	return (test_tag(obj, "quasiquote", "=", 2));
  }
  public static bool raise_q(object obj) {
	return (test_tag(obj, "raise", "=", 2));
  }
  public static object string_append(object obj1, object obj2) {
	return ((string)obj1) + ((string)obj2);
  }

  public static bool try_q(object obj) {
	return test_tag(obj, "try", ">=", 2);
  }

  public static object try_body(object obj) {
	return cadr(obj);
  }
  public static object catch_vars(object obj) {
	return cadr(obj);
  }
  public static object catch_exps(object obj) {
	return cddr(obj);
  }
  public static object finally_exps(object obj) {
	return cdr(obj);
  }

  public static bool catch_q(object obj) {
	return test_tag(obj, "catch", ">=", 3);
  }

  public static bool finally_q(object obj) {
	return test_tag(obj, "finally", ">=", 2);
  }

  public static bool unquote_q(object obj) {
	return test_tag(obj, "unquote", "=", 2);
  }
  public static bool unquote_splicing_q(object obj) {
	return test_tag(obj, "unquote-splicing", "=", 2);
  }

  public static bool test_tag(object obj, object key, 
	  object op, object size) {
	return ((list_q(obj) &&
			(! null_q(obj)) &&
			Compare(car(obj), key) &&
			Compare(length(obj), op, size)));
  }

  public static bool vector_q(object obj) {
	return (obj is object[]);
  }

  public static object vector_to_list(object obj) {
	return list(obj);
  }

  public static object load_stack(object obj) {
	return EmptyList;
  }

  public static object read_content(object filename) {
	return File.OpenText((string)filename).ReadToEnd();
  }

  public static object macro_env() {
	return null;
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

  public static bool literal_q(object datum) {
    return (number_q(datum) ||
		boolean_q(datum) ||
		char_q(datum) ||
		string_q(datum) ||
		vector_q(datum));
  }

  public static bool quote_q(object obj) {
	return test_tag(obj, "quote", "=", 2);
  }

  public static bool anything_q(object obj) {
	return true;
  }

  public static object lit_exp(object obj) {
	return list("lit-exp", obj);
  }

  public static object var_exp(object obj) {
	if (symbol_q(obj))
	  return list("var-exp", obj);
	else
	  return false;
  }

  public static bool expression_q(object obj) {
	object retval = lit_exp(obj);
	if (!false_q(retval))
	  return true;

	retval = var_exp(obj);
	if (!false_q(retval))
	  return true;

	retval = if_exp(obj);
	if (!false_q(retval))
	  return true;

	retval = assign_exp(obj);
	if (!false_q(retval))
	  return true;

	//	retval = define_exp(obj);
	//	if (!IsFalse(retval))
	//return true;

	retval = define_syntax_exp(obj);
	if (!false_q(retval))
	  return true;

	retval = begin_exp(obj);
	if (!false_q(retval))
	  return true;
	/*

	retval = lambda_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = mu_lambda_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = app_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = try_catch_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = try_finally_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = try_cath_finally_exp(obj);
	if (!IsFalse(retval))
	  return true;

	retval = raise_exp(obj);
	if (!IsFalse(retval))
	  return true;
	*/
	return false;
  }

  public static bool if_q(object obj) {
	return (expression_q(car(obj)) && 
		expression_q(cadr(obj)) && 
		expression_q(caddr(obj)));
  }
  
  public static object if_exp(object obj) {
	return list("if", car(obj), cadr(obj), caddr(obj));
  }

  public static bool if_else_q(object obj) {
	return test_tag(obj, "if", "=", 4);
  }
  
  public static bool if_then_q(object obj) {
	return test_tag(obj, "if", "=", 3);
  }

  public static object assign_exp(object obj) {
	if (symbol_q(car(obj)) && expression_q(cadr(obj)))
	  return list("assign", car(obj), cadr(obj));
	else
	  return false;
  }

  public static bool pattern_q(object x) {
    return (null_q(x) ||
		number_q(x) ||
		boolean_q(x) ||
		symbol_q(x) ||
		(pair_q(x) &&
			pattern_q(car(x)) &&
			pattern_q(cdr(x))));
  }

  public static object define_syntax_exp(object obj) {
	if (symbol_q(car(obj)) && list_of( list_of( (Predicate) pattern_q, obj)))
	  return list("define-syntax-exp", car(obj), cadr(obj));
	else
	  return false;
  }

  public static bool list_of(bool obj) {
	return false;
  }

  public static bool list_of(Predicate pred, object obj) {
	object retval = list();
	object current = obj;
	while (!null_q(current)) {
	  retval = new Cons(pred(car(obj)), retval);
	}
	//FIXME: go down list?
	return false;
  }
  
  //   (begin-exp
  //     (exps (list-of expression?)))
  
  public static bool begin_exp(object obj) {
	return list_of(expression_q, obj);
  }

  /*
  public static object define_exp(object obj) {
	if (symbol_q(car(obj)) &&
		expression_q(cadr(obj))) 
	  return test_tag("begin", cadr(obj));
  }
  */

//   (lambda-exp
//     (formals (list-of symbol?))
//     (body expression?))
//   (mu-lambda-exp
//     (formals (list-of symbol?))
//     (runt symbol?)
//     (body expression?))
//   (app-exp
//     (operator expression?)
//     (operands (list-of expression?)))
//   (try-catch-exp
//     (body expression?)
//     (catch-var symbol?)
//     (catch-exps (list-of expression?)))
//   (try-finally-exp
//     (body expression?)
//     (finally-exps (list-of expression?)))
//   (try-catch-finally-exp
//     (body expression?)
//     (catch-var symbol?)
//     (catch-exps (list-of expression?))
//     (finally-exps (list-of expression?)))
//   (raise-exp
//     (exp expression?))


  public static bool syntactic_sugar_q(object datum) {
    return (list_q(datum) && (! null_q(datum)) &&
		symbol_q(car(datum)));
	//FIXME:
	//(search-env macro-env (car datum)))))
  }

  public static bool assignment_q(object obj) {
	return test_tag(obj, "set!", "=", 3);
  }

  public static bool define_q(object obj) {
	return test_tag(obj, "define", ">=", 3);
  }

  public static bool mit_style_q(object datum) {
    return (! symbol_q(cadr(datum)));
  }

  public static object mit_define_transformer(object obj) {
	return null;
  }

  public static bool define_syntax_q(object obj) {
	return test_tag(obj, "define-syntax", ">=", 3);
  }

  public static bool begin_q(object obj) {
	return test_tag(obj, "begin", ">=", 2);
  }

  public static bool lambda_q(object obj) {
	return test_tag(obj, "lambda", ">=", 3);
  }

  public static bool lit_q(object obj) {
	return test_tag(obj, "lit", "=", 2);
  }

  public static void Main(string [] args) {
	display("Testing Add(1,1): ");
	display(Add(1, 1));
	newline();
	display("Literal test: ");
	object t = lit_exp(2);
	display(list_q(t));
	newline();
	display(lit_q(t));
	newline();
	display(null_q(t));
	newline();
	display("Compare tests: ");
	display(Compare("hello", "hello"));	newline();
	display(Compare("hello", "hel"));	newline();
	display(Compare("hello", "helloo"));	newline();
	display(Compare(4.1, 4));	newline();
	display(Compare("hello", true));	newline();
	display("Member test: ");
	t = cons("hello", t);
	//prettyPrint(member("hello", t));
	prettyPrint(list());
	display("\n");
  }

}
