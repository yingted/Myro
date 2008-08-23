// Utility Functions for Running Scheme in CSharp

using System;
using System.IO; // File
using Microsoft.Scripting.Math;
using System.Collections.Generic; // List
using Microsoft.VisualBasic.CompilerServices;

public class Config {
  public int DEBUG = 0;
  public bool NEED_NEWLINE = false;

  public Config() {
  }
}

public abstract class Scheme {

  public static Config config = new Config();

  public delegate void Function();
  public delegate bool Predicate(object obj);
  public delegate bool Predicate2(object obj1, object obj2);

  public delegate object Procedure0();
  public delegate void Procedure0Void();
  public delegate bool Procedure0Bool();

  public delegate object Procedure1(object args);
  public delegate void Procedure1Void(object args);
  public delegate bool Procedure1Bool(object args);

  public delegate object Procedure2(object args1, object args2);
  public delegate void Procedure2Void(object args1, object args2);
  public delegate bool Procedure2Bool(object args1, object args2);

  public static BigInteger makeBigInteger(int value) {
	int sign = +1;
	if (value < 0) {
	  sign = -1;
	  value *= -1;
	}
	return new BigInteger(sign, (uint)value);
  }

  public static BigInteger BigIntegerParse(string value) {
	int radix = 10;
	BigInteger multiplier = makeBigInteger(1);
	BigInteger result = makeBigInteger(0);
	value = (value.ToUpper()).Trim();
	int limit = 0;
	if(value[0] == '-')
	  limit = 1;
	for(int i = value.Length - 1; i >= limit ; i--) {
	  int posVal = (int)value[i];
	  if(posVal >= '0' && posVal <= '9')
		posVal -= '0';
	  else if(posVal >= 'A' && posVal <= 'Z')
		posVal = (posVal - 'A') + 10;
	  else
		posVal = 9999999;       // error flag
	  
	  if(posVal >= radix)
		throw(new ArithmeticException("Invalid string in constructor."));
	  else {
		if(value[0] == '-')
		  posVal = -posVal;
		result = result + (multiplier * posVal);
		if((i - 1) >= limit)
		  multiplier = multiplier * radix;
	  }
	}
	return result;
  }
  
  public class Proc {
	object proc = null;
	int args = -1;
	int returntype = 1;

	public Proc(object proctype, int args, int returntype) {
	  this.proc = proctype;
	  this.args = args;
	  this.returntype = returntype;
	}
	public object Call(object actual) {
	  trace(1, "calling Call1: {0} {1} {2} {3}\n", actual, proc, args, returntype);
	  object retval = null;
	  if (returntype == 0) { // void return
		if (args == -1) 
		  ((Procedure1Void)proc)(actual);
		else if (args == 0) 
		  ((Procedure0Void)proc)();
		else if (args == 1) 
		  ((Procedure1Void)proc)(car(actual));
		else if (args == 2) 
		  ((Procedure2Void)proc)(car(actual), cadr(actual));
	  } else if (returntype == 1) { // return object
		if (args == -1) 
		  retval = ((Procedure1)proc)(actual);
		else if (args == 0) 
		  retval = ((Procedure0)proc)();
		else if (args == 1) 
		  retval = ((Procedure1)proc)(car(actual));
		else if (args == 2) 
		  retval = ((Procedure2)proc)(car(actual), cadr(actual));
	  } else if (returntype == 2) { // return bool
		if (args == -1) 
		  retval = ((Procedure1Bool)proc)(actual);
		else if (args == 0) 
		  retval = ((Procedure0Bool)proc)();
		else if (args == 1) 
		  retval = ((Procedure1Bool)proc)(car(actual));
		else if (args == 2) 
		  retval = ((Procedure2Bool)proc)(car(actual), cadr(actual));
	  }
	  return retval;
	}

	public object Call(object args1, object args2) {
	  trace(1, "calling Call2: {0} {1}\n", args1, args2);
	  object retval = null;
	  if (returntype == 0) { // return void
		((Procedure2Void)proc)(args1, args2);
	  } else if (returntype == 1) { // return object
		retval = ((Procedure2)proc)(args1, args2);
	  } else if (returntype == 3) { // return object
		retval = ((Procedure2)proc)(car(args1), car(args2));
	  } else if (returntype == 2) { // return bool
		retval = ((Procedure2Bool)proc)(args1, args2);
	  }
	  return retval;
	}
  }

  // ProcedureN - N is arg count coming in
  // -1, 1, 2 - number of pieces to call app with (-1 is all)
  // 0, 1, 2 - return type 0 = void, 1 = object, 2 = bool
  public static Proc Add_proc = new Proc((Procedure1)Add, -1, 1);
  public static Proc Same_proc = new Proc((Procedure1Bool) Same, -1, 2);
  public static Proc Divide_proc = new Proc((Procedure1) Divide, -1, 1);
  public static Proc GreaterThan_proc = new Proc((Procedure1Bool) GreaterThan, -1, 2);
  public static Proc LessThan_proc = new Proc((Procedure1Bool) LessThan, -1, 2);
  public static Proc Multiply_proc = new Proc((Procedure1) Multiply, -1, 1);
  public static Proc Subtract_proc = new Proc((Procedure1) Subtract, -1, 1);
  public static Proc cadr_proc = new Proc((Procedure1) cadr, 1, 1);
  public static Proc car_proc = new Proc((Procedure1) car, 1, 1);
  public static Proc cdr_proc = new Proc((Procedure1) cdr, 1, 1);
  public static Proc cons_proc = new Proc((Procedure2) cons, 2, 1);
  public static Proc list_to_vector_proc = new Proc((Procedure1) list_to_vector, 1, 1);
  public static Proc memq_proc = new Proc((Procedure1Bool) memq, 2, 2);
  public static Proc range_proc = new Proc((Procedure1) range, -1, 1);
  public static Proc reverse_proc = new Proc((Procedure1) reverse, 1, 1);
  public static Proc sort_proc = new Proc((Procedure1) sort, 1, 1);
  public static Proc set_car_b_proc = new Proc((Procedure2Void) set_car_b, 2, 0);
  public static Proc set_cdr_b_proc = new Proc((Procedure2Void) set_cdr_b, 2, 0);
  public static Proc sqrt_proc = new Proc((Procedure1) sqrt, -1, 1);
  public static Proc string_to_symbol_proc = new Proc((Procedure1) string_to_symbol, 1, 1);
  public static Proc null_q_proc = new Proc((Procedure1Bool) null_q, 1, 2);
  public static Proc sys_display_proc = new Proc((Procedure1Void) display, 1, 0);
  public static Proc sys_pretty_print_proc = new Proc((Procedure1Void) pretty_print, -1, 0);
  public static Proc append_proc = new Proc((Procedure1) append, -1, 1);
  public static Proc make_binding_proc = new Proc((Procedure2)make_binding, 2, 1);

  public static char TILDE = '~';
  public static char NULL = '\0';
  public static char NEWLINE = '\n';
  public static char SINGLEQUOTE = '\'';
  public static char DOUBLEQUOTE = '"';
  public static char BACKQUOTE = '`';
  public static char BACKSPACE = '\b';
  public static char BACKSLASH = '\\';
  public static char SLASH = '/';
  public static char[] SPLITSLASH = {SLASH};

  public static void trace(int level, object fmt, params object[] objs) {
	if (level <= config.DEBUG) {
	  printf(fmt, objs);
	}
  }

  public static bool true_q (object v) {
	if (v is bool) {
	  return ((bool) v);
	} else {
	  if (v is int) 
		return (((int)v) != 0);
	  else if (v is double) 
		return (((double)v) != 0.0);
	  else
		return true;
	}
  }

  public static object get_current_time() {
	DateTime baseTime = new DateTime(1970, 1, 1, 8, 0, 0);
	DateTime nowInUTC = DateTime.UtcNow;
	return ((nowInUTC - baseTime).Ticks / 10000000.0);
  }

  public static object symbol_to_string (object x) {
	return x.ToString();
  }

  public static object group(object chars, object delimiter) {
	trace(2, "calling group({0}, {1})", chars, delimiter);
	// given list of chars and a delim char, return a list of strings
	object retval = EmptyList;
	object buffer = EmptyList;
	object current1 = chars;
	while (!Same(current1, EmptyList)) {
	  if (Same(car(current1), delimiter)) {
		retval = cons(list_to_string(buffer), retval);
	  } else {
		buffer = cons(car(current1), buffer);
	  }
	  current1 = cdr(current1);
	}
	if (!Same(buffer, EmptyList))
	  retval = cons(list_to_string(buffer), retval);
	return reverse(retval);
  }

  public static object make_initial_environment (object vars, object vals)
  {
	return list(
		extend_frame("debug", new Proc((Procedure1)debug, -1, 1),
		extend_frame("typeof", new Proc((Procedure1)get_type, 1, 1),
		extend_frame("float", new Proc((Procedure1)ToDouble, 1, 1),
		extend_frame("int", new Proc((Procedure1)ToInt, 1, 1),
		extend_frame("sort", new Proc((Procedure1)sort, 1, 1),
		extend_frame("string->symbol", new Proc((Procedure1) string_to_symbol, 1, 1),
		extend_frame("group", new Proc((Procedure2) group, 2, 1),
		extend_frame("member", new Proc((Procedure2)member, 2, 1),
		extend_frame("map", new Proc((Procedure2)map, 2, 1),
			make_frame(vars, vals)))))))))));
  }
  
  public static object make_frame (object variables, object values)
  {
	return map(make_binding_proc, variables, values);
  }
  
  public static object extend_frame(object var, object val, object env) {
	
	return cons(apply( make_binding_proc, 
			list(var, list("procedure", "<extension>", val))),
		env);
  }

  public static object debug(object args) {
	if (((int) length(args)) == 0)
	  return config.DEBUG;
	else 
	  config.DEBUG = (int)car(args);
	return config.DEBUG;
  }

  public static object get_type(object obj) {
	return obj.GetType();
  }

  public static object ToDouble(object obj) {
	try {
	  return Convert.ToDouble(obj);
	} catch {
	  if (obj is Rational) {
		return (double)((Rational)obj);
	  } else
		throw new Exception(string.Format("can't convert object of type '{0}' to float", obj.GetType()));
	}
  }

  public static object ToInt(object obj) {
	try {
	  return Convert.ToInt32(obj);
	} catch {
	  if (obj is Rational) {
		return (int)((Rational)obj);
	  } else	  
		throw new Exception(string.Format("can't convert object of type '{0}' to int", obj.GetType()));
	}
  }
  
  public static object make_macro_env () {
	return ((object)
		list(make_frame(
				list (
					"and", 
					"or",
					"cond", 
					"let",
					"letrec",
					"let*",
					"case",
					"record-case"
					  ),
				list (
					"and_transformer",
					"or_transformer",
					"cond_transformer",
					"let_transformer",
					"letrec_transformer",
					"let_star_transformer",
					"case_transformer",
					"record_case_transformer"))));
  }

  public static object range(object args) {
	throw new Exception("not implemented: range");
  }

  public static object list_tail(object tail, object pos) {
	throw new Exception("not implemented: list_tail");
  }

  public static object list_head(object tail, object pos) {
	throw new Exception("not implemented: list_head");
  }

  public static object read() {
	return Console.ReadLine();
  }

  public static object file_exists_q(object path_filename) {
	return File.Exists(path_filename.ToString());
  }

  public static object string_length(object str) {
	return str.ToString().Length;
  }

  public static object substring(object str, object start, object stop) {
	return str.ToString().Substring(((int)start), ((int)stop) - ((int)start));
  }

  public static void for_each(object proc, object items) {
	object current1 = items;
	// FIXME: compare on empty list assumes proper list
	// fix to work with improper lists
	while (!Same(current1, EmptyList)) {
	  apply(proc, car(current1));
	  current1 = cdr(current1);
	}
  }

  public static object apply(object proc, object args) {
	return ((Proc)proc).Call(args);
  }

  public static object apply(object proc, object args1, object args2) {
	return ((Proc)proc).Call(args1, args2);
  }

  public static object map(object proc, object args) {
	trace(1, "called: map1\n");
	object retval = EmptyList;
	object current1 = args;
	while (!Same(current1, EmptyList)) {
	  retval = cons( apply(proc, car(current1)), retval);
	  current1 = cdr(current1);
	}
	return retval;
  }

  public static object map(object proc, object args1, object args2) {
	trace(1, "called: map2\n");
	object retval = EmptyList;
	object current1 = args1;
	object current2 = args2;
	while (!Same(current1, EmptyList)) {
	  retval = cons( apply(proc, car(current1), car(current2)), retval);
	  current1 = cdr(current1);
	  current2 = cdr(current2);
	}
	return retval;
  }

  public static object make_binding(object args1, object args2) {
	return cons(args1, args2);
  }

  public static Func<object,bool> tagged_list(object test_string, object pred, object value) {
	trace(2, "called: tagged_list\n");
	return (object lyst) => {
	  if (list_q(lyst))
		return (((bool)Same(car(lyst), test_string)) && ((bool)((Predicate2)pred)(length(lyst), value)));
	  else
		return false;
	};
  }

  public static Func<object,bool> procedure_q = tagged_list("procedure", (Predicate2)GreaterOrEqual, 1);

  public static object list_to_vector(object lyst) {
	trace(2, "called: list_to_vector\n");
	int len = (int) length(lyst);
	object current = lyst;
	object[] retval = new object[len];
	for (int i = 0; i < len; i++) {
	  retval[i] = car(current);
	  current = cdr(current);
	}
	return retval;
  }

  public static object string_ref(object s, object i) {
	trace(2, "called: string_ref(s, {0})\n", i);
	return s.ToString()[(int)i];
  }

  public static object make_string(object obj) {
	trace(2, "called: make_string\n");
	if (obj == null || obj == (object) NULL) {
	  trace(2, "make_string returned: \"\\0\"\n");
	  return (object) "\0";
	}
	trace(2, "make_string returned: \"{0}\"\n", obj.ToString());
	return obj.ToString();
  }

  public static bool number_q(object datum) {
	return ((datum is int) ||
		(datum is double) ||
		(datum is Rational) ||
		(datum is BigInteger));
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
	trace(4, "calling string=?({0}, {1})", o1, o2);
	return ((o1 is string) && (o2 is string) && ((string)o1) == ((string)o2));
  }
  
  public static object string_to_list(object str) {
	trace(2, "called: string_to_list\n");
	object retval = EmptyList;
	if (str != null) {
	  string sstr = str.ToString();
	  for (int i = 0; i < sstr.Length; i++) {
		retval = cons(sstr[i], retval);
	  }
	}
	return retval;
  }

  public static object string_to_symbol(object s) {
	return new Symbol(s.ToString());
  }

  public static object string_to_integer(object str) {
	try {
	  return int.Parse(str.ToString());
	} catch (OverflowException) {
	  return BigIntegerParse(str.ToString());
	}
  }

  public static object string_to_decimal(object str) {
	return double.Parse(str.ToString());
  }

  public static object string_to_rational(object str) {
	string[] part = (str.ToString()).Split(SPLITSLASH);
	return new Rational(int.Parse(part[0]), int.Parse(part[1]));
  }

  public static void error(object code, object msg, params object[] rest) {
	config.NEED_NEWLINE = false;
	Console.WriteLine("Error in {0}: {1}", (code.ToString()), format(msg, rest));
  }

  public static void newline() {
	config.NEED_NEWLINE = false;
	Console.WriteLine("");
  }

  public static void display(object obj) {
	// FIXME: pass stdout port setting
	display(obj, null);
  }

  public static void display(object obj, object port) {
	// FIXME: add output port type
	string s = repr(obj);
	int len = s.Length;
	if (s.Substring(len - 1, 1) != "\n") 
	  config.NEED_NEWLINE = true;
	Console.Write(s);
  }

  public static void printf(object fmt, params object[] objs) {
	Console.Write(format(fmt, objs));
  }

  public static string repr(object obj) {
	trace(3, "calling repr...\n");
	if (obj == null) {
	  return "#<void>";
	} else if (obj is bool) {
	  return ((bool)obj) ? "#t" : "#f";
	} else if (obj is Array) {
	  return (string)array_to_string((object[]) obj);
	} else if (obj is double) {
	  string s = obj.ToString();
	  if (s.Contains("."))
		return s;
	  else
		return String.Format("{0}.", s);
	} else if (obj is String) {
	  return String.Format("\"{0}\"", obj);
	} else if (obj is Symbol) {
	  if (((Symbol)obj) == EmptyList)
		return "()";
	  else
		return obj.ToString();
	} else if (obj is Cons) {
	  if (procedure_q(obj)) {
		return "#<procedure>";
	  } else {
		object current = (Cons)obj;
		string retval = "";
		while (pair_q(current)) {
		  if (retval != "")
			retval += " ";
		  retval += repr(car(current));
		  current = cdr(current);
		  if (!pair_q(current) && !Same(current, EmptyList)) {
			retval += " . " + repr(current);
		  }
		}
		return "(" + retval + ")";
	  }
	} else {
	  return obj.ToString();
	}
  }

  public static string format(object msg, params object[] rest) {
	string retval = "";
	string new_msg = "";
	string smsg = msg.ToString();
	object[] orest = (object[]) rest;
	int count = 0;
	for (int i = 0; i < smsg.Length; i++) {
	  if (smsg[i] == TILDE) {
		if (smsg[i+1] == 's') {
		  new_msg += string.Format("{{0}}", count);
		  count += 1;
		  i++;
		} else if (smsg[i+1] == 'a') {
		  new_msg += string.Format("{{0}}", count);
		  count += 1;
		  i++;
		} else if (smsg[i+1] == '%') {
		  new_msg += "\n";
		  count += 1;
		  i++;
		} else
		  throw new Exception(string.Format("format needs to handle: \"{0}\"", 
				  smsg));
	  } else {
		new_msg += smsg[i];
	  }
	}
	retval = String.Format(new_msg, orest);
	return retval;
  }

  public static bool GreaterOrEqual(object args) {
	return GreaterOrEqual(car(args), cadr(args));
  }

  public static bool GreaterOrEqual(object obj1, object obj2) {
	if (obj1 is Symbol) {
	  // 	  if (obj2 is Symbol) {
	  // 		return (((Symbol)obj1) >= ((Symbol)obj2));
	  // 	  } else 
	  return false;
	} else {
	  try {
		return (ObjectType.ObjTst(obj1, obj2, false) >= 0);
	  } catch {
		return false;
	  }
	}
  }

  public static bool Same(object obj) {
	object item = car(obj);
	object current = cdr(obj);
	while (!Same(current, EmptyList)) {
	  if (! Same(item, car(current)))
		return false;
	  current = cdr(current);
	}
	return true;
  }

  public static int cmp(object obj1, object obj2) {
	trace(4, "calling cmp({0}, {1})\n", obj1, obj2);
	if (obj1 is Symbol) {
	  if (obj2 is Symbol) {
		return cmp(obj1.ToString(), obj2.ToString());
	  } else 
		return cmp(obj1.ToString(), obj2);
	} else {
	  if (obj2 is Symbol) {
		return cmp(obj1, obj2.ToString());
	  } else {
		return ObjectType.ObjTst(obj1, obj2, false);
	  }
	}
  }

  public static bool Same(object obj1, object obj2) {
	trace(4, "calling compare({0}, {1})\n", obj1, obj2);
	if (obj1 is Symbol) {
	  if (obj2 is Symbol) {
		return (obj1.ToString() == obj2.ToString());
	  } else 
		return Same(obj1.ToString(), obj2);
	} else {
	  if (obj2 is Symbol) {
		return Same(obj1, obj2.ToString());
	  } else {
		try {
		  bool retval = (ObjectType.ObjTst(obj1, obj2, false) == 0);
		  trace(4, "  compare returning: {0}\n", retval);
		  return retval;
		} catch {
		  trace(4, "  false2\n");
		  return false;
		}
	  }
	}
  }

  public static bool LessThan(object obj) {
	return LessThan(car(obj), cadr(obj));
  }

  public static bool LessThan(object obj1, object obj2) {
	return (ObjectType.ObjTst(obj1, obj2, false) < 0);
  }

  public static bool GreaterThan(object args) {
	return GreaterThan(car(args), cadr(args));
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

  public static bool Same(object obj1, object op, object obj2) {
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

  public static object Add(object obj) {
	// For adding 0 or more numbers in list
	object retval = 0;
	object current = obj;
	while (!Same(current, EmptyList)) {
	  retval = Add(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }

  public static object Multiply(object obj) {
	// For multiplying 0 or more numbers in list
	object retval = 1;
	object current = obj;
	while (!Same(current, EmptyList)) {
	  retval = Multiply(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }

  public static object Subtract(object obj) {
	// For subtracting 1 or more numbers in list
	object retval = car(obj);
	object current = cdr(obj);
	while (!Same(current, EmptyList)) {
	  retval = Subtract(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }
	
  public static object Divide(object obj) {
	// For dividing 1 or more numbers in list
	object retval = car(obj);
	object current = cdr(obj);
	while (!Same(current, EmptyList)) {
	  retval = Divide(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }
	
  public static object Add(object obj1, object obj2) {
	try {
	  return (ObjectType.AddObj(obj1, obj2));
	} catch {
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
	  } else {
		BigInteger b1 = null;
		BigInteger b2 = null;
		if (obj1 is int) {
		  b1 = makeBigInteger((int) obj1);
		} else if (obj1 is BigInteger) {
		  b1 = (BigInteger)obj1;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
		if (obj2 is int) {
		  b2 = makeBigInteger((int) obj2);
		} else if (obj2 is BigInteger) {
		  b2 = (BigInteger)obj2;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
		return b1 + b2;
	  }
	}
	throw new Exception(String.Format("unable to add {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Subtract(object obj1, object obj2) {
	try {
	  return (ObjectType.SubObj(obj1, obj2));
	} catch {
	  if (obj1 is Rational) {
		if (obj2 is Rational) {
		  return (((Rational)obj1) - ((Rational)obj2));
		} else if (obj2 is int) {
		  return (((Rational)obj1) - ((int)obj2));
		} else if (obj2 is double) {
		  return (((double)((Rational)obj1)) - ((double)obj2));
		}
	  } else if (obj2 is Rational) {
		if (obj1 is Rational) {
		  return (((Rational)obj1) - ((Rational)obj2));
		} else if (obj1 is int) {
		  return (((Rational)obj2) - ((int)obj1));
		} else if (obj1 is double) {
		  return (((double)((Rational)obj2)) - ((double)obj1));
		}
	  } else {
		BigInteger b1 = null;
		BigInteger b2 = null;
		if (obj1 is int) {
		  b1 = makeBigInteger((int) obj1);
		} else if (obj1 is BigInteger) {
		  b1 = (BigInteger)obj1;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
		if (obj2 is int) {
		  b2 = makeBigInteger((int) obj2);
		} else if (obj2 is BigInteger) {
		  b2 = (BigInteger)obj2;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
		return b1 - b2;
	  }
	}
	throw new Exception(String.Format("unable to subtract {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Multiply(object obj1, object obj2) {
	// FIXME: need hierarchy of numbers, handle rational/complex/etc
	try {
	  return ObjectType.MulObj(obj1, obj2);
	} catch {
	  if (obj1 is Rational) {
		if (obj2 is Rational) {
		  return (((Rational)obj1) * ((Rational)obj2));
		} else if (obj2 is int) {
		  return (((Rational)obj1) * ((int)obj2));
		} else if (obj2 is double) {
		  return (((double)((Rational)obj1)) * ((double)obj2));
		}
	  } else if (obj2 is Rational) {
		if (obj1 is Rational) {
		  return (((Rational)obj1) * ((Rational)obj2));
		} else if (obj1 is int) {
		  return (((Rational)obj2) * ((int)obj1));
		} else if (obj1 is double) {
		  return (((double)((Rational)obj2)) * ((double)obj1));
		}
	  } else {
		BigInteger b1 = null;
		BigInteger b2 = null;
		if (obj1 is int) {
		  b1 = makeBigInteger((int) obj1);
		} else if (obj1 is BigInteger) {
		  b1 = (BigInteger)obj1;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
		if (obj2 is int) {
		  b2 = makeBigInteger((int) obj2);
		} else if (obj2 is BigInteger) {
		  b2 = (BigInteger)obj2;
		} else
		  throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
		return b1 * b2;
	  }
	}
	throw new Exception(String.Format("unable to multiply {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Divide(object obj1, object obj2) {
	if ((obj1 is int) && (obj2 is int)) {
	  return new Rational((int)obj1, (int)obj2);
	} else {
	  try {
		return (ObjectType.DivObj(obj1, obj2));
	  } catch {
		if (obj1 is Rational) {
		  if (obj2 is Rational) {
			return (((Rational)obj1) / ((Rational)obj2));
		  } else if (obj2 is int) {
			return (((Rational)obj1) / ((int)obj2));
		  } else if (obj2 is double) {
			return (((double)((Rational)obj1)) / ((double)obj2));
		  }
		} else if (obj2 is Rational) {
		  if (obj1 is Rational) {
			return (((Rational)obj1) / ((Rational)obj2));
		  } else if (obj1 is int) {
			return (((Rational)obj2) / ((int)obj1));
		  } else if (obj1 is double) {
			return (((double)((Rational)obj2)) / ((double)obj1));
		  }
		} else {
		  BigInteger b1 = null;
		  BigInteger b2 = null;
		  if (obj1 is int) {
			b1 = makeBigInteger((int) obj1);
		  } else if (obj1 is BigInteger) {
			b1 = (BigInteger)obj1;
		  } else
			throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
		  if (obj2 is int) {
			b2 = makeBigInteger((int) obj2);
		  } else if (obj2 is BigInteger) {
			b2 = (BigInteger)obj2;
		  } else
			throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
		  return b1 / b2;
		}
	  }
	}
	throw new Exception(String.Format("unable to divide {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  // List functions -----------------------------------------------

  public static object member(object obj1, object obj2) {
	trace(2, "calling member({0}, {1})\n", obj1, obj2);
	if (list_q(obj2)) {
	  object current = obj2;
	  while (!Same(current, EmptyList)) {
		if (Same(obj1, car(current)))
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
	trace(3, "called: length\n");
	if (list_q(obj)) {
	  if (null_q(obj)) {
		trace(3, "length returned: {0}\n", 0);
		return 0;
	  } else {
		int len = 0;
		object current = (Cons)obj;
		while (!Same(current, EmptyList)) {
		  len++;
		  current = cdr(current);
		}
		trace(3, "length returned: {0}\n", len);
		return len;
	  }
	} else
	  throw new Exception("attempt to take length of a non-list");
  }

  public static bool list_q(object o1) {
	return (null_q(o1) || pair_q(o1));
  }

  public static object list_to_string(object lyst) {
	String retval = "";
	if (lyst is Cons) {
	  object current = lyst;
	  while (!Same(current, EmptyList)) {
		retval += car(current).ToString();
		current = cdr(current);
	  }
	}
	return retval;
  }

  static object pivot (object l) {
	if (null_q(l))
	  return "done";
	else if (null_q(cdr(l)))
	  return "done";
	else if (cmp(car(l), cadr(l)) <= 0)
	  return pivot(cdr(l));
	else
	  return car(l);
  }

  // usage: (partition 4 '(6 4 2 1 7) () ()) -> returns partitions
  static object partition (object piv,  object l, object p1, object p2) {
	if (null_q(l))
	  return list(p1, p2);
	else if (cmp(car(l), piv) < 0)
	  return partition(piv, cdr(l), cons(car(l), p1), p2);
	else
	  return partition(piv, cdr(l), p1, cons(car(l), p2));
  }

  public static object sort(object l) {
	object piv = pivot(l);
	if (Same(piv, "done")) return l;
	object parts = partition(piv, l, EmptyList, EmptyList);
	return append(sort(car(parts)),
		          sort(cadr(parts)));
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

  public static void set_cdr_b(object obj) {
	set_cdr_b(car(obj), cadr(obj));
  }
  
  public static void set_cdr_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.cdr = item;
  }

  public static void set_car_b(object obj) {
	set_car_b(car(obj), cadr(obj));
  }
  public static void set_car_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.car = item;
  }

  public static object append(params object[] obj) {
	return Append(obj[0], obj[1]);
  }

  public static object append(object obj) {
	return Append(car(obj), cadr(obj));
  }

  public static object Append(object obj1, object obj2) {
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

  public static object sqrt(object obj) {
	if (pair_q(obj)) {
	  obj = car(obj);
	  if (obj is int)
		return Math.Sqrt((int)obj);
	  else
		throw new Exception(String.Format("can't take sqrt of this type of number: {0}", obj));
	}
	throw new Exception("need to apply procedure to list");
  }

  public static object cons(object obj) {
	return cons(car(obj), cadr(obj));
  }

  public static object cons(object obj1, object obj2) {
	return (object) new Cons(obj1, obj2);
  }

  public static object cdr(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).cdr;
	else
	  throw new Exception(string.Format("cdr: object is not a list: {0}",
			  obj));
  }

  public static object car(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).car;
	else
	  throw new Exception(string.Format("car: object is not a list: {0}",
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
		return "(...)";
	  }
	}

	public string UnSafeToString() {
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
	  if (denominator == 0)
		throw new Exception("cannot represent rationals with a zero denominator");
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
	  if (n1 > n2) return checked((n2 / GCD(n1, n2)) * n1);
	  else         return checked((n1 / GCD(n1, n2)) * n2);
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
	  //if (denominator != 1)
	  return string.Format("{0}/{1}", numerator, denominator);
	  //else
	  //return numerator.ToString();
	}

	public static implicit operator double(Rational f) {
	  return (((double) f.numerator) / ((double) f.denominator));
	}

	public static implicit operator int(Rational f) {
	  return f.numerator / f.denominator;
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

	public static Rational operator +(int i, Rational f1) {
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

  public static void pretty_print(object obj) {
	// FIXME: need to make this safe
	// Just get representation for now
	printf(repr(obj));
	newline();
  }  

  public static bool isTokenType(List<object> token, string tokenType) {
	return Same(token[0], tokenType);
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

  public static bool memq(object obj) {
	return memq(car(obj), cadr(obj));
  }

  public static bool memq(object item1, object list) {
	if (list is Cons) {
	  object current = list;
	  while (! Same(current, EmptyList)) {
		if (Same(item1, car(current))) {
		  return true;
		}
		current = cdr(current);
	  }
	  return false;
	}
	return false;
  }

  public static object vector_to_list(object obj) {
	return list(obj);
  }

  public static object read_content(object filename) {
	return File.OpenText(filename.ToString()).ReadToEnd();
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
	// Same tests:
	printf("hello == hello: {0}\n", Same("hello", "hello"));
	printf("hello == hel: {0}\n", Same("hello", "hel"));
	printf("hello == helloo: {0}\n", Same("hello", "helloo"));
	printf("4.1 == 4: {0}\n", Same(4.1, 4));
	printf("hello == true: {0}\n", Same("hello", true));
	
	printf("() == (): {0}\n", Same(list(), list()));

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
	printf("t = {0}\n", repr(t));
	printf("member(hello, t) : {0}\n", repr(member("hello", t)));
	printf("member(a, t) : {0}\n", repr(member("a", t)));
	printf("member(c, t) : {0}\n", repr(member("c", t)));
	printf("(): {0}\n", repr(list()));
	printf("list(t): {0}\n", repr(list(t)));
	printf("length(list(t)): {0}\n", length(list(t)));
	printf("length(cdr(list(t))): {0}\n", length(cdr(list(t))));
	printf("length(car(list(t))): {0}\n", length(car(list(t))));
	printf("cons(\"X\", list(t))): {0}\n", repr(cons("X", list(t))));
	printf("x is: {0}\n", repr("x"));
	printf("t is: {0}\n", repr(t));
	printf("list(): {0}\n", list());
	printf("cons('a', list()): {0}\n", cons("a", list()));
	printf("cons('a', 'b'): {0}\n", cons("a", "b"));

	printf("cons('a', null): {0}\n", cons("a", null));

	printf("string-append('test', NULL): \"{0}\"\n", 	 
		string_append ((object) "test",
			(object) make_string ((object) NULL)));

	int val = 15;
	printf("BigInteger, long, int:\n");
	printf("  {0}: {1} == {2} == WRONG! {3}\n", val,
		bigfact(makeBigInteger(val)), longfact(val), intfact(val));
	printf("Multiply:\n");
	printf("15: {0} \n", Multiply(Multiply( Multiply( intfact(12), 13), 14), 15));

	printf("1827391823712983712983712938: {0}\n", BigIntegerParse("1827391823712983712983712938"));
  }

  public static long longfact(long n) {
	if (n == 1) return n;
	return n * longfact(n - 1);
  }

  public static int intfact(int n) {
	if (n == 1) return n;
	return n * intfact(n - 1);
  }

  public static BigInteger bigfact(BigInteger n) {
	if (n == 1) return n;
	return n * bigfact(n - 1);
  }

}
