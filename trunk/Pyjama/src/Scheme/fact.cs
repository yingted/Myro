
using System;
using System.Collections.Generic; // List


public class fact_rm {

  static string pc = null;
  static object final_reg = null;
  static object k_reg = null;
  static object n_reg = null;
  static object value_reg = null;
  static object temp_1 = null;


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
  
  static SchemeSymbol EmptyList = new SchemeSymbol("()");

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

  public static object make_cont(params object[] args) {
	Object result = EmptyList;
	int count = ((Array)args).Length;
	for (int i = 0; i < count; i++) {
	  result = new Cons(args[count - i - 1], result);
	}
	return ((object)new Cons("continuation", result));
  }

  public static void error(string code, string msg, params object[] rest) {
	Console.WriteLine(code + " " + msg);
  }

  public static void fact(int n) {
	n_reg = n;
	pc = "fact_cps";
	k_reg = make_cont("<cont-1>");
  }

  public static void fib(int n) {
	n_reg = n;
	pc = "fib_cps";
	k_reg = make_cont("<cont-1>");
  }
  
  public static void Main(string [] args) {
	fact(5);
	trampoline();
	fib(35);
	trampoline();
  }

  public static void trampoline() {
	while (true) {
	  if (pc == ""){
		Console.WriteLine(final_reg);
		break;
	  } else if (pc == "fact_cps") {
		fact_cps();
	  } else if (pc == "fib_cps") {
		fib_cps();
	  } else if (pc == "apply_cont") {
		apply_cont();
	  } else {
		Console.WriteLine("Error in trampoline: " + pc);
	  }
	}
  }

  public static object list_ref(object obj, int pos) {
	Cons result = ((Cons)obj);
	for (int i = 0; i < pos; i++) {
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

  public static void apply_cont() { 
	{ 
	  object temp_1 = null;
	  temp_1 = cdr(k_reg);
	  if (((string)car(temp_1)) == "<cont-1>") { 
		final_reg = value_reg;
		pc = "";
	  } else if (((string)car(temp_1)) == "<cont-2>") { 
		object v1 = null;
		object k = null;
		k = list_ref(temp_1, 2);
		v1 = list_ref(temp_1, 1);
		{ 
		  value_reg = ((int)v1) + ((int)value_reg);
		  k_reg = k;
		  pc = "apply_cont";
		} 
	  } else if ((bool) ((((string)car(temp_1)) == "<cont-3>"))) { 
		object n = null;
		object k = null;
		k = list_ref(temp_1, 2);
		n = list_ref(temp_1, 1);
		{ k_reg = make_cont("<cont-2>", value_reg, k);
		  n_reg = (((int)n) - 2);
		  pc = "fib_cps";
		} 
	  } else if (((string)car(temp_1)) == "<cont-4>") { 

		object n = null;
		object k = null;
		k = list_ref(temp_1, 2);
		n = list_ref(temp_1, 1);
		{ 
		  value_reg = ((object) (((int)n) * ((int)value_reg)));
		  k_reg = k;
		  pc = "apply_cont";
		} 
	  } else error("apply-cont", "bad continuation: ~a", k_reg);
	} 
  }

  public static void fib_cps() { 
	if (((int)n_reg) == 1) { 
	  value_reg = 1;
	  pc = "apply_cont";
	} else if (((int)n_reg) == 2) { 
	  value_reg = 1;
	  pc = "apply_cont";
	} else { k_reg = make_cont("<cont-3>", n_reg, k_reg);
	  n_reg = (((int)n_reg) - 1);
	  pc = "fib_cps";
	} }

  public static void fact_cps() { 
	if (((int)n_reg) == 0) { 
	  value_reg = 1;
	  pc = "apply_cont";
	} else { k_reg = make_cont("<cont-4>", n_reg, k_reg);
	  n_reg = (((int)n_reg) - 1);
	  pc = "fact_cps";
	} 
  }
}

