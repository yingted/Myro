using System; // Exception
using System.IO; // File
using System.Collections.Generic; // List

//-----------------------------------------------------------------------
// reader (registerized)

public class Reader {

  delegate void Function();
  
  // global registers
  static List<object> tokens_reg = null;
  static List<object> k_reg = null;
  static string terminator_reg = null;
  static object sexp_reg = null;
  static Function pc = null;
  static string keyword_reg = null;
  
  static SchemeSymbol EmptyList = new SchemeSymbol("()");
  
  public static List<object> makeList(params object[] args) {
	return new List<object>(args);
  }
  
  public static object readDatum(string input) {
	tokens_reg = Scanner.scanInput(input);
	k_reg = makeList("init-cont");
	pc =  new Function(readSexp);
	return run();
  }
  
  // file reader
  public static object readFile(string filename) {
	tokens_reg = Scanner.scanInput(readContent(filename));
	k_reg = null; 
	pc = new Function(printSexps);
	return run();
  }
  
  public static string readContent(string filename) {
	String content = File.OpenText(filename).ReadToEnd();
	return content;
  }
  
  // the trampoline
  public static object run() {
	while (pc != null) {
	  pc();
	}
	return sexp_reg;
  }
  
  public static void readSexp() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "integer") {
	  sexp_reg = new ExactNumber(int.Parse((string)token[1]));
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "decimal") {
	  sexp_reg = new InexactNumber(double.Parse((string)token[1]));
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "rational") {
	  sexp_reg = new ExactNumber(
		  int.Parse((string)token[1]), int.Parse((string)token[2]));
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "boolean") {
	  sexp_reg = new SchemeBoolean((bool) token[1]);
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "character") {
	  char ch = (char) token[1];
	  sexp_reg = new SchemeCharacter(ch);
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "string") {
	  string str = (string) token[1];
	  sexp_reg = new SchemeString(str);
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "identifier") {
	  string id = (string) token[1];
	  sexp_reg = (object) new SchemeSymbol(id);
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else if (tag == "apostrophe") {
	  keyword_reg = "quote";
	  pc = new Function(readAbbreviation);
	} else if (tag == "backquote") {
	  keyword_reg = "quasiquote";
	  pc = new Function(readAbbreviation);
	} else if (tag == "comma") {
	  keyword_reg = "unquote";
	  pc = new Function(readAbbreviation);
	} else if (tag == "comma-at") {
	  keyword_reg = "unquote-splicing";
	  pc = new Function(readAbbreviation);
	} else if (tag == "lparen") {
	  tokens_reg.RemoveAt(0);
	  if (isTokenType(((List<object>) tokens_reg[0]), "dot")) 
		pc = new Function(readError);
	  else
		terminator_reg = "rparen";
	  pc = new Function(readSexpSequence);
	} else if (tag == "lbracket") {
	  tokens_reg.RemoveAt(0);
	  if (isTokenType(((List<object>) tokens_reg[0]), "dot")) {
	    pc = new Function(readError);
	  } else {
	    terminator_reg = "rbracket";
	    pc = new Function(readSexpSequence);
	  }
	} else if (tag == "lvector") {
	  tokens_reg.RemoveAt(0);
	  k_reg = makeList("vector-cont", k_reg);
	  pc = new Function(readVector);
	} else {
	  pc = new Function(readError);
	}
  }
  
  public static void readAbbreviation() {
    tokens_reg.RemoveAt(0);
    k_reg = makeList("abbreviation-cont", keyword_reg, k_reg);
    pc = new Function(readSexp);
  }

  public static void readSexpSequence() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "rparen" || tag == "rbracket") {
	    sexp_reg = EmptyList;
	    pc = new Function(closeSexpSequence);
	} else if (tag == "dot") {
	    tokens_reg.RemoveAt(0);
	    k_reg = makeList("dot-cont", terminator_reg, k_reg);
	    pc = new Function(readSexp);
	} else {
	    k_reg = makeList("seq1-cont", terminator_reg, k_reg);
	    pc = new Function(readSexp);
	}
    }

    public static void closeSexpSequence() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "rparen" || tag == "rbracket") {
	    if (isTokenType(token, terminator_reg)) {
		tokens_reg.RemoveAt(0);
		pc = new Function(applyCont);
	    } else if (terminator_reg == "rparen") {
		throw new Exception("parenthesized list terminated by bracket");
	    } else if (terminator_reg == "rbracket") {
		throw new Exception("bracketed list terminated by parenthesis");
	    } else {
		throw new Exception("should never reach here");
	    }
	} else {
	    pc = new Function(readError);
	}
    }

  public static void readVector() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "rparen") {
	  sexp_reg = EmptyList;
	  tokens_reg.RemoveAt(0);
	  pc = new Function(applyCont);
	} else {
	  k_reg = makeList("vector-sexp1-cont", k_reg);
	  pc = new Function(readSexp);
	}
  }
  
  public static void readError() {
	List<object> token = (List<object>) tokens_reg[0];
	if (isTokenType(token, "end-marker")) {
	  throw new Exception("unexpected end of input");
	} else {
	  throw new Exception(String.Format("unexpected token {0} encountered", token));
	}
  }
  
  public static void printSexps() {
	List<object> token = (List<object>) tokens_reg[0];
	if (isTokenType(token, "end-marker")) {
	  sexp_reg = new SchemeSymbol("done");
	  pc = null;
	} else {
	  k_reg = makeList("print-sexps-cont");
	  pc = new Function(readSexp);
	}
  }
  
  // continuations
  
  public static void applyCont() {
	string tag = (string) k_reg[0];
	//Console.WriteLine("   applyCont with {0}", tag);
	if (tag == "init-cont") {
	  if (isTokenType((List<object>) tokens_reg[0], "end-marker")) {
		pc = null;
	  } else {
		throw new Exception(String.Format("tokens left over: {0}", tokens_reg));
	  }
	} else if (tag == "abbreviation-cont") {
	  string keyword = (string) k_reg[1];
	  List<object> k = (List<object>) k_reg[2];
	  k_reg = k;
	  sexp_reg = new Cons(new SchemeSymbol(keyword), new Cons(sexp_reg, EmptyList));
	  pc = new Function(applyCont);
	} else if (tag == "dot-cont") {
	  string expectedTerminator = (string) k_reg[1];
	  List<object> k = (List<object>) k_reg[2];
	  terminator_reg = expectedTerminator;
	  k_reg = k;
	  pc = new Function(closeSexpSequence);
	} else if (tag == "seq1-cont") {
	  string expectedTerminator = (string) k_reg[1];
	  List<object> k = (List<object>) k_reg[2];
	  terminator_reg = expectedTerminator;
	  k_reg = makeList("seq2-cont", sexp_reg, k);
	  pc = new Function(readSexpSequence);
	} else if (tag == "seq2-cont") {
	  List<object> k = null;
	  object sexp1 = null;
	  if (k_reg[1] is List<object>) {
		sexp1 = (List<object>) k_reg[1];
	  } else {
		sexp1 = k_reg[1];
	  }
	  if (k_reg[2] is List<object>) {
		k = (List<object>) k_reg[2];
	  } else {
		throw new Exception("invalid k_reg list2");
	  }
	  k_reg = k;
	  sexp_reg = new Cons(sexp1, sexp_reg);
	  pc = new Function(applyCont);
	} else if (tag == "print-sexps-cont") {
	  prettyPrint(sexp_reg);
	  pc = new Function(printSexps);
	} else if (tag == "vector-cont") {
	  List<object> k = (List<object>) k_reg[1];
	  k_reg = k;
	  sexp_reg = new Vector(sexp_reg);
	  pc = new Function(applyCont);
	} else if (tag == "vector-sexp1-cont") {
	  List<object> k = (List<object>) k_reg[1];
	  k_reg = makeList("vector-rest-cont", sexp_reg, k);
	  pc = new Function(readVector);
	} else if (tag == "vector-rest-cont") {
	  object sexp1 = k_reg[1];
	  List<object> k = (List<object>) k_reg[2];
	  k_reg = k;
	  sexp_reg = new Cons(sexp1, sexp_reg);
	  pc = new Function(applyCont);
	} else {
	  throw new Exception(String.Format("invalid continuation {0} in applyCont", k_reg));
	}
  }
  
  //-----------------------------------------------------------------------
  // S-expression representations
  
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
  
  //-----------------------------------------------------------------------
  // examples:
  // >>> readDatum("apple")
  // >>> readDatum("#T")
  // >>> readDatum("(a (b c (d)))")
  // >>> readDatum("(a b c 1 2 -3.14 #f \"hello there\" #\\newline (e [f . x] . 4) ())")
  // >>> readFile("reader.ss")
  // >>> readDatum("(a 'b (quote c) #(1 2 d))")
  // >>> readDatum("2/3") + readDatum("3/4")
  
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
  
  public static void Main(string[] args) {
	if (args[0] == "exp") {
	  string s = args[1];
	  System.Console.WriteLine("Parsing expression: '{0}'...", s);
	  System.Console.WriteLine(prettyPrint(readDatum(s)));
	} else {
	  System.Console.WriteLine("Parsing file: '{0}'...", args[0]);
	  System.Console.WriteLine(prettyPrint(readFile(args[0])));
	}
  }
  
}
