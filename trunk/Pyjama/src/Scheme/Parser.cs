using System; // Exception
using System.IO; // File
using System.Collections.Generic; // List

//-----------------------------------------------------------------------
// parser (registerized)

public class Parser {

    delegate void Function();

    // global registers
    static List<object> tokens_reg = null;
    static List<object> k_reg = null;
    static string terminator_reg = null;
    static object sexp_reg = null;
    static Function pc = null;

    static SchemeSymbol EmptyList = new SchemeSymbol("()");

    public static List<object> makeList(params object[] args) {
	return new List<object>(args);
    }
    
    public static object parse(string input) {
	tokens_reg = Scanner.scanInput(input);
	k_reg = makeList("init-cont");
	pc =  new Function(parseSexp);
	return run();
    }

    // the trampoline
    public static object run() {
	while (pc != null) {
	    pc();
	}
	return sexp_reg;
    }

    public static void parseSexp() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "integer") {
	    sexp_reg = new SchemeInteger((int) token[1]);
	    tokens_reg.RemoveAt(0);
	    pc = new Function(applyCont);
	} else if (tag == "double") {
	    sexp_reg = new SchemeDouble((double) token[1]);
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
	    tokens_reg.RemoveAt(0);
	    k_reg = makeList("quote-cont", k_reg);
	    pc = new Function(parseSexp);
	} else if (tag == "lparen") {
	    tokens_reg.RemoveAt(0);
	    if (isTokenType(((List<object>) tokens_reg[0]), "dot")) 
		pc = new Function(parseError);
	    else
		terminator_reg = "rparen";
	    pc = new Function(parseSexpSequence);
	} else if (tag == "lbracket") {
	    tokens_reg.RemoveAt(0);
	    if (isTokenType(((List<object>) tokens_reg[0]), "dot")) {
		pc = new Function(parseError);
	    } else {
		terminator_reg = "rbracket";
		pc = new Function(parseSexpSequence);
	    }
	} else {
	    pc = new Function(parseError);
	}
    }

    public static void parseSexpSequence() {
	List<object> token = (List<object>) tokens_reg[0];
	string tag = (string) token[0];
	if (tag == "rparen" || tag == "rbracket") {
	    sexp_reg = EmptyList;
	    pc = new Function(closeSexpSequence);
	} else if (tag == "dot") {
	    tokens_reg.RemoveAt(0);
	    k_reg = makeList("dot-cont", terminator_reg, k_reg);
	    pc = new Function(parseSexp);
	} else {
	    k_reg = makeList("seq1-cont", terminator_reg, k_reg);
	    pc = new Function(parseSexp);
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
	    pc = new Function(parseError);
	}
    }

    public static void parseError() {
	List<object> token = (List<object>) tokens_reg[0];
	if (isTokenType(token, "end-marker")) {
	    throw new Exception("unexpected end of input");
	} else {
	    throw new Exception(String.Format("unexpected token {0} encountered", token));
	}
    }

    // file loader

    public static object loadFile(string filename) {
	tokens_reg = Scanner.scanInput(readContent(filename));
	pc = new Function(processSexps);
	return run();
    }

    public static void processSexps() {
	List<object> token = (List<object>) tokens_reg[0];
	if (isTokenType(token, "end-marker")) {
	    sexp_reg = new SchemeSymbol("done");
	    pc = null;
	} else {
	    k_reg = makeList("process-cont");
	    pc = new Function(parseSexp);
	}
    }

    public static string readContent(string filename) {
	String content = File.OpenText(filename).ReadToEnd();
	return content;
    }

    // continuations

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

    public static void applyCont() {
	string tag = (string) k_reg[0];
	//Console.WriteLine("   applyCont with {0}", tag);
	if (tag == "init-cont") {
	    if (isTokenType((List<object>) tokens_reg[0], "end-marker")) {
		pc = null;
	    } else {
		throw new Exception(String.Format("tokens left over: {0}", tokens_reg));
	    }
	} else if (tag == "quote-cont") {
	    List<object> k = (List<object>) k_reg[1];
	    k_reg = k;
	    sexp_reg = new Cons(new SchemeSymbol("quote"), new Cons(sexp_reg, EmptyList));
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
	    pc = new Function(parseSexpSequence);
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
	} else if (tag == "process-cont") {
	    prettyPrint(sexp_reg);
	    pc = new Function(processSexps);
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
    
    public class SchemeInteger {
	int num;
	
	public SchemeInteger(int num) {
	    this.num = num;
	}

        public override bool Equals(object other) {
	    return (other is SchemeInteger && this.num == ((SchemeInteger)other).num);
	}

        public override int GetHashCode() {
	    return num.GetHashCode();
	}

	public override string ToString() {
	    return this.num.ToString();
	}
    }

    public class SchemeDouble {
	double num;
	
	public SchemeDouble(double num) {
	    this.num = num;
	}

        public override bool Equals(object other) {
	    return (other is SchemeDouble && this.num == ((SchemeDouble)other).num);
	}

        public override int GetHashCode() {
	    return num.GetHashCode();
	}

	public override string ToString() {
	    return this.num.ToString();
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
    // >>> parse("apple")
    // >>> parse("#T")
    // >>> parse("(a (b c (d)))")
    // >>> parse("(a b c 1 2 -3.14 #f \"hello there\" #\\newline (e [f . x] . 4) ())")
    // >>> loadFile("scanner-parser.ss")
    
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
	// example:
	// >>> scanFile("scanner.ss")
	string s = arrayToString(args);
	System.Console.WriteLine("Parsing: '{0}'...", s);
	System.Console.WriteLine(prettyPrint(parse(s)));
    }

}