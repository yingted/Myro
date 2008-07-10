using System; // Exception
using System.Collections.Generic; // List

//-----------------------------------------------------------------------
// scanner - character stream represented as a position number

public class Scanner {

    static string charsToScan = "";

    public static char first(int n) {
	return charsToScan[n];
    }

    public static int remaining(int n) {
	return n + 1;
    }

    public static List<object> scanInput(string input) {
	charsToScan = input + "\0";
	int chars = 0;
	List<object> tokens = makeList();
	while (true) {
	    List<object> list = applyAction(makeList("goto", "start-state"), 
					    makeList(), chars);
	    List<object> token = (List<object>) list[0]; 
	    int charsLeft = (int) list[1];
	    tokens.Add(token);
	    if (isTokenType(token, "end-marker")) {
		return tokens;
	    } else {
		chars = charsLeft;
	    }
	}
    }

    //-----------------------------------------------------------------------
    // scanner actions
    
    // <action> ::= ("shift", <next-action>)
    //            | ("replace", <new-char>, <next-action>)
    //            | ("drop", <next-action>)
    //            | ("goto", <state>)
    //            | ("emit", <token-type>)
    
    public static List<object> applyAction(List<object> action, List<object> buffer, int chars) {
	while (true) {
	    string tag = (string) action[0];
	    if (tag == "shift") {
		List<object> next = (List<object>) action[1];
		buffer.Add(first(chars));
		chars = remaining(chars);
		action = next;
	    } else if (tag == "replace") {
		char newChar = (char) action[1];
		List<object> next = (List<object>) action[2];
		buffer.Add(newChar);
		chars = remaining(chars);
		action = next;
	    } else if (tag == "drop") {
		List<object> next = (List<object>) action[1];
		chars = remaining(chars);
		action = next;
	    } else if (tag == "goto") {
		string state = (string) action[1];
		action = applyState(state, first(chars));
	    } else if (tag == "emit") {
		string tokenType = (string) action[1];
		List<object> token = convertBufferToToken(tokenType, buffer);
		return makeList(token, chars);
	    } else {
		throw new Exception(String.Format("invalid action: {0} in applyAction", action));
	    }
	}
    }
	
    public static void scanError(char c) {
	if (c == '\0') {
	    throw new Exception("unexpected end of input in scan");
	} else {
	    throw new Exception(String.Format("unexpected character {0} encountered", c));
	}
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
    
    public static string listToString(List<object> list) {
	string retval = "";
	foreach (object item in list) {
	    retval += item.ToString();
	}
	return retval;
    }
    
    public static string prettyPrint(List<object> list) {
	string retval = "";
	foreach (object item in list) {
	    if (retval != "")
		retval += " ";
	    if (item is List<object>)
		retval += prettyPrint((List<object>) item);
	    else
		retval += item.ToString();
	}
	return "(" + retval + ")";
    }
    
    public static List<object> makeNumber(List<object> list) {
	string s = listToString(list);
	if (s.IndexOf('.') >= 0)
	    return makeList("double", double.Parse(s)); 
	else
	    return makeList("integer", int.Parse(s)); // or System.Convert.ToInt32(s, 10);
    }
    
    public static List<object> makeList(params object[] args) {
	return new List<object>(args);
    }
    
    public static List<object> convertBufferToToken(string tokenType, List<object> buffer) {
	if (tokenType == "number") {
	    return makeNumber(buffer);
	} else if (tokenType == "identifier") {
	    return makeList("identifier", listToString(buffer));
	} else if (tokenType == "boolean") {
	    string sbool = (string) buffer[0];
	    return makeList("boolean", (sbool.IndexOf('t') >= 0 || sbool.IndexOf('T') >= 0));
	} else if (tokenType == "character") {
	    return makeList("character", buffer[0]);
	} else if (tokenType == "named-character") {
	    string name = listToString(buffer);
	    if (name == "nul") { 
		return makeList("character", '\0'); 
	    } else if (name == "space") { 
		return makeList("character", ' '); 
	    } else if (name == "tab") { 
		return makeList("character", '\t'); 
	    } else if (name == "newline") { 
		return makeList("character", '\n'); 
	    } else if (name == "linefeed") { 
		return makeList("character", '\n'); 
	    } else if (name == "backspace") { 
		return makeList("character", '\b'); 
	    } else if (name == "return") { 
		return makeList("character", '\r'); 
	    } else if (name == "page") { 
		return makeList("character", '\f'); 
	    } else {
		throw new Exception(String.Format("invalid character name #\\{0}", name));
	    }
	} else if (tokenType == "string") {
	    return makeList("string", listToString(buffer));
	} else {
	    return makeList(tokenType);
	}
    }
    
    public static bool isTokenType(List<object> token, string tokenType) {
	return (((string) token[0]) == tokenType);
    }
    
    //-----------------------------------------------------------------------
    // character categories
    
    static string alphabeticChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static string numericChars = "0123456789";
    static string whitespaceChars = " \t\n\r\f\b";
    static string delimiterChars = whitespaceChars + "()[]\";#\0";
    static string initialChars = alphabeticChars + "!$%&*/:<=>?^_~";
    static string specialSubsequentChars = "+-@.";
    static string subsequentChars = initialChars + numericChars + specialSubsequentChars;
    static string signChars = "+-";
    static string booleanChars = "tTfF";
    
    //-----------------------------------------------------------------------
    // finite state automaton
    
    // this function is just a big lookup table
    public static List<object> applyState(string state, char c) {
	if (state == "start-state") {
	    if (whitespaceChars.IndexOf(c) >= 0) { 
		return makeList("drop", makeList("goto", "start-state"));
	    } else if (c == ';') {
		return makeList("drop", makeList("goto", "comment-state"));
	    } else if (c == '(') { 
		return makeList("drop", makeList("emit", "lparen"));
	    } else if (c == '[') {
		return makeList("drop", makeList("emit", "lbracket"));
	    } else if (c == ')') { 
		return makeList("drop", makeList("emit", "rparen"));
	    } else if (c == ']') { 
		return makeList("drop", makeList("emit", "rbracket"));
	    } else if (c == '\'') { 
		return makeList("drop", makeList("emit", "apostrophe"));
	    } else if (c == '#') { 
		return makeList("drop", makeList("goto", "hash-prefix-state"));
	    } else if (c == '"') { 
		return makeList("drop", makeList("goto", "string-state"));
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (signChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "signed-state"));
	    } else if (c == '.') { 
		return makeList("shift", makeList("goto", "decimal-point-state"));
	    } else if (numericChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "whole-number-state"));
	    } else if (c == '\0') {
		return makeList("emit", "end-marker");
	    } else {
		scanError(c);
	    }
	} else if (state == "comment-state") {
	    if (c == '\n') {
		return makeList("drop", makeList("goto", "start-state"));
	    } else if (c == '\0') { 
		return makeList("goto", "start-state");
	    } else {
		return makeList("drop", makeList("goto", "comment-state"));
	    }
	} else if (state == "hash-prefix-state") {
	    if (booleanChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("emit", "boolean"));
	    } else if (c == '\\') {
		return makeList("drop", makeList("goto", "character-state"));
	    } else { 
		scanError(c);
	    }
	} else if (state == "character-state") {
	    if (alphabeticChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "alphabetic-character-state"));
	    } else if (c != '\0') {
		return makeList("shift", makeList("emit", "character"));
	    } else { 
		scanError(c);
	    }
	} else if (state == "alphabetic-character-state") {
	    if (alphabeticChars.IndexOf(c) >= 0) { 
		return makeList("shift", makeList("goto", "named-character-state"));
	    } else {
		return makeList("emit", "character");
	    }
	} else if (state == "named-character-state") {
	    if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "named-character");
	    } else {
		return makeList("shift", makeList("goto", "named-character-state"));
	    }
	} else if (state == "string-state") {
	    if (c == '"') {
		return makeList("drop", makeList("emit", "string"));
	    } else if (c == '\\') {
		return makeList("drop", makeList("goto", "string-escape-state"));
	    } else if (c == '\0') {
		scanError(c);
	    } else {
		return makeList("shift", makeList("goto", "string-state"));
	    }
	} else if (state == "string-escape-state") {
	    if (c == '"') {
		return makeList("shift", makeList("goto", "string-state"));
	    } else if (c == '\\') {
		return makeList("shift", makeList("goto", "string-state"));
	    } else if (c == 'b') {
		return makeList("replace", '\b', makeList("goto", "string-state"));
	    } else if (c == 'f') {
		return makeList("replace", '\f', makeList("goto", "string-state"));
	    } else if (c == 'n') {
		return makeList("replace", '\n', makeList("goto", "string-state"));
	    } else if (c == 't') {
		return makeList("replace", '\t', makeList("goto", "string-state"));
	    } else if (c == 'r') {
		return makeList("replace", '\r', makeList("goto", "string-state"));
	    } else {
		scanError(c);
	    }
	} else if (state == "identifier-state") {
	    if (subsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "identifier");
	    } else { 
		scanError(c);
	    }
	} else if (state == "signed-state") {
	    if (numericChars.IndexOf(c) >= 0) { 
		return makeList("shift", makeList("goto", "whole-number-state"));
	    } else if (c == '.') {
		return makeList("shift", makeList("goto", "signed-decimal-point-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "identifier");
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (specialSubsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else {
		scanError(c);
	    }
	} else if (state == "decimal-point-state") {
	    if (numericChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "fractional-number-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "dot");
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (specialSubsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else {
		scanError(c);
	    }
	} else if (state == "signed-decimal-point-state") {
	    if (numericChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "fractional-number-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "identifier");
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (specialSubsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else {
		scanError(c);
	    }
	} else if (state == "whole-number-state") {
	    if (numericChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "whole-number-state"));
	    } else if (c == '.') {
		return makeList("shift", makeList("goto", "fractional-number-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "number");
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (specialSubsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else {
		scanError(c);
	    }
	} else if (state == "fractional-number-state") {
	    if (numericChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "fractional-number-state"));
	    } else if (delimiterChars.IndexOf(c) >= 0) {
		return makeList("emit", "number");
	    } else if (initialChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else if (specialSubsequentChars.IndexOf(c) >= 0) {
		return makeList("shift", makeList("goto", "identifier-state"));
	    } else {
		scanError(c);
	    }
	} else {
	    throw new Exception(String.Format("invalid state {0} in applyState", state));
	}
	throw new Exception("should not ever get here");
    }

    // scans an entire file and returns a list of all of the tokens
    public static List<object> scanFile(string filename) {
	string content = "";
	//f = open(filename);
	//string content = f.read();
	//f.close();
	return scanInput(content);
    }

    public static void Main(string[] args) {
	// example:
	// >>> scanFile("scanner.ss")
	string s = arrayToString(args);
	System.Console.WriteLine("Scanning: '{0}'...", s);
	System.Console.WriteLine(prettyPrint(scanInput(s)));
    }
}