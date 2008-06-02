using System;
using System.Diagnostics;
using System.Reflection;
using System.Threading;
using System.IO;

namespace Scheme
{
    // Expressions 
    public abstract class Expression 
    {
        abstract public object Eval(Env globalEnv, Env localEnv);

        static public Object[] Eval_Rands(Expression[] rands, Env globalEnv, Env localEnv)
        {
            if (rands == null)
                return null;

	    //Console.WriteLine("Eval_Rands: rands.Length: {0}", rands.Length);

            Object[] dest = new Object[rands.Length];

            for (int i=0; i<rands.Length; i++)
                dest[i] = rands[i].Eval(globalEnv, localEnv);
        
            return dest;
        }

        static public Expression Parse(object a)
        {
	    int pos = 0;
            if (a is Symbol) 
            {
		// FIXME: get-property interfers with dot-notation of assemblies
                if (((Symbol) a).val.IndexOf(".") == -1)
                    return new Var(a as Symbol);
                else
                {
                    string aString = ((Symbol) a).val;
                    int posLastDot = aString.LastIndexOf(".");
                    Expression[] rands = new Expression[2];
                    rands[0] = Expression.Parse(Symbol.Create(aString.Substring(0, posLastDot)));
                    rands[1] = new Lit(Symbol.Create(aString.Substring(posLastDot + 1, aString.Length - posLastDot - 1)));
		    return new App(Expression.Parse(Symbol.Create("get-property")), rands);
                }
            }
            if (a is Pair) 
            {
                Pair pair = a as Pair;
                object car = pair.car;

		// Fix to speed up non-symbol ToString conversions:
		string carString = null;
		if (car is Symbol) {
		    carString = car.ToString();
		}

                switch (carString)
                {
		case "begin":
		    Begin beginExps = null;
		    int count = ((pair.cdr == null) ? 0 : pair.cdr.Count);
		    Expression[] exps = new Expression[count];
		    pos = 0;
		    if (count > 0) {
			foreach (object obj in pair.cdr) {
			    exps[pos] = Parse(obj);
			    pos++;
			}
		    }
		    beginExps = new Begin(exps);
		    return beginExps;
		case "import-prim":
		    return new Builtin("import-prim", Parse(pair.cdr.car));
		case "dir":
		    return new Builtin("dir");
		case "globals":
		    return new Builtin("globals");
		case "locals":
		    return new Builtin("locals");
		    /*
		case "or":
		    return new Builtin("or", Parse(pair.cdr));
		case "and":
		    return new Builtin("and", Parse(pair.cdr));
		    */
		case "if": 
		    Pair curr = pair.cdr;
		    Expression test_exp = Parse(curr.car);
		    curr = curr.cdr;
		    Expression true_exp = Parse(curr.car);
		    curr = curr.cdr;
		    Expression false_exp = Parse(curr.car);
		    return new If(test_exp, true_exp, false_exp);
		case "quote":
		    // FIXME: dotted pair lists need to be parsed someplace
		    return new Lit(pair.cdr.car);
		case "set!": 
		    Symbol var = pair.cdr.car as Symbol;
		    if (var == null)
			throw new Exception("set! error -> variable must be a symbol: " + Util.Dump(pair));
		    
		    Expression exp = Parse(pair.cdr.cdr.car) as Expression;
		    if (var.val.IndexOf('.') == -1)
			{
			    Assignment assignment = new Assignment(var, exp);
			    return assignment;
			}
		    else
			{
			    string aString = var.val;
			    int posLastDot = aString.LastIndexOf(".");
			    Expression[] rands = new Expression[3];
			    rands[0] = Expression.Parse(Symbol.Create(aString.Substring(0, posLastDot)));
			    rands[1] = new Lit(Symbol.Create(aString.Substring(posLastDot + 1, aString.Length - posLastDot - 1)));
			    rands[2] = exp;
			    
			    App app = new App(Expression.Parse(Symbol.Create("set-property")), rands);
			    return app;
			}
		case "lambda":
		    // Debug.WriteLine("sparsing lambda " + pair.ToString());
		    //Console.WriteLine("parsing lambda...");
		    curr = pair.cdr  as Pair;
		    Symbol[] ids = null;
		    bool all_in_one = false;
		    if (curr.car != null)
                        {
                            if (curr.car is Pair)
				{
				    Object[] ids_as_obj = (curr.car as Pair).ToArray();
				    ids = new Symbol[ids_as_obj.Length];
				    for (int i=0; i<ids_as_obj.Length; i++)
					{
					    ids[i] = ids_as_obj[i] as Symbol;
					    if (ids[i] == null)
						throw new Exception("lambda error -> params must be symbols: " + Util.Dump(pair));
					}
				} 
                            else 
				{
				    all_in_one = true;
				    ids = new Symbol[1];
				    ids[0] = curr.car as Symbol;
				    if (ids[0] == null)
					throw new Exception("lambda error -> params must be symbols: " + Util.Dump(pair));
				}
                        } 
		    curr = curr.cdr  as Pair;
		    // insert implied begin if neccessary
		    Expression body;
		    if (curr.cdr == null)
                        {
                            body = Parse(curr.car);
                        }
		    else
                        {
                            Expression[] begin = new Expression[curr.Count];
                            pos = 0;
			    if (curr != null) {
				foreach (object obj in curr) {
				    begin[pos] = Parse(obj);
				    pos++;
				}
			    }
                            body = new Begin(begin);
                        }
		    //Console.WriteLine("returning proc");
		    return new Proc(ids, body, all_in_one);
		default:  // app
		    if (pair.hasMember)
                        {
                            Expression[] rands = new Expression[2];
                            if (pair.member.IndexOf('.') != -1)
				{
				    string currentMember = pair.member;
				    int posLastDot = currentMember.LastIndexOf(".");
				    
				    pair.member = currentMember.Substring(0, posLastDot);
				    rands[0] = Expression.Parse(pair);
				    pair.member = currentMember;
				    
				    rands[1] = new Lit(Symbol.Create(currentMember.Substring(posLastDot + 1, currentMember.Length - posLastDot - 1)));
				}
                            else
				{
				    pair.hasMember = false;
				    rands[0] = Expression.Parse(pair);
				    pair.hasMember = true;
				    
				    rands[1] = new Lit(Symbol.Create(pair.member));
				}
                            return new App(Expression.Parse(Symbol.Create("get-property")), rands);
                        }
		    else
                        {
                            Expression[] rands = null;
                            if (pair.cdr != null)
				{
				    rands = new Expression[pair.cdr.Count];
				    pos = 0;
				    foreach (object obj in pair.cdr)
					{
					    rands[pos] = Expression.Parse(obj);
					    pos++;
					}
				}
			    IPrim prim = Primitives.getPrim(pair.car.ToString());
                            if (prim != null)
				{
				    Primapp primapp = new Primapp(prim, rands);
				    return primapp;
				}
                            else
				{
				    App app = new App(Expression.Parse(pair.car), rands);
				    return app;
				}
                        }
                }
            } 
            else 
                return new Lit(a);
        }
    }

    public class Lit : Expression 
    {
        public object datum;
        public Lit(object datum) { this.datum = datum; }
        public override object Eval(Env globalEnv, Env localEnv)
        {
            // Debug.WriteLine("Eval Lit: " + datum);
            return datum;
        }

        public override String ToString() { return "<Lit: " + datum + ">"; }
    }

    public class Var : Expression 
    {
        public Symbol id;
        public Var(Symbol id) { this.id = id; }
        public override object Eval(Env globalEnv, Env localEnv)
        {
	    //Console.WriteLine("Eval->Var: " + id + localEnv.ToString());
	    if (localEnv.Contains(id))
		return localEnv.Apply(id);
	    else if (globalEnv.Contains(id))
		return globalEnv.Apply(id);
	    else
		throw new Exception(String.Format("unknown variable '{0}'", 
						  id));
        }

        override public System.String ToString() { return "<var: " + id + "> "; } 
    }

    public class Proc : Expression 
    {
        public Symbol[] ids; // if null, gets all args as a list
        public Expression body;
        public bool all_in_one;
        public Proc(Symbol[] ids, Expression body, bool all_in_one) { this.ids = ids; this.body = body; this.all_in_one = all_in_one; }
        override public System.String ToString() { return "<proc: ids=[" + Util.arrayToString(ids) + "]  body=" + body + "> "; } 
        public override object Eval(Env globalEnv, Env localEnv)
        {
            //DebugInfo.EvalExpression(this);
            // Debug.WriteLine("Eval->Proc");
            return new Closure(ids, body, all_in_one, localEnv);
        }

    }

    public class Primapp : Expression 
    {
        public IPrim prim;
        public Expression[] rands;
	public String rator;

        public Primapp(IPrim prim, Expression[] rands) { 
	    this.prim = prim; this.rands = rands; 
	}
        public Primapp(IPrim prim, String rator, Expression[] rands) { 
	    this.rator = rator;
	    this.rands = rands;
	}
        override public System.String ToString() { return "<primapp: prim=" + prim + " rands=[" + Util.arrayToString(rands) + "]> "; } 
        public override object Eval(Env globalEnv, Env localEnv)
        {
	    Object[] eval_Rands = null;
	    try {
		 eval_Rands = Eval_Rands(rands, globalEnv, localEnv);
	    } catch {
		throw new Exception(String.Format("bad rands ({0} {1})",
						  prim.GetRator(),
						  Util.arrayToString(rands)));
	    }
	    try {
		return prim.Call(eval_Rands);
	    } catch {
		throw new Exception(String.Format("bad apply ({0} {1})",
						  prim.GetRator(),
						  Util.arrayToString(rands)));
	    }
        }
    }

    public class Builtin : Expression {
	string key;
	Expression arg;
        public Builtin(string key) { this.key = key; }
        public Builtin(string key, Expression arg) { 
	    this.key = key; 
	    this.arg = arg;
	}
        public override string ToString() {    
	    return String.Format("<Builtin '{0}'>", key);    
	}
        public override object Eval(Env globalEnv, Env localEnv)
        {
	    //if (key == "or") {
	    //while ()
	    //object retval = arg.Eval(globalEnv, localEnv);
	    if (key == "globals")
		return Util.arrayToList(globalEnv.Keys());
	    else if (key == "locals")
		return Util.arrayToList(localEnv.Keys());
	    else if (key == "dir")
		return new Pair(Util.arrayToList(localEnv.Keys()), Util.arrayToList(globalEnv.Keys()));
	    else if (key == "import-prim") {
		String assname = (String) arg.Eval(globalEnv, localEnv);
		// FIXME: filenames only here
		Assembly assembly = Assembly.LoadFrom(assname);
		Type[] typeArray = assembly.GetTypes();
		Object [] names = new Object[typeArray.Length];
		int i = 0;
		if (typeArray != null) {
		    foreach (Type type in typeArray) {
			int pos = type.FullName.IndexOf("+");
			if (pos != -1) {
			    string name = type.FullName.Substring(pos + 1, 
								  type.FullName.Length - pos - 1);
			    names[i++] = name;
			    Symbol def = Symbol.Create(name);
			    Pair body = new Pair(Pair.Cons(Symbol.Create("new-prim"),
							   Pair.Cons(type.FullName,
								     Pair.Cons(Symbol.Create("_using"),
									       new Pair(Symbol.Create("args"))))));
			    Expression expr = Expression.Parse(Pair.Cons(Symbol.Create("lambda"), 
									 Pair.Cons(Symbol.Create("args"), body)));
			    globalEnv.Bind(def, (object) expr.Eval(globalEnv, localEnv));                    
			}
		    }
		}
		return names;
	    } else {
		throw new Exception(String.Format("unknown eval for builtin '{0}'", key));
	    }
        }
    }

    public class Begin : Expression 
    {
        public Expression[] expressions;
        public Begin(Expression[] expressions) { 
	    this.expressions = expressions; 
	}
        public override string ToString()    {    return "<begin: exps=[" + Util.arrayToString(expressions) + "]> ";    }
        public override object Eval(Env globalEnv, Env localEnv)
        {
            Expression[] exps = expressions;
            // Debug.WriteLine("Eval->Begin");
	    if (exps != null && exps.Length > 0) {
		for (int i=0; i<exps.Length-1; i++)
		    exps[i].Eval(globalEnv, localEnv);
		return exps[exps.Length-1].Eval(globalEnv, localEnv);
	    } else {
		return (Expression) null;
	    }
        }
    }

    public class If : Expression 
    {
        public Expression test_exp, true_exp, false_exp;
        public If(Expression test_exp, Expression true_exp, Expression false_exp) 
        {
            this.test_exp = test_exp; this.true_exp = true_exp; this.false_exp = false_exp;
        }
        override public System.String ToString() { return "<if: " + test_exp + true_exp + false_exp + "> "; } 
        public override object Eval(Env globalEnv, Env localEnv)
        {
	    object testVal = false;
	    if (test_exp != null)
		testVal = test_exp.Eval(globalEnv, localEnv);
            if ((testVal is bool) && (((bool) testVal) == false)) { // return false only if a bool false
		if (false_exp != null)
		    return false_exp.Eval(globalEnv, localEnv);
	    } else {
		if (true_exp != null)
		    return true_exp.Eval(globalEnv, localEnv);
	    }
	    return null;
        }
    }

    public class Assignment : Expression
    {
        public Symbol id; 
        public Expression val;
        public Assignment(Symbol id, Expression val) { this.id = id; this.val = val; }
        public override string ToString()
        {
            return "<assignment id=" + id + " val=" + val + ">";
        }

        public override object Eval(Env globalEnv, Env localEnv)
        {
            // Debug.WriteLine("Eval->Assign: " + id);
            object valEval = val.Eval(globalEnv, localEnv);
            //DebugInfo.EvalExpression(this);
	    // FIXME: assign to whichone has it
	    if (localEnv.Contains(id))
		return localEnv.Bind(id, valEval);
	    else if (globalEnv.Contains(id))
		return globalEnv.Bind(id, valEval);
	    else
		throw new Exception(String.Format("can't set! '{0}'; needs to exist", id));
	}
    }

    public class App : Expression
    {
        public Expression rator; 
        public Expression[] rands;
        public App(Expression rator, Expression[] rands) { this.rator = rator; this.rands = rands; }
        override public System.String ToString() { 
	    return "<app: rator=" + rator + ", rands=[" + Util.arrayToString(rands) + "]> "; 
	} 
        public override object Eval(Env globalEnv, Env localEnv)
        {
            Object proc = null;
	    proc = rator.Eval(globalEnv, localEnv);
            if (proc is Closure)
	    {
		Object[] args = Eval_Rands(rands, globalEnv, localEnv);
		object result = (proc as Closure).Eval(globalEnv, localEnv, args);
		return result;
	    } else {
		throw new Exception(String.Format("invalid operator '{0}'", proc.ToString()));
	    }
        }
    }
}
