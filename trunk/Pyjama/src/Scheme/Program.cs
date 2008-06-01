using System;
using System.IO;
using System.Collections;
using System.Reflection;

namespace Scheme {

    public class Program {
	
        public Env initEnv;
        public Hashtable macros = new Hashtable();

        public Program() 
        {
            this.initEnv = new Extended_Env(new Symbol[] { Symbol.Create("_macros") }, 
					      new Object[] { macros }, 
					      Env.The_Empty_Env);

        } 
	// new Pair(new Symbol("x")), new Pair(3), Env.The_Empty_Env);}
        // this.initEnv = Env.The_Empty_Env; // new Pair(new Symbol("x")), new Pair(3), Env.The_Empty_Env);}
            
        public object LoadEmbededInitScheme()
        {
            //Assembly executingAssembly = Assembly.GetExecutingAssembly();
            //Stream initSchemeStream = executingAssembly.GetManifestResourceStream("Scheme.InitScheme.ss");
            //StreamReader initSchemeStreamReader = new StreamReader(initSchemeStream); 
	    // FIXME: get from current directory?
            StreamReader streamReader = new StreamReader("init.ss");
            return Eval(streamReader);
        }

        public object Eval(Expression exp) {    
	    return exp.Eval(initEnv, new Empty_Env()); 
	}
        public object Eval(string str)
        {
            return Eval(new StringReader(str));
        }

        public object Eval(string str, Env env)
        {
            Env curEnv = initEnv;
            initEnv = env;

            object result = Eval(str);

            initEnv = curEnv;
            return result;
        }

        public object Eval(TextReader str, Env env)
        {
            Env curEnv = initEnv;
            initEnv = env;

            object result = Eval(str);

            initEnv = curEnv;
            return result;
        }

        public object Eval(TextReader str)
        {
            Object evaledObj = null;
            Object parsedObj = Util.read(str);
            if (parsedObj is Pair)
            {
                if (macros.Count > 0)
                {
                    Closure macroExpand = initEnv.Apply(Symbol.Create("macro-expand")) as Closure;
		    try {
			parsedObj = macroExpand.Eval(initEnv, new Empty_Env(), new Object[] { parsedObj });
		    } catch {
			// FIXME; macro's can fail?
		    }
                } 
                
                Pair p = parsedObj as Pair;
                switch (p.car.ToString()) 
                {
		case "load":
		    String filename = (String) Expression.Parse(p.cdr.car).Eval(initEnv, new Empty_Env());
		    Console.WriteLine("Loading '{0}'...", filename);
		    String text = File.OpenText(filename).ReadToEnd();
		    evaledObj = Eval(new StringReader(text));
		    break;
		case "define":
		    Expression expr = null;
		    Symbol def = null;
		    if (p.cdr.car is Pair) // (define (fn x y) body)
                        {
                            Pair def_and_args = p.cdr.car as Pair;
                            Pair body = p.cdr.cdr;
                            Pair syms = def_and_args.cdr;
                            def = Symbol.Create(def_and_args.car.ToString());
                            expr = Expression.Parse(Pair.Cons(Symbol.Create("lambda"), Pair.Cons(syms,body)));
                            // Debug.WriteLine(def + "-->" + expr);
                        }
		    else // (define fn (lambda (x y) body))
                        {
                            def = Symbol.Create(p.cdr.car.ToString());
                            expr = Expression.Parse(p.cdr.cdr.car);
                        }
		    //initEnv = new Extended_Env(new Pair(def), new Pair(null), initEnv);
		    //Object value = Eval(expr);
		    //Console.WriteLine("defining: " + def);
		    initEnv.Bind(def, Eval(expr));                    
		    evaledObj = Util.Dump(null);
		    break;
		case "parse":
		    evaledObj = Util.Dump(Expression.Parse(p.cdr.car));
		    break;
		case "define-syntax":
		    Symbol name = p.cdr.car as Symbol;
		    // Console.WriteLine("macrodef" + name + " " + Expression.Parse(p.cdr.cdr.car).Eval(initEnv).ToString());
		    Closure transformer = (Closure) Expression.Parse(p.cdr.cdr.car).Eval(initEnv, new Empty_Env());
		    this.macros[name] = transformer;
		    // Console.WriteLine("macro: " + name);
		    break;
		default:
		    // need to call macro-expand in here. but how?
		    evaledObj = Util.Dump(Eval(Expression.Parse(parsedObj)));
		    break;
                }
            } 
            else 
                evaledObj = Util.Dump(Eval(Expression.Parse(parsedObj)));

            if (str.Peek() == -1)
                return evaledObj;
            else
                return Eval(str);
        }

	public static void Main(string[] args) {
	    int start = Environment.TickCount;
	    Program prog = null;
	    prog = new Program();
	    // FIXME; don't load if flag
	    prog.LoadEmbededInitScheme();
	    
	    for (int i=0; i < args.Length; i++) {
		try {
		    String init = File.OpenText(args[0]).ReadToEnd();
		    prog.Eval(new StringReader(init));
		    //StreamReader streamReader = new StreamReader(args[i]);
		    //prog.Eval(streamReader);
		} catch {
		    Console.WriteLine(String.Format("Scheme: cannot load '{0}'", args[i]));
		}
	    }
	    
	    int end = Environment.TickCount;
	    while (true) {
		StreamWriter str = null;
		//str = new StreamWriter("transcript.ss", true);
		try {
		    Console.WriteLine("(" + (end - start) + " ms)");
		    Console.Write("> ");
		    
		    String val = Console.ReadLine();
		    
		    if (str != null)
			str.WriteLine(val);
		    
		    start = Environment.TickCount;
		    object result = prog.Eval(new StringReader(val));
		    end = Environment.TickCount;
		    
		    Console.WriteLine(result);
		} catch (Exception e) {
		    Console.WriteLine("Scheme Error: " + e.Message); // + e.Message); // .Message);
		    Console.WriteLine("Stacktrace: " + e.StackTrace);
		}
		if (str != null)
		    str.Close();
	    }
	}
    }
}
