using System;
using System.Collections;
//using TopSupport.Core.Common;

namespace Scheme 
{
    public abstract class Env 
    {
        static public Empty_Env The_Empty_Env = new Empty_Env();
        public Extended_Env Extend(Symbol[] syms, Object[] vals)
        {
            return new Extended_Env(syms, vals, this);
        }
        abstract public object Apply(Symbol id);
        abstract public bool Contains(Symbol id);
        abstract public object Bind(Symbol id, Object val);
	abstract public Pair ToExpression();
        //    abstract public object Bind(Symbol[] ids, Object[] vals);
    }

    public class Empty_Env : Env 
    {
        override public object Apply(Symbol id)
        {
            throw new Exception(String.Format("Unbound variable: '{0}' ", id));
        }
        override public bool Contains(Symbol id)
        {
            return false;
        }
        override public System.String ToString()
        {
            return "()";
        }
        override public Pair ToExpression()
        {
            return new Pair(new Pair(null));
        }
        override public object Bind(Symbol id, Object val)
        {
            throw new Exception(String.Format("Unbound variable: '{0}' ", id));
        }

        /*    override public object Bind(Symbol[] ids, Object[] vals)
            {
                throw new Exception("Unbound variable " + ids);
            }
        */
    }

    public class Extended_Env : Env
    {
        internal Hashtable bindings = new Hashtable(); //Hashlist
        internal Env env = null;

	override public Pair ToExpression() {
	    Pair retval = new Pair(new Pair(null));
	    Pair current = (Pair) retval.car;
	    // FIXME: alpha sort keys
	    foreach (object key in bindings.Keys) {
		if (current.car == null) {
		    current.car = key; 
		} else {
		    current.cdr = new Pair(key);
		    current = current.cdr;
		}
	    }
	    retval.cdr = env.ToExpression();
	    return retval;
	}

        override public System.String ToString()
        {
	    string retval = "";
	    // FIXME: alpha sort keys
	    foreach (object key in bindings.Keys) {
		if (retval != "") {
		    retval += " ";
		}
		retval += "'" + key.ToString(); 
	    }
            return "(" + retval + " " + env.ToString() + ")";
        }
        public Extended_Env(Symbol[] inSyms, Object[] inVals, Env inEnv)
        {
            this.env = inEnv;
        
            for (int pos=0; pos < inSyms.Length; pos++)
            {
                Symbol currSym = (Symbol) inSyms[pos];
            
                if (!currSym.ToString().Equals("."))
                    this.bindings[currSym] = inVals[pos];
                else 
                {
                    // multiple values passed in (R5RS 4.1.4)
                    currSym = (Symbol) inSyms[pos+1];
                    this.bindings[currSym] = Pair.FromArrayAt(inVals,pos);
                    break;
                }
            }
        }

        // this and above should be merged somehow (may be obvious actually)
        /*    public override object Bind(Symbol[] inSyms, Object[] inVals)
            {
                for (int pos=0; pos < inSyms.Length; pos++)
                {
                    Symbol currSym = (Symbol) inSyms[pos];
            
                    if (!currSym.ToString().Equals("."))
                        this.bindings[currSym] = inVals[pos];
                    else 
                    {
                        // multiple values passed in (R5RS 4.1.4)
                        currSym = (Symbol) inSyms[pos+1];
                        this.bindings[currSym] = Pair.FromArrayAt(inVals,pos);
                        break;
                    }
                }
                return null;
            }
        */
        override public object Bind(Symbol id, Object val)
        {
            if (this.bindings.ContainsKey(id) || env == The_Empty_Env)
            {
                bindings[id] = val;
                return id;
            } 
            else
            {
                return env.Bind(id,val);
            }
        }

	override public bool Contains(Symbol id) {
	    if (this.bindings.ContainsKey(id))
		return true;
	    else 
		return env.Contains(id);
	}
	    
        override public object Apply(Symbol id)
        {
	    if (this.bindings.ContainsKey(id))
		return bindings[id];
	    else 
		return env.Apply(id);
        }
    }
}