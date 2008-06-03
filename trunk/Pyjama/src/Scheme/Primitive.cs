using System;
using System.Diagnostics;
using System.Reflection;
using System.Collections;
using Microsoft.VisualBasic.CompilerServices;

namespace Scheme
{
    public interface IPrim 
    {
        String GetRator();
        object Call(Object[] args);
    }

    public class BasePrim {
	public string rator = "undefined";
	public BasePrim(string rator) {
	    this.rator = rator;
	}
	public BasePrim() {
	    this.rator = "unspecified";
	}
	public String GetRator() {
	    return rator;
	}
    }
    
    class Primitives
    {
        static Hashtable prims = null;

        static public IPrim getPrim(string name)
        {
            if (prims == null)
                setupPrimitives();
            return (IPrim) prims[name];
        }

        static public void addPrim(string name, IPrim prim)
        {
            if (prims == null)
                throw new Exception("setupPrims must be called first");
            prims[name] = prim;
        }

        static public void setupPrimitives() 
        {
            prims = new Hashtable(50);
            prims["addobj-prim"] = new AddObjPrim("addobj-prim");
            prims["subobj-prim"] = new SubObjPrim("subobj-prim");
            prims["mulobj-prim"] = new MulObjPrim("mulobj-prim");
            prims["divobj-prim"] = new DivObjPrim("divobj-prim");
            prims["strcompare-prim"] = new StrcompPrim("strcompare-prim");
            prims["numcompare-prim"] = new NumcompPrim("numcompare-prim");
            prims["hash-ref-prim"] = new HashrefPrim("hash-ref-prim");
            prims["hash-set!-prim"] = new HashsetPrim("hash-set!-prim");
            prims["cons-proper-prim"] = new ConsProperPrim("cons-proper-prim");
            prims["cons-improper-prim"] = new ConsImproperPrim("cons-improper-prim");
            prims["set-car-prim"] = new SetCarPrim("set-car-prim");
            prims["set-proper-cdr-prim"] = new SetProperCdrPrim("set-proper-cdr-prim");
            prims["set-improper-cdr-prim"] = new SetImproperCdrPrim("set-improper-cdr-prim");
            prims["car-prim"] = new CarPrim("car-prim");
            prims["cdr-prim"] = new CdrPrim("cdr-prim");
            prims["eq?-prim"] = new EqPrim("eq?-prim");
            prims["eqv?-prim"] = new EqvPrim("eqv?-prim");
            prims["is-pair-prim"] = new IsPairPrim("is-pair-prim");

            prims["typeof-prim"] = new TypeofPrim("typeof-prim");

            prims["vector-prim"] = new VectorPrim("vector-prim");
            prims["vector-length-prim"] = new VecLenPrim("vector-length-prim");
            prims["vector-ref-prim"] = new VecRefPrim("vector-ref-prim");
            prims["vector-set!-prim"] = new VecSetPrim("vector-set!-prim");
            prims["string->symbol-prim"] = new StrSymPrim("string->symbol-prim");
            prims["tostring-prim"] = new TostringPrim("tostring-prim");

	    prims["get-type-prim" ] = new GetTypePrim("get-type-prim");
            prims["new-prim"] = new NewPrim("new-prim");
            prims["call-prim"] = new CallPrim("call-prim");
            prims["call-static-prim"] = new CallStaticPrim("call-static-prim");
            prims["get-property-prim"] = new GetPropPrim("get-property-prim");
            prims["set-property-prim"] = new SetPropPrim("set-property-prim");
            prims["get-field-prim"] = new GetFieldPrim("get-field-prim");
            prims["set-field-prim"] = new SetFieldPrim("set-field-prim");
            prims["copy-debug-prim"] = new CopyDebugPrim("copy-debug-prim");
	    prims["defined-prim?"] = new DefinedPrim("defined-prim?");	    
	    prims["fraction-add-prim"] = new FractionAddPrim("fraction-add-prim");	    
	    prims["fraction-sub-prim"] = new FractionSubPrim("fraction-sub-prim");	    
	    prims["fraction-div-prim"] = new FractionDivPrim("fraction-div-prim");	    
	    prims["fraction-mul-prim"] = new FractionMulPrim("fraction-mul-prim");	    
        }

	public class FractionEqvPrim : BasePrim, IPrim {
	    public FractionEqvPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return Fraction.Eqv(args[0], args[1]);
		
            }
	}
	
        public class FractionAddPrim : BasePrim, IPrim    
        {
	    public FractionAddPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		return (object) Fraction.Add(args[0], args[1]);
            }
        }

        public class FractionSubPrim : BasePrim, IPrim    
        {
	    public FractionSubPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		return (object) Fraction.Sub(args[0], args[1]);
            }
        }

        public class FractionDivPrim : BasePrim, IPrim    
        {
	    public FractionDivPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		return (object) Fraction.Div(args[0], args[1]);
            }
        }

        public class FractionMulPrim : BasePrim, IPrim    
        {
	    public FractionMulPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		return (object) Fraction.Mul(args[0], args[1]);
            }
        }

        public class StrSymPrim : BasePrim, IPrim    
        {
	    public StrSymPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return Scheme.Symbol.Create((string) args[0]); 
            }
        }
        
        public class TostringPrim : BasePrim, IPrim    
        { 
	    public TostringPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            { 
		if (args[0] == null)
		    return "()";
		else
		    return args[0].ToString(); 
            }
        }
        
        public class VectorPrim : BasePrim, IPrim    
        {
	    public VectorPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                if (args[1] != null)
                    return System.Array.CreateInstance(args[1] as Type, (int) args[0]);
                else
                    return new Object[(int) args[0]];
            }
        }

        public class VecLenPrim : BasePrim, IPrim    
        {
	    public VecLenPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		if (args[0] == null)
		    return 0;
		else
		    return (args[0] as System.Array).Length; 
            }
        }
        
        public class VecRefPrim : BasePrim, IPrim    
        {
	    public VecRefPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return (args[0] as System.Array).GetValue((int) args[1]); 
            }
        }
        
        public class VecSetPrim : BasePrim, IPrim    
        {
	    public VecSetPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                (args[0] as System.Array).SetValue(args[2], (int) args[1]); return null; 
            } 
        }
        
        public class AddObjPrim : BasePrim, IPrim    
        { 
	    public AddObjPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return ObjectType.AddObj(args[0], args[1]); 
            }
        }
        
        public class SubObjPrim : BasePrim, IPrim
        { 
	    public SubObjPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return ObjectType.SubObj(args[0], args[1]); 
            }
        }
        
        public class MulObjPrim : BasePrim, IPrim
        { 
	    public MulObjPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return ObjectType.MulObj(args[0], args[1]); 
            }
        }
        
        public class DivObjPrim : BasePrim, IPrim
        { 
	    public DivObjPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return ObjectType.DivObj(args[0], args[1]); 
            }
        }
        
        public class StrcompPrim : BasePrim, IPrim
        { 
	    public StrcompPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return String.Compare((string) args[0], (string) args[1], (bool) args[2]);
            }
        }

        public class NumcompPrim : BasePrim, IPrim
        { 
	    public NumcompPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		object o1 = args[0], o2 = args[1];
		if (args[0] is Fraction)
		    o1 = (Single)((Fraction)args[0]).ToSingle();
		if (args[1] is Fraction)
		    o2 = (Single)((Fraction)args[1]).ToSingle();
                return ObjectType.ObjTst(o1, o2, false);
            }
        }
        
        public class HashrefPrim : BasePrim, IPrim    
        { 
	    public HashrefPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return (args[0] as Hashtable)[args[1]]; 
            } 
        }
        
        public class HashsetPrim : BasePrim, IPrim    
        { 
	    public HashsetPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return (args[0] as Hashtable)[args[1]] = args[2]; 
            }
        }
        
        public class ConsProperPrim : BasePrim, IPrim    
        { 
	    public ConsProperPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args) { 
		return Pair.Cons(args[0], args[1] as Pair); 
	    }    
        }

        public class ConsImproperPrim : BasePrim, IPrim    
        { 
	    public ConsImproperPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args) { 
		return Pair.Cons(args[0], args[1] as Object); 
	    }    
        }

        public class SetCarPrim : BasePrim, IPrim
        { 
	    public SetCarPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                (args[0] as Pair).car = args[1];
                return null;
            }
        }
        
        public class SetProperCdrPrim : BasePrim, IPrim
        { 
	    public SetProperCdrPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                (args[0] as Pair).cdr = (Pair) args[1];
		((Pair)args[0]).proper = true;
                return null;
            }
        }

        public class SetImproperCdrPrim : BasePrim, IPrim
        { 
	    public SetImproperCdrPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                (args[0] as Pair).cdr_ = (object) args[1];
		((Pair)args[0]).proper = false;
                return null;
            }
        }

        public class CarPrim : BasePrim, IPrim
        { 
		
	    public CarPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
                return (args[0] as Pair).car; 
            }
        }
        
        public class CdrPrim : BasePrim, IPrim
        { 
	    public CdrPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            { 
		if (((Pair)args[0]).proper)
		    return (args[0] as Pair).cdr; 
		else
		    return (args[0] as Pair).cdr_; 
            } 
        }
        
        public class EqPrim : BasePrim, IPrim     
        { 
	    public EqPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            { 
                if ((args[0] is Boolean) && (args[1] is Boolean))
                    return (bool) args[0] == (bool) args[1];
                else 
                    return System.Object.ReferenceEquals(args[0],args[1]);
            } 
        }

        public class IsPairPrim : BasePrim, IPrim     
        { 
	    public IsPairPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            { 
                if (args[0] is Pair) 
                        return true;
                else 
                        return false;
            } 
        }
        
        public class EqvPrim : BasePrim, IPrim 
        { 
	    public EqvPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		if (args[0] == null) {
		    return (args[1] == null);
		} else {
		    return args[0].Equals(args[1]); 
		}
            }
        }
        public class TypeofPrim : BasePrim, IPrim     
        { 
	    public TypeofPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		if (args[0] == null) {
		    return null;
		} else {
		    return args[0].GetType(); 
		} 
            }
        }

        
        public class GetTypePrim : BasePrim, IPrim 
        {
	    public GetTypePrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args) // sym typename, pair usings
            {
                if (args[1] == null)
                    return Scheme.Util.GetType(args[0].ToString());
                else
                    return Scheme.Util.GetType(args[0].ToString(), args[1] as Pair);
            }
        }

        public class NewPrim : BasePrim, IPrim 
        {
	    public NewPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)    
            {
		//Console.WriteLine("args: {0}", Util.arrayToString(args));
                Type type = Util.GetType(args[0].ToString(), args[1] as Pair);
		if (type != null) {
		    try 
			{
			    if (args[2] == null)
				return Activator.CreateInstance(type);
			    else
				return Activator.CreateInstance(type, (args[2] as Pair).ToArray(), null);
			} 
		    catch (Exception e) 
			{
			    Console.WriteLine("[Newprim failed with type '{0}']", type);
			    throw e;
			}
		} else {
		    if (args[2] == null)
			throw new Exception(String.Format("class '{0}' not found in [{1}]",
							  args[0].ToString(),
							  args[1].ToString()));
		    else
			throw new Exception(String.Format("class '{0}({2})' not found in [{1}]",
							  args[0].ToString(),
							  args[1].ToString(),
							  args[2].ToString()));
		}
	    }
        }

        public class CallPrim : BasePrim, IPrim
        {
	    public CallPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                return Util.Call_Method(args,false);
            }
        }

        public class DefinedPrim : BasePrim, IPrim
        {
	    public DefinedPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                return Util.Defined(args);
            }
        }

        public class  CallStaticPrim : BasePrim, IPrim
        {
	    public CallStaticPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                return Util.Call_Method(args,true);
            }
        }

        public class GetPropPrim : BasePrim, IPrim
        {
	    public  GetPropPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                Object obj = args[0];
                String field = (args[1] as Symbol).ToString();
                System.Object[] rest = null;
                if (args[2] != null)
                    rest = (args[2] as Pair).ToArray();
                if (obj is Symbol)
                    return Util.GetType(obj.ToString()).GetProperty(field).GetValue(null,rest);
                else
                    return obj.GetType().GetProperty(field).GetValue(obj, rest);
            }
        }
        public class  SetPropPrim : BasePrim, IPrim
        {
	    public  SetPropPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                Object obj = args[0];
                String field = args[1].ToString();
                Object val = args[2];
                System.Object[] rest = null;
                if (args[3] != null)
                    rest = (args[3] as Pair).ToArray();
                if (obj is Symbol)
                    Util.GetType(obj.ToString()).GetProperty(field).SetValue(null,val,rest);
                else 
                {
                    PropertyInfo prop = obj.GetType().GetProperty(field);
                    if (prop == null)
                        throw new Exception("Could not find property " + field + " in object of type " + obj.GetType().ToString());
                    prop.SetValue(obj, val, rest);
                }
                return true;
            }
        }
        public class  GetFieldPrim : BasePrim, IPrim
        {
	    public  GetFieldPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                return args[0].GetType().GetField((args[1] as Symbol).ToString()).GetValue(args[0]);
            }
        }
        public class  SetFieldPrim : BasePrim, IPrim
        {
	    public  SetFieldPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                args[0].GetType().GetField((args[1] as Symbol).ToString()).SetValue(args[0],args[2]);
                return true;
            }
        }
        public class CopyDebugPrim : BasePrim, IPrim
        {
	    public CopyDebugPrim(string rator) {
		this.rator = rator;
	    }
            public object Call(Object[] args)
            {
                if ((args[0] is Pair) && (args[1] is Pair))
                {
                    //((Pair) args[1]).marker = ((Pair) args[0]).marker;
                    ((Pair) args[1]).hasMember = ((Pair) args[0]).hasMember;
                    ((Pair) args[1]).member = ((Pair) args[0]).member;
                }
                else
                    throw new Exception("arguments not pairs");

                return args[1];
            }
        }

    }
}