
using System;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

public class LoadDLL {

  public static void Main(string [] args) {
	// Filename
	//PrintParts(args[0]);
	
	Assembly assembly = Assembly.LoadFrom(args[0]);
	foreach (Type type in assembly.GetTypes()) {
	  Console.WriteLine("Type: '{0}'", type);
	  string className = type.FullName;

	  // Don't list those methods that come from other modules.
	  // if mi.module != "Graphics.dll" don't list

	  Console.WriteLine("  Methods:");
	  foreach (MethodInfo mi in type.GetMethods()) { 
		if (!mi.IsVirtual) {
		  Console.WriteLine("    Name: {0}.{1}", className, mi.Name);
		  Console.WriteLine("      IsHideBySig      : {0}", mi.IsHideBySig);
		  Console.WriteLine("      IsPrivate        : {0}", mi.IsPrivate);
		  Console.WriteLine("      IsPublic         : {0}", mi.IsPublic);
		  Console.WriteLine("      IsSpecialName    : {0}", mi.IsSpecialName);
		  Console.WriteLine("      IsStatic         : {0}", mi.IsStatic);
		  Console.WriteLine("      IsVirtual        : {0}", mi.IsVirtual);
		  Console.WriteLine("      MemberType       : {0}", mi.MemberType);
		  Console.WriteLine("      MetadataToken    : {0}", mi.MetadataToken);
		  Console.WriteLine("      MethodHandle     : {0}", mi.MethodHandle);
		  Console.WriteLine("      Module           : {0}", mi.Module);
		  Console.WriteLine("      ReflectedType    : {0}", mi.ReflectedType);
		  Console.WriteLine("      ReturnParameter  : {0}", mi.ReturnParameter);
		  Console.WriteLine("      ReturnType       : {0}", mi.ReturnType);
		  Console.WriteLine("      ReturnTypeCustomAttributes: {0}", mi.ReturnTypeCustomAttributes);
		}
	  }

	  Console.WriteLine("  Fields:");
	  foreach (FieldInfo fi in type.GetFields()) {
		Console.WriteLine("    Name: {0}.{1}", className, fi.Name);
		Console.WriteLine("      Attributes      : '{0}'", fi.Attributes);
		Console.WriteLine("      Declaring Type  : '{0}'", fi.DeclaringType);
		Console.WriteLine("      DeclaringType   : '{0}'", fi.DeclaringType);
		Console.WriteLine("      FieldHandle     : '{0}'", fi.FieldHandle);
		Console.WriteLine("      FieldType       : '{0}'", fi.FieldType);
		Console.WriteLine("      IsAssembly      : '{0}'", fi.IsAssembly);
		Console.WriteLine("      IsFamily        : '{0}'", fi.IsFamily);
		Console.WriteLine("      IsFamilyAndAssembly: '{0}'", fi.IsFamilyAndAssembly);
		Console.WriteLine("      IsInitOnly      : '{0}'", fi.IsInitOnly);
		Console.WriteLine("      IsLiteral       : '{0}'", fi.IsLiteral);
		Console.WriteLine("      IsNotSerialized : '{0}'", fi.IsNotSerialized);
		Console.WriteLine("      IsPinvokeImpl   : '{0}'", fi.IsPinvokeImpl);
		Console.WriteLine("      IsPrivate       : '{0}'", fi.IsPrivate);
		Console.WriteLine("      IsPublic        : '{0}'", fi.IsPublic);
		Console.WriteLine("      IsSpecialName   : '{0}'", fi.IsSpecialName);
		Console.WriteLine("      IsStatic        : '{0}'", fi.IsStatic);
		Console.WriteLine("      MemberType      : '{0}'", fi.MemberType);
		Console.WriteLine("      MetadataToken   : '{0}'", fi.MetadataToken);
		Console.WriteLine("      Module          : '{0}'", fi.Module);
		Console.WriteLine("      ReflectedType   : '{0}'", fi.ReflectedType);
	  }
	}
	
  }

  public static Object[] Using(String filename) {
	// from Tacky/Util.cs
	// FIXME: filenames only here
	Assembly assembly = Assembly.LoadFrom(filename);
	Type[] typeArray = assembly.GetTypes();
	Object [] names = new Object[typeArray.Length];
	int i = 0;
	if (typeArray != null) {
	  foreach (Type type in typeArray) {
		Console.WriteLine("... FullName: '{0}'", type.FullName);
		int pos = type.FullName.IndexOf("+");
		if (pos != -1) {
		  string name = type.FullName.Substring(pos + 1, 
			  type.FullName.Length - pos - 1);
		  if (name != "")
			names[i++] = name;
// 		  Symbol def = Symbol.Create(name);
// 		  Pair body = new Pair(Pair.Cons(Symbol.Create("new-prim"),
// 				  Pair.Cons(type.FullName,
// 					  Pair.Cons(Symbol.Create("_using"),
// 						  new Pair(Symbol.Create("args"))))));
// 		  Expression expr = Expression.Parse(Pair.Cons(Symbol.Create("lambda"), 
// 				  Pair.Cons(Symbol.Create("args"), body)));
// 		  globalEnv.Bind(def, (object) expr.Eval(globalEnv, localEnv));                    
		}
	  }
	}
	return names;
  }
}

  /*
  static public Type[] GetTypes(object[] objs)
      {
		int i = 0;
		Type[] retval = new Type[objs.Length];
		foreach (Object obj in objs)
		{
		  if (obj == null)
			retval[i] =Type.GetType("System.Object");
		  else
			retval[i] = obj.GetType();
		  //Console.WriteLine("gtypes [" + retval[i] + "]");
		  i++;
		}
		return retval;
	  }
	  
	  // could move to scheme, but likely just take out of 
	  // prims & make part of Expressions.App
	  public static bool Defined(Object[] args)
	  {
	    // 'Class.Method (args)
	    String [] parts = args[0].ToString().Split('.');
	    String className = "";
	    String methodName = "";
		//Console.WriteLine(parts);	    
	    if (parts.Length > 1) {
		  for (int i = 0; i < (parts.Length - 1); i++) {
		    if (className != "") 
			  className += ".";
		    className += parts[i];
		  }
		  methodName = parts[parts.Length - 1];
		  
		  Type[] types = new Type[0];
		  if (args[1] != null) 
		  {
			types = GetTypes((args[1] as Pair).ToArray());
		  }
		  Type type = Util.GetType(className);
		  MethodInfo method = null;
		  try {
		    method = type.GetMethod(methodName, types);
		  } catch {
		    // not defined
		  }
		  return (method != null);
	    } else {
		  return false;
	    }
	  }
	  
	  // could move to scheme, but likely just take out of 
	  // prims & make part of Expressions.App
	  public static object Call_Method(Object[] args, bool static_call)
	  {
		// Console.WriteLine("call: " + Util.arrayToString(args));
		// Assembly a = System.Reflection.Assembly.Load("System");
		Object[] objs = null;
		Type[] types = new Type[0];
		if (args[2] != null) 
		  // see def of call & call-static in init.ss 
		  // method args passed in as rest, if none then it's ()
		{
		  objs = (args[2] as Pair).ToArray();
		  types = GetTypes(objs);
		}
		Type type;
		if (static_call == true) // args.car is Symbol 
		  type = Util.GetType(args[0].ToString());
		else if (args[0] == null)
		  type = Type.GetType("System.Object");
		else
		  type = args[0].GetType();
		
		object retval = null;
	    MethodInfo method = null;
		try 
		{
		  method = type.GetMethod(args[1].ToString(), types);
	    } catch {
		  throw new Exception("call: method sig not found " + args[1]);
	    }
	    if (method != null)
		  retval = method.Invoke(args[0], objs);
	    else { 
		  throw new Exception("call: method sig not found " + args[1]);
	    }
		return retval;
	  }
	  
	  public static Type GetType(String tname, Pair prefixes)
	  {
		Type type = GetType(tname);
		if (type != null)
		  return type;
		foreach (string prefix in prefixes) 
		{
		  type = GetType(prefix + "." + tname);
		  if (type != null)
			return type;
		}
		return null;
	  }
	  
	  public static Type GetType(String tname)
	  {
		Assembly[] assemblies = System.AppDomain.CurrentDomain.GetAssemblies();
		
		foreach (Assembly assembly in assemblies) 
		{
		  Type type = assembly.GetType(tname);
		  // type = System.Reflection.Assembly.Load(aname).GetType(tname);
		  if (type != null)
			return type;
		}
		return null;
	  }
  */
