
using System;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

[assembly: AssemblyVersion ("1.0.*")]
[assembly: AssemblyKeyFile ("pyjama.key")]
public class LoadDLL {


  public static void Main( )
  {

#pragma warning disable 612
	//Assembly assembly = Assembly.LoadWithPartialName("System");
#pragma warning restore 612
 	
	
	//Assembly assembly = Assembly.Load("System, Version=2.1.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089") ; //.BigInteger

	//DumpFile("/home/dblank/fepy/IPCE/IronPython-1.1.1/IronMath.dll");

	//Dump(assembly);

	DumpThis();

	/*
	Type theMathType = assembly.GetType("Cos");
	Object theObj =
		Activator.CreateInstance(theMathType);
	
	// array with one member
	Type[] paramTypes = new Type[1];
	paramTypes[0]= Type.GetType("System.Double");
	
	// Get method info for Cos( )
	MethodInfo CosineInfo =
		theMathType.GetMethod("Cos",paramTypes);
	
	// fill an array with the actual parameters
	Object[] parameters = new Object[1];
	parameters[0] = 45;
	Object returnVal =
		CosineInfo.Invoke(theObj,parameters);
	Console.WriteLine(
		"The cosine of a 45 degree angle {0}",
		returnVal);
	*/
  }

  public static void DumpThis() {
	int indent = 0;
	// Display information about each assembly loading into this AppDomain.
	foreach (Assembly b in AppDomain.CurrentDomain.GetAssemblies())
	{
	  Display(indent, "Assembly: {0}", b);
	  
	  // Display information about each module of this assembly.
	  foreach ( Module m in b.GetModules(true) )
	  {
		Display(indent+1, "Module: {0}", m.Name);
	  }
	  
	  // Display information about each type exported from this assembly.
	  
	  indent += 1;
	  foreach ( Type t in b.GetExportedTypes() )
	  {
		Display(0, "");
		Display(indent, "Type: {0}", t);
		
		// For each type, show its members & their custom attributes.
		
		indent += 1;
		foreach (MemberInfo mi in t.GetMembers() )
		{
		  Display(indent, "Member: {0}", mi.Name);
		  DisplayAttributes(indent, mi);
		  
		  // If the member is a method, display information about its parameters.
		  
		  if (mi.MemberType==MemberTypes.Method)
		  {
			foreach ( ParameterInfo pi in ((MethodInfo) mi).GetParameters() )
			{
			  Display(indent+1, "Parameter: Type={0}, Name={1}", pi.ParameterType, pi.Name);
			}
		  }
		  
		  // If the member is a property, display information about the property's accessor methods.
		  if (mi.MemberType==MemberTypes.Property)
		  {
			foreach ( MethodInfo am in ((PropertyInfo) mi).GetAccessors() )
			{
			  Display(indent+1, "Accessor method: {0}", am);
			}
		  }
		}
		indent -= 1;
	  }
	  indent -= 1;
	}
  }

  public static void DisplayAttributes(Int32 indent, MemberInfo mi)
  {
	// Get the set of custom attributes; if none exist, just return.
	object[] attrs = mi.GetCustomAttributes(false);
	if (attrs.Length==0) {return;}
	
	// Display the custom attributes applied to this member.
	Display(indent+1, "Attributes:");
	foreach ( object o in attrs )
	{
	  Display(indent+2, "{0}", o.ToString());
	}
  }

  public static void Display(Int32 indent, string format, params object[] param) 
  {
	Console.Write(new string(' ', indent*2));
	Console.WriteLine(format, param);
  }
  

  public static void DumpFile(string filename) {
	Assembly assembly = Assembly.LoadFrom(filename);
	Dump(assembly);
  }	

  public static void DumpFrom(string assembly_name) {
	Assembly assembly = Assembly.Load(assembly_name);
	Dump(assembly);
  }	

  public static void Dump(Assembly assembly) {
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


// 	Type theType = Type.GetType(
// 		"System.Reflection.Assembly");
 
// 	MemberInfo[] mbrInfoArray =
// 		theType.FindMembers(MemberTypes.Method,
// 			BindingFlags.Default,
// 			Type.FilterName, "*");
// 	foreach (MemberInfo mbrInfo in mbrInfoArray )
// 	{
//       Console.WriteLine("{0} is a {1}",
// 		  mbrInfo, mbrInfo.MemberType);
// 	}
// 	//object[] objs = {"Graphics.makeGraphWin"};
// 	//Call_Method(objs, true);

  }

  public static Type MyGetType(String tname) {
	Assembly[] assemblies = System.AppDomain.CurrentDomain.GetAssemblies();
	foreach (Assembly assembly in assemblies) {
	  Type type = assembly.GetType(tname);
	  // type = System.Reflection.Assembly.Load(aname).GetType(tname);
	  if (type != null)
		return type;
	}
	return null;
  }

  static public Type[] MyGetTypes(object[] objs)
  {
	int i = 0;
	Type[] retval = new Type[objs.Length];
	foreach (Object obj in objs)
	{
	  if (obj == null)
		retval[i] = Type.GetType("System.Object");
	  else
		retval[i] = obj.GetType();
	  //Console.WriteLine("gtypes [" + retval[i] + "]");
	  i++;
	}
	return retval;
  }

  // could move to scheme, but likely just take out of 
  // prims & make part of Expressions.App
  public static object Call_Method(Object[] args, bool static_call)
  {
	// Console.WriteLine("call: " + Util.arrayToString(args));
	// Assembly a = System.Reflection.Assembly.Load("System");
	Object[] objs = new Object[args.Length - 1];
	Type[] types = new Type[0];
	for (int i = 1; i < args.Length; i ++) { //args[2] != null) {
	  // see def of call & call-static in init.ss 
	  // method args passed in as rest, if none then it's ()
	  objs[i - 1] = args[i];
	  types = MyGetTypes(objs);
	}
	Type type;
	if (static_call == true) // args.car is Symbol 
	  type = MyGetType(args[0].ToString());
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
	  throw new Exception("call: method sig not found ");
	}
	if (method != null)
	  retval = method.Invoke(args[0], objs);
	else { 
	  throw new Exception("call: method invoke failed ");
	}
	return retval;
  }
  
}
