
using System;
using Microsoft.VisualBasic.CompilerServices;

namespace Testing {
  
  /*
  public static class Extensions {
    public static int ToInt32(this Object o) {
      return 3;
    }
  }
  */
  
  public class Fraction : IConvertible {
    int N;
    int D;
    
    public Fraction(int n, int d) {
      N = n;
      D = d;
    }

    public int CompareTo(Fraction other) {
      long d = (long)this.N * (long)(other.D) - (long)other.N * (long)(this.D);
      return (d > 0) ? 1 : ((d < 0) ? -1 : 0);
    }

    public int CompareTo(object obj)
    {
      if (obj == null)
	return 1;
      return this.CompareTo((Fraction)obj);
    }
    
    public override string ToString() {
      return String.Format("{0}/{1}", N, D);
    }

    TypeCode IConvertible.GetTypeCode()
    {
      return TypeCode.Object;
    }
    
    bool IConvertible.ToBoolean(IFormatProvider provider)
    {
      throw new InvalidCastException();
    }
    
    byte IConvertible.ToByte(IFormatProvider provider)
    {
      return (byte)(this.N/this.D);
    }
    
    char IConvertible.ToChar(IFormatProvider provider)
    {
      throw new InvalidCastException();
    }
    
    DateTime IConvertible.ToDateTime(IFormatProvider provider)
    {
      throw new InvalidCastException();
    }
    
    decimal IConvertible.ToDecimal(IFormatProvider provider)
    {
      return (decimal)this.N / (decimal)this.D;
    }
    
    double IConvertible.ToDouble(IFormatProvider provider)
    {
      return (double)this.N / (double)this.D;
    }
    
    short IConvertible.ToInt16(IFormatProvider provider)
    {
      return (short)(this.N / this.D);
    }
    
    int IConvertible.ToInt32(IFormatProvider provider)
    {
      return this.N / this.D;
    }
    
    long IConvertible.ToInt64(IFormatProvider provider)
    {
      return this.N / this.D;
    }
    
    sbyte IConvertible.ToSByte(IFormatProvider provider)
    {
      return (sbyte)(this.N / this.D);
    }
    
    float IConvertible.ToSingle(IFormatProvider provider)
    {
      return (float)this.N / (float)this.D;
    }
    
    string IConvertible.ToString(IFormatProvider provider)
    {
      return this.ToString();
    }
    
    object IConvertible.ToType(Type conversionType, IFormatProvider provider)
    {
      return Convert.ChangeType(this, conversionType, provider);
    }
    
    ushort IConvertible.ToUInt16(IFormatProvider provider)
    {
      return (ushort)(this.N / this.D);
    }
    
    uint IConvertible.ToUInt32(IFormatProvider provider)
    {
      return (uint)(this.N / this.D);
    }
    
    ulong IConvertible.ToUInt64(IFormatProvider provider)
    {
      return (ulong)(this.N / this.D);
    }
    
    public static bool operator >(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) > 0;
    }
    public static bool operator <(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) < 0;
    }
    public static bool operator >=(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) >= 0;
    }
    public static bool operator <=(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) <= 0;
    }
    public static bool operator ==(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) == 0;
    }
    public static bool operator !=(Fraction f1, Fraction f2)
    {
      return f1.CompareTo(f2) != 0;
    }
    
    public override bool Equals(object obj)
    {
      if (obj != null)
	return this.CompareTo((Fraction)obj) == 0;
      return false;
    }
    public override int GetHashCode()
    {
      return this.D ^ (this.N);
    }
  }
  
  public class Test {
    
    
    public static void Main() {
      
      object o1 = 1.1;
      object o2 = 2.3;
      object o3 = new Fraction(5,2);
      
      Console.WriteLine("{0} + {1} = {2}", o1, o3, 
			ObjectType.AddObj(o1, o3));
    }
  }
}
