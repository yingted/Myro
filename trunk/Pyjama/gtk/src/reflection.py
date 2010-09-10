import clr
import System

def get_reference(dll_file):
    """
    >>> get_references("Myro.dll")
    [<reference>, <reference>, ...]
    """
    for reference in clr.References:
        if reference.ManifestModule.Name == dll_file:
            return reference
    return None

def get_classes(dll_file):
    """
    >>> get_classes("Myro.dll")
    ['Myro']
    """
    reference = get_reference(dll_file)
    return [c for c in reference.ManifestModule.GetTypes() if not c.Name.startswith("_")]

def get_class(dll_file, _class):
    """
    >>> get_class('Myro.dll', 'Myro')
    <class>
    """
    reference = get_reference(dll_file)
    for c in reference.ManifestModule.GetTypes():
        if not c.Name.startswith("_") and c.Name == _class:
            return c

def get_items(dll_file, _class):
    # How to get all of the items in a class:
    # >>> [x.Name for x in clr.References[2].ManifestModule.GetTypes()[0].GetMembers()]
    # ['init', 'forward', 'backward', 'Equals', 'GetHashCode', 'GetType', 'ToString', 'robot', '_Scribbler', '_Robot']
    # MemberType -> System.Reflection.MemberTypes.Method
    return [i.Name for i in [c for c in get_reference(dll_file).ManifestModule.GetTypes() if c.Name == _class] 
            if (i.Name not in ["GetType"]) and 
               ((int(System.Reflection.MethodAttributes.VtableLayoutMask) & int(i.Attributes)) == 0) and 
               not i.Name.startswith("_")]

def get_methods(dll_file, _class):
    # How to get all of the items in a class:
    # >>> [x.Name for x in clr.References[2].ManifestModule.GetTypes()[0].GetMembers()]
    # ['init', 'forward', 'backward', 'Equals', 'GetHashCode', 'GetType', 'ToString', 'robot', '_Scribbler', '_Robot']
    # MemberType -> System.Reflection.MemberTypes.Method
    return [i.Name for i in [c for c in get_reference(dll_file).ManifestModule.GetTypes() if c.Name == _class] 
            if (i.Name not in ["GetType"]) and i.MemberType is System.Reflection.MemberTypes.Method and
               (int(System.Reflection.MethodAttributes.VtableLayoutMask) & int(i.Attributes) == 0) and 
               not i.Name.startswith("_")]

# [('init', <enum System.Reflection.MethodAttributes: Public, Static, HideBySig>, Method), 
#  ('forward', <enum System.Reflection.MethodAttributes: Public, Static, HideBySig>, Method), 
#  ('backward', <enum System.Reflection.MethodAttributes: Public, Static, HideBySig>, Method), 
#  ('Equals', <enum System.Reflection.MethodAttributes: Public, Virtual, HideBySig, VtableLayoutMask>, Method),
#  ('GetHashCode', <enum System.Reflection.MethodAttributes: Public, Virtual, HideBySig, VtableLayoutMask>, Method),
#  ('GetType', <enum System.Reflection.MethodAttributes: Public, HideBySig>, Method), 
#  ('ToString', <enum System.Reflection.MethodAttributes: Public, Virtual, HideBySig, VtableLayoutMask>, Method),
#  ('robot', <enum System.Reflection.FieldAttributes: Public, Static>, Field), 
#  ('_Scribbler', <enum System.Reflection.TypeAttributes: NestedPublic, Serializable, BeforeFieldInit>, NestedType),
#  ('_Robot', <enum System.Reflection.TypeAttributes: NestedPublic, BeforeFieldInit>, NestedType)]

        # How many params does each take? What is return value?
        # repr(m)
        # The following is about Myro.forward(), m:
        # '<System.Reflection.MonoMethod object at 0x0000000000000066 [Void forward(Single, Nullable`1)]>'
        # >>> m.GetParameters()
        # Array[ParameterInfo]((<System.Reflection.ParameterInfo object at 0x000000000000006C [Single power]>, <System.Reflection.ParameterInfo object at 0x000000000000006D [System.Nullable`1[[System.Single, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]] time]>))
        # >>> dir(m.GetParameters()[0])
        # ['Attributes', 'AttrsImpl', 'ClassImpl', 'DefaultValue', 'DefaultValueImpl', 'Equals', 'Finalize', 'GetCustomAttributes', 'GetHashCode', 'GetIDsOfNames', 'GetOptionalCustomModifiers', 'GetRequiredCustomModifiers', 'GetType', 'GetTypeInfo', 'GetTypeInfoCount', 'Invoke', 'IsDefined', 'IsIn', 'IsLcid', 'IsOptional', 'IsOut', 'IsRetval', 'Member', 'MemberImpl', 'MemberwiseClone', 'MetadataToken', 'Name', 'NameImpl', 'ParameterType', 'Position', 'PositionImpl', 'RawDefaultValue',...]
        # p is a parameter:
        # p.Attributes
        # <enum System.Reflection.ParameterAttributes: Optional, HasDefault>
        # p.DefaultValue, p.ParameterType, 
        # 1, System.Single, 
 
        # How to find static fields in class:
        # >>> clr.References[2].ManifestModule.GetTypes()[0].GetFields()[0].Name
        # 'robot'
