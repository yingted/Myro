import clr
import System
import time

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

def get_items(dll_file, _class, item_type):
    # method_type is from System.Reflection.MemberTypes.Method
    # How to get all of the items in a class:
    # >>> [x.Name for x in clr.References[2].ManifestModule.GetTypes()[0].GetMembers()]
    # ['init', 'forward', 'backward', 'Equals', 'GetHashCode', 'GetType', 'ToString', 'robot', '_Scribbler', '_Robot']
    # MemberType -> System.Reflection.MemberTypes.Method
    items = []
    reference = get_reference(dll_file)
    if reference:
        manifest = reference.ManifestModule
        if manifest:
            mtypes = manifest.GetTypes()
            if mtypes:
                for c in mtypes:
                    if c is not None and c.Name == _class:
                        for member in c.GetMembers():
                            if (member and (member.Name not in ["GetType"]) and 
                                (int(System.Reflection.MethodAttributes.VtableLayoutMask) & int(member.Attributes)) == 0 and 
                                member.MemberType == item_type and
                                not member.Name.startswith("_")):
                                items.append((c, member))
    return items

def get_methods(dll_file, _class):
    return get_items(dll_file, _class, System.Reflection.MemberTypes.Method)

def get_fields(dll_file, _class):
    # How to find static fields in class:
    # >>> clr.References[2].ManifestModule.GetTypes()[0].GetFields()[0].Name
    # 'robot'
    return get_items(dll_file, _class, System.Reflection.MemberTypes.Field)

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
