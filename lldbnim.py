from __future__ import annotations

import json
import os
import re
import traceback
from typing import *

import lldb

"""
The lldb module contains the public APIs for Python binding.

Some of the important classes are described here:

* :py:class:`SBTarget`: Represents the target program running under the debugger.
* :py:class:`SBProcess`: Represents the process associated with the target program.
* :py:class:`SBThread`: Represents a thread of execution. :py:class:`SBProcess` contains SBThreads.
* :py:class:`SBFrame`: Represents one of the stack frames associated with a thread. :py:class:`SBThread`
  contains SBFrame(s).
* :py:class:`SBSymbolContext`: A container that stores various debugger related info.
* :py:class:`SBValue`: Represents the value of a variable, a register, or an expression.
* :py:class:`SBModule`: Represents an executable image and its associated object and symbol
  files.  :py:class:`SBTarget` contains SBModule.
* :py:class:`SBBreakpoint`: Represents a logical breakpoint and its associated settings.
  :py:class:`SBTarget` contains SBBreakpoints.
* :py:class:`SBSymbol`: Represents the symbol possibly associated with a stack frame.
* :py:class:`SBCompileUnit`: Represents a compilation unit, or compiled source file.
* :py:class:`SBFunction`: Represents a generic function, which can be inlined or not.
* :py:class:`SBBlock`: Represents a lexical block. :py:class:`SBFunction` contains SBBlocks.
* :py:class:`SBLineEntry`: Specifies an association with a contiguous range of instructions
  and a source file location. :py:class:`SBCompileUnit` contains SBLineEntry.

The different enums in the `lldb` module are described in :doc:`python_api_enums`.


"""



class Format:
    ...
class TypeSummaryCapping:
    ...

class TypeClass:
    ...
class MatchType:
    ...
class WatchIdT:
    ...

class MemberFunctionKind:
    ...

class SectionType:
    ...
class SymbolType:
    ...

class QueueKind:
    ...

class UInt8ConstStar:
    ...
class QueueItemKind:
    ...
class StructuredDataType:
    ...

class InstrumentationRuntimeType:
    ...
class QueueItemSP:
    ...
class StopReason:
    ...
class AddrT:
    ...
class BasicType:
    ...
    
class QueueIdT:
    ...

class ValueType:
    ...

class TemplateArgumentKind:
    ...

class UInt8T:
    ...
class UInt16T:
    ...    
class UInt32T:
    ...    
class BreakIdT:
    ...
class CharConstStar:
    ...
class TIdT:
    ...
class SizeT:
    ...
class PIDT:
    ...
class BreakpointEventType:
    ...
class CommandArgumentType:
    ...
class ConnectionStatus:
    ...
class VoidStar:
    ...
class CharStar:
    ...
class ByteOrder:
    ...
class UInt64TStar:
    ...
class UInt32TStar:
    ...
class UInt16TStar:
    ...
class UInt8TStar:
    ...

class Int64TStar:
    ...
class Int32TStar:
    ...
class DoubleStar:
    ...
class StateType:
    ...

class PathType:
    ...
class ThreadT:
    ...
class FileSP:
    ...
class ThreadFuncT:
    ...
class ReturnStatus:
    ...
class ThreadResultT:
    ...

class VoidConstStar:
    ...
class LanguageType:
    ...
class WatchpointEventType:
    ...

class LogOutputCallback:
    ...

class DescriptionLevel:
    ...
class ErrorType:
    ...
class Char:
    ...
class EventSP:
    ...
class OffsetT:
    ...
class UInt64T:
    ...
class Int8T:
    ...
class Int16T:
    ...
class Int32T:
    ...
class Int64T:
    ...
class QueueSP:
    ...
class CharConstStarStar:
    ...
class LongDouble:
    ...

class ScriptLanguage:
    ...
class UserIdT:
    ...
class CharConst:
    ...

class DynamicValueType:
    ...

def SBBreakpoint_EventIsBreakpointEvent(event: SBEvent) -> bool:
    ...

def SBBreakpoint_GetBreakpointEventTypeFromEvent(event: SBEvent) -> BreakpointEventType:
    ...

def SBBreakpoint_GetBreakpointFromEvent(event: SBEvent) -> SBBreakpoint:
    ...

def SBBreakpoint_GetBreakpointLocationAtIndexFromEvent(event: SBEvent, loc_idx: UInt32T) -> SBBreakpointLocation:
    ...

def SBBreakpoint_GetNumBreakpointLocationsFromEvent(event_sp: SBEvent) -> UInt32T:
    ...

def SBCommandInterpreter_GetArgumentTypeAsCString(arg_type: CommandArgumentType) -> CharConstStar:
    ...

def SBCommandInterpreter_GetArgumentDescriptionAsCString(arg_type: CommandArgumentType) -> CharConstStar:
    ...

def SBCommandInterpreter_EventIsCommandInterpreterEvent(event: SBEvent) -> bool:
    ...

def SBCommandInterpreter_GetBroadcasterClass() -> CharConstStar:
    ...

def SBCommunication_GetBroadcasterClass() -> CharConstStar:
    ...

def SBData_CreateDataFromCString(endian: ByteOrder, addr_byte_size: UInt32T, data: CharConstStar) -> SBData:
    ...

def SBData_CreateDataFromUInt64Array(endian: ByteOrder, addr_byte_size: UInt32T, array: UInt64TStar) -> SBData:
    ...

def SBData_CreateDataFromUInt32Array(endian: ByteOrder, addr_byte_size: UInt32T, array: UInt32TStar) -> SBData:
    ...

def SBData_CreateDataFromSInt64Array(endian: ByteOrder, addr_byte_size: UInt32T, array: Int64TStar) -> SBData:
    ...

def SBData_CreateDataFromSInt32Array(endian: ByteOrder, addr_byte_size: UInt32T, array: Int32TStar) -> SBData:
    ...

def SBData_CreateDataFromDoubleArray(endian: ByteOrder, addr_byte_size: UInt32T, array: DoubleStar) -> SBData:
    ...

def SBDebugger_Initialize() -> None:
    ...

def SBDebugger_InitializeWithErrorHandling() -> SBError:
    ...

def SBDebugger_Terminate() -> None:
    ...

def SBDebugger_Create(source_init_files: bool = None ,  log_callback: LogOutputCallback = None) -> SBDebugger:
    ...

def SBDebugger_Destroy(debugger: SBDebugger) -> None:
    ...

def SBDebugger_MemoryPressureDetected() -> None:
    ...

def SBDebugger_GetDefaultArchitecture(arch_name: CharStar, arch_name_len: SizeT) -> bool:
    ...

def SBDebugger_SetDefaultArchitecture(arch_name: CharConstStar) -> bool:
    ...

def SBDebugger_GetVersionString() -> CharConstStar:
    ...

def SBDebugger_StateAsCString(state: StateType) -> CharConstStar:
    ...

def SBDebugger_GetBuildConfiguration() -> SBStructuredData:
    ...

def SBDebugger_StateIsRunningState(state: StateType) -> bool:
    ...

def SBDebugger_StateIsStoppedState(state: StateType) -> bool:
    ...

def SBDebugger_FindDebuggerWithID(id: int) -> SBDebugger:
    ...

def SBDebugger_SetInternalVariable(var_name: CharConstStar, value: CharConstStar, debugger_instance_name: CharConstStar) -> SBError:
    ...

def SBDebugger_GetInternalVariableValue(var_name: CharConstStar, debugger_instance_name: CharConstStar) -> SBStringList:
    ...


def SBEvent_GetCStringFromEvent(event: SBEvent) -> CharConstStar:
    ...

def SBFile_MakeBorrowed(BORROWED: FileSP) -> SBFile:
    r"""
    initialize a SBFile from a python file object
    """
    ...

def SBFile_MakeForcingIOMethods(FORCE_IO_METHODS: FileSP) -> SBFile:
    r"""
    initialize a SBFile from a python file object
    """
    ...

def SBFile_MakeBorrowedForcingIOMethods(BORROWED_FORCE_IO_METHODS: FileSP) -> SBFile:
    r"""
    initialize a SBFile from a python file object
    """
    ...


def SBFileSpec_ResolvePath(src_path: CharConstStar, dst_path: CharStar, dst_len: SizeT) -> int:
    ...


def SBHostOS_GetProgramFileSpec() -> SBFileSpec:
    ...

def SBHostOS_GetLLDBPythonPath() -> SBFileSpec:
    ...

def SBHostOS_GetLLDBPath(path_type: PathType) -> SBFileSpec:
    ...

def SBHostOS_GetUserHomeDirectory() -> SBFileSpec:
    ...

def SBHostOS_ThreadCreated(name: CharConstStar) -> None:
    ...

def SBHostOS_ThreadCreate(name: CharConstStar, arg2: ThreadFuncT, thread_arg: VoidStar, err: SBError) -> ThreadT:
    ...

def SBHostOS_ThreadCancel(thread: ThreadT, err: SBError) -> bool:
    ...

def SBHostOS_ThreadDetach(thread: ThreadT, err: SBError) -> bool:
    ...

def SBHostOS_ThreadJoin(thread: ThreadT, result: ThreadResultT, err: SBError) -> bool:
    ...

def SBModuleSpecList_GetModuleSpecifications(path: CharConstStar) -> SBModuleSpecList:
    ...

def SBPlatform_GetHostPlatform() -> SBPlatform:
    ...

def SBProcess_GetBroadcasterClassName() -> CharConstStar:
    ...

def SBProcess_GetStateFromEvent(event: SBEvent) -> StateType:
    ...

def SBProcess_GetRestartedFromEvent(event: SBEvent) -> bool:
    ...

def SBProcess_GetNumRestartedReasonsFromEvent(event: SBEvent) -> SizeT:
    ...

def SBProcess_GetRestartedReasonAtIndexFromEvent(event: SBEvent, idx: SizeT) -> CharConstStar:
    ...

def SBProcess_GetProcessFromEvent(event: SBEvent) -> SBProcess:
    ...

def SBProcess_GetInterruptedFromEvent(event: SBEvent) -> bool:
    ...

def SBProcess_GetStructuredDataFromEvent(event: SBEvent) -> SBStructuredData:
    ...

def SBProcess_EventIsProcessEvent(event: SBEvent) -> bool:
    ...

def SBProcess_EventIsStructuredDataEvent(event: SBEvent) -> bool:
    ...

def SBLanguageRuntime_GetLanguageTypeFromString(string: CharConstStar) -> LanguageType:
    ...

def SBLanguageRuntime_GetNameForLanguageType(language: LanguageType) -> CharConstStar:
    ...


def in_range(symbol, section):
    """Test whether a symbol is within the range of a section."""
    symSA = symbol.GetStartAddress().GetFileAddress()
    symEA = symbol.GetEndAddress().GetFileAddress()
    secSA = section.GetFileAddress()
    secEA = secSA + section.GetByteSize()

    if symEA != LLDB_INVALID_ADDRESS:
        if secSA <= symSA and symEA <= secEA:
            return True
        else:
            return False
    else:
        if secSA <= symSA and symSA < secEA:
            return True
        else:
            return False



def SBModule_GarbageCollectAllocatedModules() -> None:
    r"""
    SBModule_GarbageCollectAllocatedModules()

        Removes all modules which are no longer needed by any part of LLDB from
        the module cache.

        This is an implementation detail exposed for testing and should not be
        relied upon. Use SBDebugger::MemoryPressureDetected instead to reduce
        LLDB's memory consumption during execution.

    """
    ...


def SBReproducer_Capture(path: CharConstStar) -> CharConstStar:
    ...

def SBReproducer_PassiveReplay(path: CharConstStar) -> CharConstStar:
    ...

def SBReproducer_SetAutoGenerate(b: bool) -> bool:
    ...

def SBReproducer_SetWorkingDirectory(path: CharConstStar) -> None:
    ...

def SBTarget_GetBroadcasterClassName() -> CharConstStar:
    ...

def SBTarget_EventIsTargetEvent(event: SBEvent) -> bool:
    ...

def SBTarget_GetTargetFromEvent(event: SBEvent) -> SBTarget:
    ...

def SBTarget_GetNumModulesFromEvent(event: SBEvent) -> UInt32T:
    ...

def SBTarget_GetModuleAtIndexFromEvent(idx: UInt32T, event: SBEvent) -> SBModule:
    ...

def SBThread_GetBroadcasterClassName() -> CharConstStar:
    ...

def SBThread_EventIsThreadEvent(event: SBEvent) -> bool:
    ...

def SBThread_GetStackFrameFromEvent(event: SBEvent) -> SBFrame:
    ...

def SBThread_GetThreadFromEvent(event: SBEvent) -> SBThread:
    ...

def SBTypeSummary_CreateWithSummaryString(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
    ...

def SBTypeSummary_CreateWithFunctionName(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
    ...

def SBTypeSummary_CreateWithScriptCode(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
    ...


def SBTypeSynthetic_CreateWithClassName(data: CharConstStar, options: UInt32T=0) -> SBTypeSynthetic:
    ...

def SBTypeSynthetic_CreateWithScriptCode(data: CharConstStar, options: UInt32T=0) -> SBTypeSynthetic:
    ...

def SBWatchpoint_EventIsWatchpointEvent(event: SBEvent) -> bool:
    ...

def SBWatchpoint_GetWatchpointEventTypeFromEvent(event: SBEvent) -> WatchpointEventType:
    ...

def SBWatchpoint_GetWatchpointFromEvent(event: SBEvent) -> SBWatchpoint:
    ...



def command(command_name=None, doc=None):
    
    """A decorator function that registers an LLDB command line
        command that is bound to the function it is attached to."""
    
    """@import lldb
    def callable(function):
        ""Registers an lldb command for the decorated function.""
        command = "command script add -f %s.%s %s" % (function.__module__, function.__name__, command_name or function.__name__)
        lldb.debugger.HandleCommand(command)
        if doc:
            function.__doc__ = doc
        return function

    return callable"""
    ...



##########################################################################################

class SBAddress:
    r"""
    A section + offset based address class.

    The SBAddress class allows addresses to be relative to a section
    that can move during runtime due to images (executables, shared
    libraries, bundles, frameworks) being loaded at different
    addresses than the addresses found in the object file that
    represents them on disk. There are currently two types of addresses
    for a section:

    * file addresses
    * load addresses

    File addresses represents the virtual addresses that are in the 'on
    disk' object files. These virtual addresses are converted to be
    relative to unique sections scoped to the object file so that
    when/if the addresses slide when the images are loaded/unloaded
    in memory, we can easily track these changes without having to
    update every object (compile unit ranges, line tables, function
    address ranges, lexical block and inlined subroutine address
    ranges, global and static variables) each time an image is loaded or
    unloaded.

    Load addresses represents the virtual addresses where each section
    ends up getting loaded at runtime. Before executing a program, it
    is common for all of the load addresses to be unresolved. When a
    DynamicLoader plug-in receives notification that shared libraries
    have been loaded/unloaded, the load addresses of the main executable
    and any images (shared libraries) will be  resolved/unresolved. When
    this happens, breakpoints that are in one of these sections can be
    set/cleared.

    See docstring of SBFunction for example usage of SBAddress.
    """

    
    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__( self, rhs: SBAddress):
        ...
    @overload
    def __init__(self, section: SBSection, offset: AddrT) :
        ...
    @overload
    def __init(self, load_addr: AddrT , target :  SBTarget):
        ...

        

    def IsValid(self) -> bool:
        ...


    def __eq__(self, other):
        ...


    def __ne__(self, rhs: SBAddress) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def GetFileAddress(self) -> AddrT:
        ...

    def GetLoadAddress(self, target: SBTarget) -> AddrT:
        ...

    def SetLoadAddress(self, load_addr: AddrT, target: SBTarget) -> None:
        ...

    def OffsetAddress(self, offset: AddrT) -> bool:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def GetSection(self) -> SBSection:
        ...

    def GetOffset(self) -> AddrT:
        ...

    def SetAddress(self, section: SBSection, offset: AddrT) -> None:
        ...

    def GetSymbolContext(self, resolve_scope: UInt32T) -> SBSymbolContext:
        r"""
            GetSymbolContext() and the following can lookup symbol information for a given address.
            An address might refer to code or data from an existing module, or it
            might refer to something on the stack or heap. The following functions
            will only return valid values if the address has been resolved to a code
            or data address using :py:class:`SBAddress.SetLoadAddress' or
            :py:class:`SBTarget.ResolveLoadAddress`.
        """
        ...

    def GetModule(self) -> SBModule:
        r"""
            GetModule() and the following grab individual objects for a given address and
            are less efficient if you want more than one symbol related objects.
            Use :py:class:`SBAddress.GetSymbolContext` or
            :py:class:`SBTarget.ResolveSymbolContextForAddress` when you want multiple
            debug symbol related objects for an address.
            One or more bits from the SymbolContextItem enumerations can be logically
            OR'ed together to more efficiently retrieve multiple symbol objects.
        """
        ...

    def GetCompileUnit(self) -> SBCompileUnit:
        ...

    def GetFunction(self) -> SBFunction:
        ...

    def GetBlock(self) -> SBBlock:
        ...

    def GetSymbol(self) -> SBSymbol:
        ...

    def GetLineEntry(self) -> SBLineEntry:
        ...

    def __str__(self) -> str:
        ...

    def __get_load_addr_property__ (self) -> AddrT:
        '''Get the load address for a lldb.SBAddress using the current target. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.'''
        """@if not target:
            raise RuntimeError(self.__runtime_error_str)
        return self.GetLoadAddress (target)"""
        ...

    def __set_load_addr_property__ (self, load_addr) -> None:
        '''Set the load address for a lldb.SBAddress using the current target. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.'''
        """@if not target:
            raise RuntimeError(self.__runtime_error_str)
        return self.SetLoadAddress (load_addr, target)"""
        ...

    def __int__(self):
        '''Convert an address to a load address if there is a process and that process is alive, or to a file address otherwise. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.'''
        """@if not process or not target:
            raise RuntimeError(self.__runtime_error_str)
        if process.is_alive:
            return self.GetLoadAddress (target)
        return self.GetFileAddress ()"""
        ...

    def __oct__(self):
        '''Convert the address to an octal string. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.'''
        """@return '%o' % int(self)"""
        ...

    def __hex__(self):
        '''Convert the address to an hex string. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.'''
        """@return '0x%x' % int(self)"""
        ...

    module = property(GetModule, None, doc='''A read only property that returns an lldb object that represents the module (lldb.SBModule) that this address resides within.''')
    compile_unit = property(GetCompileUnit, None, doc='''A read only property that returns an lldb object that represents the compile unit (lldb.SBCompileUnit) that this address resides within.''')
    line_entry = property(GetLineEntry, None, doc='''A read only property that returns an lldb object that represents the line entry (lldb.SBLineEntry) that this address resides within.''')
    function = property(GetFunction, None, doc='''A read only property that returns an lldb object that represents the function (lldb.SBFunction) that this address resides within.''')
    block = property(GetBlock, None, doc='''A read only property that returns an lldb object that represents the block (lldb.SBBlock) that this address resides within.''')
    symbol = property(GetSymbol, None, doc='''A read only property that returns an lldb object that represents the symbol (lldb.SBSymbol) that this address resides within.''')
    offset = property(GetOffset, None, doc='''A read only property that returns the section offset in bytes as an integer.''')
    section = property(GetSection, None, doc='''A read only property that returns an lldb object that represents the section (lldb.SBSection) that this address resides within.''')
    file_addr = property(GetFileAddress, None, doc='''A read only property that returns file address for the section as an integer. This is the address that represents the address as it is found in the object file that defines it.''')
    load_addr = property(__get_load_addr_property__, __set_load_addr_property__, doc='''A read/write property that gets/sets the SBAddress using load address. This resolves the SBAddress using the SBTarget from lldb.target so this property can ONLY be used in the interactive script interpreter (i.e. under the lldb script command). For things like Python based commands and breakpoint callbacks use GetLoadAddress instead.''')



class SBAttachInfo:
    r"""Describes how to attach when calling :py:class:`SBTarget.Attach`."""
    
    @overload
    def __init__(self ):
        ...

    @overload
    def __init__(self , pid : PIDT):
        ...
        
    @overload
    def __init__(self , path: CharConstStar , wait_for: bool  , _async: bool = None):
        ...

    def GetProcessID(self) -> PIDT:
        ...

    def SetProcessID(self, pid: PIDT) -> None:
        ...

    def SetExecutable(self, path_or_file: Union[SBFileSpec , CharConstStar]) -> None:
        ...

    def GetWaitForLaunch(self) -> bool:
        ...

    def SetWaitForLaunch(self, b : bool, _async: bool) -> None:
        ...

    def GetIgnoreExisting(self) -> bool:
        ...

    def SetIgnoreExisting(self, b: bool) -> None:
        ...

    def GetResumeCount(self) -> UInt32T:
        ...

    def SetResumeCount(self, c: UInt32T) -> None:
        ...

    def GetProcessPluginName(self) -> CharConstStar:
        ...

    def SetProcessPluginName(self, plugin_name: CharConstStar) -> None:
        ...

    def GetUserID(self) -> UInt32T:
        ...

    def GetGroupID(self) -> UInt32T:
        ...

    def UserIDIsValid(self) -> bool:
        ...

    def GroupIDIsValid(self) -> bool:
        ...

    def SetUserID(self, uid: UInt32T) -> None:
        ...

    def SetGroupID(self, gid: UInt32T) -> None:
        ...

    def GetEffectiveUserID(self) -> UInt32T:
        ...

    def GetEffectiveGroupID(self) -> UInt32T:
        ...

    def EffectiveUserIDIsValid(self) -> bool:
        ...

    def EffectiveGroupIDIsValid(self) -> bool:
        ...

    def SetEffectiveUserID(self, uid: UInt32T) -> None:
        ...

    def SetEffectiveGroupID(self, gid: UInt32T) -> None:
        ...

    def GetParentProcessID(self) -> PIDT:
        ...

    def SetParentProcessID(self, pid: PIDT) -> None:
        ...

    def ParentProcessIDIsValid(self) -> bool:
        ...

    def GetListener(self) -> SBListener:
        ...

    def SetListener(self, listener: SBListener) -> None:
        ...
    
class SBBlock:
    r"""Represents a lexical block. SBFunction contains SBBlock(s)."""

    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self, rhs : SBBlock):
        ...

    def IsInlined(self) -> bool:
        r"""
        Is this block contained within an inlined function?
        """
        ...

    def IsValid(self) -> bool:
        ...

    def GetInlinedName(self) -> Optional[CharConstStar]:
        r"""
            Get the function name if this block represents an inlined function;
            otherwise, return None.
        """
        ...

    def GetInlinedCallSiteFile(self) -> SBFileSpec:
        r"""
            Get the call site file if this block represents an inlined function;
            otherwise, return an invalid file spec.
        """
        ...

    def GetInlinedCallSiteLine(self) -> UInt32T:
        r"""
            Get the call site line if this block represents an inlined function;
            otherwise, return 0.
        """
        ...

    def GetInlinedCallSiteColumn(self) -> UInt32T:
        r"""
            Get the call site column if this block represents an inlined function;
            otherwise, return 0.
        """
        ...

    def GetParent(self) -> SBBlock:
        ...

    def GetContainingInlinedBlock(self) -> SBBlock:
        r"""
        Get the inlined block that is or contains this block.
        """
        ...

    def GetSibling(self) -> SBBlock:
        r"""
        Get the sibling block for this block.
        """
        ...

    def GetFirstChild(self) -> SBBlock:
        r"""
        Get the first child block.
        """
        ...

    def GetNumRanges(self) -> UInt32T:
        ...

    def GetRangeStartAddress(self, idx: UInt32T) -> SBAddress:
        ...

    def GetRangeEndAddress(self, idx: UInt32T) -> SBAddress:
        ...

    def GetRangeIndexForBlockAddress(self, block_addr: SBAddress) -> UInt32T:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...
    
    @overload
    def GetVariables(self,frame: SBFrame , arguments: bool, locals: bool  , statis : bool , use_dynamic: DynamicValueType) -> SBValueList:
        ...

    @overload
    def GetVariables(self,target: SBTarget , arguments: bool, locals: bool  , statis : bool) -> SBValueList:
        ...

    def __str__(self) -> str:
        ...

    def get_range_at_index(self, idx):
        """@if idx < self.GetNumRanges():
            return [self.GetRangeStartAddress(idx), self.GetRangeEndAddress(idx)]
        return []"""

    class ranges_access:
        

        '''A helper object that will lazily hand out an array of lldb.SBAddress that represent address ranges for a block.'''
        def __init__(self, sbblock):
            self.sbblock = sbblock

        def __len__(self):
            if self.sbblock:
                return int(self.sbblock.GetNumRanges())
            return 0

        def __getitem__(self, key):
            count = len(self)
            if type(key) is int:
                return self.sbblock.get_range_at_index (key);
            if isinstance(key, SBAddress):
                range_idx = self.sbblock.GetRangeIndexForBlockAddress(key);
                if range_idx < len(self):
                    return [self.sbblock.GetRangeStartAddress(range_idx), self.sbblock.GetRangeEndAddress(range_idx)]
            else:
                print("error: unsupported item type: %s" % type(key))
            return None

    def get_ranges_access_object(self) -> ranges_access:
        '''An accessor function that returns a ranges_access() object which allows lazy block address ranges access.'''
        ...

    def get_ranges_array(self):
        '''An accessor function that returns an array object that contains all ranges in this block object.'''
        """@if not hasattr(self, 'ranges_array'):
            self.ranges_array = []
            for idx in range(self.num_ranges):
                self.ranges_array.append ([self.GetRangeStartAddress(idx), self.GetRangeEndAddress(idx)])
        return self.ranges_array"""

    def get_call_site(self):
        return declaration(self.GetInlinedCallSiteFile(), self.GetInlinedCallSiteLine(), self.GetInlinedCallSiteColumn())

    parent = property(GetParent, None, doc='''A read only property that returns the same result as GetParent().''')
    first_child = property(GetFirstChild, None, doc='''A read only property that returns the same result as GetFirstChild().''')
    call_site = property(get_call_site, None, doc='''A read only property that returns a lldb.declaration object that contains the inlined call site file, line and column.''')
    sibling = property(GetSibling, None, doc='''A read only property that returns the same result as GetSibling().''')
    name = property(GetInlinedName, None, doc='''A read only property that returns the same result as GetInlinedName().''')
    inlined_block = property(GetContainingInlinedBlock, None, doc='''A read only property that returns the same result as GetContainingInlinedBlock().''')
    range = property(get_ranges_access_object, None, doc='''A read only property that allows item access to the address ranges for a block by integer (range = block.range[0]) and by lldb.SBAddress (find the range that contains the specified lldb.SBAddress like "pc_range = lldb.frame.block.range[frame.addr]").''')
    ranges = property(get_ranges_array, None, doc='''A read only property that returns a list() object that contains all of the address ranges for the block.''')
    num_ranges = property(GetNumRanges, None, doc='''A read only property that returns the same result as GetNumRanges().''')

class SBBreakpoint:
    r"""
    Represents a logical breakpoint and its associated settings.

    For example (from test/functionalities/breakpoint/breakpoint_ignore_count/
    TestBreakpointIgnoreCount.py),::

        def breakpoint_ignore_count_python(self):
            '''Use Python APIs to set breakpoint ignore count.'''
            exe = os.path.join(os.getcwd(), 'a.out')

            # Create a target by the debugger.
            target = self.dbg.CreateTarget(exe)
            self.assertTrue(target, VALID_TARGET)

            # Now create a breakpoint on main.c by name 'c'.
            breakpoint = target.BreakpointCreateByName('c', 'a.out')
            self.assertTrue(breakpoint and
                            breakpoint.GetNumLocations() == 1,
                            VALID_BREAKPOINT)

            # Get the breakpoint location from breakpoint after we verified that,
            # indeed, it has one location.
            location = breakpoint.GetLocationAtIndex(0)
            self.assertTrue(location and
                            location.IsEnabled(),
                            VALID_BREAKPOINT_LOCATION)

            # Set the ignore count on the breakpoint location.
            location.SetIgnoreCount(2)
            self.assertTrue(location.GetIgnoreCount() == 2,
                            'SetIgnoreCount() works correctly')

            # Now launch the process, and do not stop at entry point.
            process = target.LaunchSimple(None, None, os.getcwd())
            self.assertTrue(process, PROCESS_IS_VALID)

            # Frame#0 should be on main.c:37, frame#1 should be on main.c:25, and
            # frame#2 should be on main.c:48.
            #lldbutil.print_stacktraces(process)
            from lldbutil import get_stopped_thread
            thread = get_stopped_thread(process, lldb.eStopReasonBreakpoint)
            self.assertTrue(thread != None, 'There should be a thread stopped due to breakpoint')
            frame0 = thread.GetFrameAtIndex(0)
            frame1 = thread.GetFrameAtIndex(1)
            frame2 = thread.GetFrameAtIndex(2)
            self.assertTrue(frame0.GetLineEntry().GetLine() == self.line1 and
                            frame1.GetLineEntry().GetLine() == self.line3 and
                            frame2.GetLineEntry().GetLine() == self.line4,
                            STOPPED_DUE_TO_BREAKPOINT_IGNORE_COUNT)

            # The hit count for the breakpoint should be 3.
            self.assertTrue(breakpoint.GetHitCount() == 3)

            process.Continue()

    SBBreakpoint supports breakpoint location iteration, for example,::

        for bl in breakpoint:
            print('breakpoint location load addr: %s' % hex(bl.GetLoadAddress()))
            print('breakpoint location condition: %s' % hex(bl.GetCondition()))

    and rich comparison methods which allow the API program to use,::

        if aBreakpoint == bBreakpoint:
            ...

    to compare two breakpoints for equality.
    """



    @staticmethod
    def EventIsBreakpointEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetBreakpointEventTypeFromEvent(event: SBEvent) -> BreakpointEventType:
        ...

    @staticmethod
    def GetBreakpointFromEvent(event: SBEvent) -> SBBreakpoint:
        ...

    @staticmethod
    def GetBreakpointLocationAtIndexFromEvent(event: SBEvent, loc_idx: UInt32T) -> SBBreakpointLocation:
        ...

    @staticmethod
    def GetNumBreakpointLocationsFromEvent(event_sp: SBEvent) -> UInt32T:
        ...

    
    @overload
    def __init__(self):
        ...
    
    @overload
    def __init__(self, rhs : SBBreakpoint ):
        ...
    
    
    def __eq__(self, rhs: SBBreakpoint) -> bool:
        ...

    def __ne__(self, rhs: SBBreakpoint) -> bool:
        ...

    def GetID(self) -> BreakIdT:
        ...

    def IsValid(self) -> bool:
        ...

    def ClearAllBreakpointSites(self) -> None:
        ...

    def GetTarget(self) -> SBTarget:
        ...

    def FindLocationByAddress(self, vm_addr: AddrT) -> SBBreakpointLocation:
        ...

    def FindLocationIDByAddress(self, vm_addr: AddrT) -> BreakIdT:
        ...

    def FindLocationByID(self, bp_loc_id: BreakIdT) -> SBBreakpointLocation:
        ...

    def GetLocationAtIndex(self, index: UInt32T) -> SBBreakpointLocation:
        ...

    def SetEnabled(self, enable: bool) -> None:
        ...

    def IsEnabled(self) -> bool:
        ...

    def SetOneShot(self, one_shot: bool) -> None:
        ...

    def IsOneShot(self) -> bool:
        ...

    def IsInternal(self) -> bool:
        ...

    def GetHitCount(self) -> UInt32T:
        ...

    def SetIgnoreCount(self, count: UInt32T) -> None:
        ...

    def GetIgnoreCount(self) -> UInt32T:
        ...

    def SetCondition(self, condition: CharConstStar) -> None:
        r"""
            The breakpoint stops only if the condition expression evaluates to true.
        """
        ...

    def GetCondition(self) -> CharConstStar:
        r"""
            Get the condition expression for the breakpoint.
        """
        ...

    def SetAutoContinue(self, auto_continue: bool) -> None:
        ...

    def GetAutoContinue(self) -> bool:
        ...

    def SetThreadID(self, sb_thread_id: TIdT) -> None:
        ...

    def GetThreadID(self) -> TIdT:
        ...

    def SetThreadIndex(self, index: UInt32T) -> None:
        ...

    def GetThreadIndex(self) -> UInt32T:
        ...

    def SetThreadName(self, thread_name: CharConstStar) -> None:
        ...

    def GetThreadName(self) -> CharConstStar:
        ...

    def SetQueueName(self, queue_name: CharConstStar) -> None:
        ...

    def GetQueueName(self) -> CharConstStar:
        ...

    def SetScriptCallbackFunction(self, callback_function_name: CharConstStar, extra_args: SBStructuredData = None) -> SBError:
        r"""
            Set the name of the script function to be called when the breakpoint is hit.
            To use this variant, the function should take (frame, bp_loc, extra_args, internal_dict) and
            when the breakpoint is hit the extra_args will be passed to the callback function.
        """
        ...

    def SetScriptCallbackBody(self, script_body_text: CharConstStar) -> SBError:
        r"""
            Provide the body for the script function to be called when the breakpoint is hit.
            The body will be wrapped in a function, which be passed two arguments:
            'frame' - which holds the bottom-most SBFrame of the thread that hit the breakpoint
            'bpno'  - which is the SBBreakpointLocation to which the callback was attached.

            The error parameter is currently ignored, but will at some point hold the Python
            compilation diagnostics.
            Returns true if the body compiles successfully, false if not.
        """
        ...

    def SetCommandLineCommands(self, commands: SBStringList) -> None:
        ...

    def GetCommandLineCommands(self, commands: SBStringList) -> bool:
        ...

    def AddName(self, new_name: CharConstStar) -> bool:
        ...

    def AddNameWithErrorHandling(self, new_name: CharConstStar) -> SBError:
        ...

    def RemoveName(self, name_to_remove: CharConstStar) -> None:
        ...

    def MatchesName(self, name: CharConstStar) -> bool:
        ...

    def GetNames(self, names: SBStringList) -> None:
        ...

    def GetNumResolvedLocations(self) -> SizeT:
        ...

    def GetNumLocations(self) -> SizeT:
        ...

    def GetDescription(self, description: SBStream , include_locations: bool = None) -> bool:
        ...

    def AddLocation(self, address: SBAddress) -> SBError:
        ...

    def SerializeToStructuredData(self) -> SBStructuredData:
        ...

    def IsHardware(self) -> bool:
        ...

    def __str__(self) -> str:
        ...

    """@"""
    class locations_access:
        '''A helper object that will lazily hand out locations for a breakpoint when supplied an index.'''
        def __init__(self, sbbreakpoint):
            self.sbbreakpoint = sbbreakpoint

        def __len__(self):
            if self.sbbreakpoint:
                return int(self.sbbreakpoint.GetNumLocations())
            return 0

        def __getitem__(self, key):
            if type(key) is int and key < len(self):
                return self.sbbreakpoint.GetLocationAtIndex(key)
            return None

    def get_locations_access_object(self) -> locations_access:
        '''An accessor function that returns a locations_access() object which allows lazy location access from a lldb.SBBreakpoint object.'''
        ...

    def get_breakpoint_location_list(self):
        '''An accessor function that returns a list() that contains all locations in a lldb.SBBreakpoint object.'''
        """@locations = []
        accessor = self.get_locations_access_object()
        for idx in range(len(accessor)):
            locations.append(accessor[idx])
        return locations"""

    def __iter__(self) -> Iterable[SBBreakpointLocation]:
        ...

    def __len__(self) -> int:
        ...

    locations = property(get_breakpoint_location_list, None, doc='''A read only property that returns a list() of lldb.SBBreakpointLocation objects for this breakpoint.''')
    location = property(get_locations_access_object, None, doc='''A read only property that returns an object that can access locations by index (not location ID) (location = bkpt.location[12]).''')
    id = property(GetID, None, doc='''A read only property that returns the ID of this breakpoint.''')
    enabled = property(IsEnabled, SetEnabled, doc='''A read/write property that configures whether this breakpoint is enabled or not.''')
    one_shot = property(IsOneShot, SetOneShot, doc='''A read/write property that configures whether this breakpoint is one-shot (deleted when hit) or not.''')
    num_locations = property(GetNumLocations, None, doc='''A read only property that returns the count of locations of this breakpoint.''')


class SBBreakpointList:
    r"""Represents a list of :py:class:`SBBreakpoint`."""

    
    

    def __init__(self, target: SBTarget):
        ...
    

    def GetSize(self) -> SizeT:
        ...

    def GetBreakpointAtIndex(self, idx: SizeT) -> SBBreakpoint:
        ...

    def FindBreakpointByID(self, arg2: BreakIdT) -> SBBreakpoint:
        ...

    def Append(self, sb_bkpt: SBBreakpoint) -> None:
        ...

    def AppendIfUnique(self, sb_bkpt: SBBreakpoint) -> bool:
        ...

    def AppendByID(self, id: BreakIdT) -> None:
        ...

    def Clear(self) -> None:
        ...


class SBBreakpointLocation:
    r"""
    Represents one unique instance (by address) of a logical breakpoint.

    A breakpoint location is defined by the breakpoint that produces it,
    and the address that resulted in this particular instantiation.
    Each breakpoint location has its settable options.

    :py:class:`SBBreakpoint` contains SBBreakpointLocation(s). See docstring of SBBreakpoint
    for retrieval of an SBBreakpointLocation from an SBBreakpoint.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self ,  rhs: SBBreakpointLocation ):
        ...
        

    def GetID(self) -> BreakIdT:
        ...

    def IsValid(self) -> bool:
        ...

    def GetAddress(self) -> SBAddress:
        ...

    def GetLoadAddress(self) -> AddrT:
        ...

    def SetEnabled(self, enabled: bool) -> None:
        ...

    def IsEnabled(self) -> bool:
        ...

    def GetHitCount(self) -> UInt32T:
        ...

    def GetIgnoreCount(self) -> UInt32T:
        ...

    def SetIgnoreCount(self, n: UInt32T) -> None:
        ...

    def SetCondition(self, condition: CharConstStar) -> None:
        r"""
            The breakpoint location stops only if the condition expression evaluates
            to true.
        """
        ...

    def GetCondition(self) -> CharConstStar:
        r"""
            Get the condition expression for the breakpoint location.
        """
        ...

    def GetAutoContinue(self) -> bool:
        ...

    def SetAutoContinue(self, auto_continue: bool) -> None:
        ...

    def SetScriptCallbackFunction(self, callback_function_name : CharConstStar  , extra_args : SBStructuredData) -> SBError:
        r"""
            Set the name of the script function to be called when the breakpoint is hit.
            To use this variant, the function should take (frame, bp_loc, extra_args, internal_dict) and
            when the breakpoint is hit the extra_args will be passed to the callback function.
        """
        ...

    def SetScriptCallbackBody(self, script_body_text: CharConstStar) -> SBError:
        r"""
            Provide the body for the script function to be called when the breakpoint location is hit.
            The body will be wrapped in a function, which be passed two arguments:
            'frame' - which holds the bottom-most SBFrame of the thread that hit the breakpoint
            'bpno'  - which is the SBBreakpointLocation to which the callback was attached.

            The error parameter is currently ignored, but will at some point hold the Python
            compilation diagnostics.
            Returns true if the body compiles successfully, false if not.
        """
        ...

    def SetCommandLineCommands(self, commands: SBStringList) -> None:
        ...

    def GetCommandLineCommands(self, commands: SBStringList) -> bool:
        ...

    def SetThreadID(self, sb_thread_id: TIdT) -> None:
        ...

    def GetThreadID(self) -> TIdT:
        ...

    def SetThreadIndex(self, index: UInt32T) -> None:
        ...

    def GetThreadIndex(self) -> UInt32T:
        ...

    def SetThreadName(self, thread_name: CharConstStar) -> None:
        ...

    def GetThreadName(self) -> CharConstStar:
        ...

    def SetQueueName(self, queue_name: CharConstStar) -> None:
        ...

    def GetQueueName(self) -> CharConstStar:
        ...

    def IsResolved(self) -> bool:
        ...

    def GetDescription(self, description: SBStream, level: DescriptionLevel) -> bool:
        ...

    def GetBreakpoint(self) -> SBBreakpoint:
        ...

    def __str__(self) -> str:
        ...



class SBBreakpointName:
    r"""
    Represents a breakpoint name registered in a given :py:class:`SBTarget`.

    Breakpoint names provide a way to act on groups of breakpoints.  When you add a
    name to a group of breakpoints, you can then use the name in all the command
    line lldb commands for that name.  You can also configure the SBBreakpointName
    options and those options will be propagated to any :py:class:`SBBreakpoint` s currently
    using that name.  Adding a name to a breakpoint will also apply any of the
    set options to that breakpoint.

    You can also set permissions on a breakpoint name to disable listing, deleting
    and disabling breakpoints.  That will disallow the given operation for breakpoints
    except when the breakpoint is mentioned by ID.  So for instance deleting all the
    breakpoints won't delete breakpoints so marked.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , target  : SBTarget ,name  : CharConstStar ):
        ...
    @overload
    def __init__(self , breakpoint  : SBBreakpoint , name  : CharConstStar ):
        ...
    def __init__(self , rhs  : SBBreakpointName ):
        ...

    def __eq__(self, rhs: SBBreakpointName) -> bool:
        ...

    def __ne__(self, rhs: SBBreakpointName) -> bool:
        ...


    def IsValid(self) -> bool:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def SetEnabled(self, enable: bool) -> None:
        ...

    def IsEnabled(self) -> bool:
        ...

    def SetOneShot(self, one_shot: bool) -> None:
        ...

    def IsOneShot(self) -> bool:
        ...

    def SetIgnoreCount(self, count: UInt32T) -> None:
        ...

    def GetIgnoreCount(self) -> UInt32T:
        ...

    def SetCondition(self, condition: CharConstStar) -> None:
        ...

    def GetCondition(self) -> CharConstStar:
        ...

    def SetAutoContinue(self, auto_continue: bool) -> None:
        ...

    def GetAutoContinue(self) -> bool:
        ...

    def SetThreadID(self, sb_thread_id: TIdT) -> None:
        ...

    def GetThreadID(self) -> TIdT:
        ...

    def SetThreadIndex(self, index: UInt32T) -> None:
        ...

    def GetThreadIndex(self) -> UInt32T:
        ...

    def SetThreadName(self, thread_name: CharConstStar) -> None:
        ...

    def GetThreadName(self) -> CharConstStar:
        ...

    def SetQueueName(self, queue_name: CharConstStar) -> None:
        ...

    def GetQueueName(self) -> CharConstStar:
        ...

    def SetScriptCallbackFunction(self, callback_function_name : CharConstStar , extra_args: SBStructuredData) -> SBError:
        ...

    def SetCommandLineCommands(self, commands: SBStringList) -> None:
        ...

    def GetCommandLineCommands(self, commands: SBStringList) -> bool:
        ...

    def SetScriptCallbackBody(self, script_body_text: CharConstStar) -> SBError:
        ...

    def GetHelpString(self) -> CharConstStar:
        ...

    def SetHelpString(self, help_string: CharConstStar) -> None:
        ...

    def GetAllowList(self) -> bool:
        ...

    def SetAllowList(self, value: bool) -> None:
        ...

    def GetAllowDelete(self) -> bool:
        ...

    def SetAllowDelete(self, value: bool) -> None:
        ...

    def GetAllowDisable(self) -> bool:
        ...

    def SetAllowDisable(self, value: bool) -> None:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...



class SBBroadcaster:
    r"""
    Represents an entity which can broadcast events.

    A default broadcaster is
    associated with an SBCommandInterpreter, SBProcess, and SBTarget.  For
    example, use ::

        broadcaster = process.GetBroadcaster()

    to retrieve the process's broadcaster.

    See also SBEvent for example usage of interacting with a broadcaster.
    """

    
    

    def __init__(self, *args):
        r"""
        __init__(SBBroadcaster self) -> SBBroadcaster
        __init__(SBBroadcaster self, char const * name) -> SBBroadcaster
        __init__(SBBroadcaster self, SBBroadcaster rhs) -> SBBroadcaster
        """
        ...
    

    def IsValid(self) -> bool:
        ...


    def Clear(self) -> None:
        ...

    def BroadcastEventByType(self, event_type: UInt32T, unique: bool=False) -> None:
        ...

    def BroadcastEvent(self, event: SBEvent, unique: bool=False) -> None:
        ...

    def AddInitialEventsToListener(self, listener: SBListener, requested_events: UInt32T) -> None:
        ...

    def AddListener(self, listener: SBListener, event_mask: UInt32T) -> UInt32T:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def EventTypeHasListeners(self, event_type: UInt32T) -> bool:
        ...

    def RemoveListener(self, *args) -> bool:
        """@"""
        ...

    def __eq__(self, rhs: SBBroadcaster) -> bool:
        ...

    def __ne__(self, rhs: SBBroadcaster) -> bool:
        ...

    def __eq__(self, rhs):
        ...

    def __ne__(self, rhs):
        ...


class SBCommandInterpreter:
    r"""
    SBCommandInterpreter handles/interprets commands for lldb.

    You get the command interpreter from the :py:class:`SBDebugger` instance.

    For example (from test/ python_api/interpreter/TestCommandInterpreterAPI.py),::

        def command_interpreter_api(self):
            '''Test the SBCommandInterpreter APIs.'''
            exe = os.path.join(os.getcwd(), 'a.out')

            # Create a target by the debugger.
            target = self.dbg.CreateTarget(exe)
            self.assertTrue(target, VALID_TARGET)

            # Retrieve the associated command interpreter from our debugger.
            ci = self.dbg.GetCommandInterpreter()
            self.assertTrue(ci, VALID_COMMAND_INTERPRETER)

            # Exercise some APIs....

            self.assertTrue(ci.HasCommands())
            self.assertTrue(ci.HasAliases())
            self.assertTrue(ci.HasAliasOptions())
            self.assertTrue(ci.CommandExists('breakpoint'))
            self.assertTrue(ci.CommandExists('target'))
            self.assertTrue(ci.CommandExists('platform'))
            self.assertTrue(ci.AliasExists('file'))
            self.assertTrue(ci.AliasExists('run'))
            self.assertTrue(ci.AliasExists('bt'))

            res = lldb.SBCommandReturnObject()
            ci.HandleCommand('breakpoint set -f main.c -l %d' % self.line, res)
            self.assertTrue(res.Succeeded())
            ci.HandleCommand('process launch', res)
            self.assertTrue(res.Succeeded())

            process = ci.GetProcess()
            self.assertTrue(process)

            ...

    The HandleCommand() instance method takes two args: the command string and
    an SBCommandReturnObject instance which encapsulates the result of command
    execution.
    """

    

    @staticmethod
    def GetArgumentTypeAsCString(arg_type: CommandArgumentType) -> CharConstStar:
        ...

    @staticmethod
    def GetArgumentDescriptionAsCString(arg_type: CommandArgumentType) -> CharConstStar:
        ...

    @staticmethod
    def EventIsCommandInterpreterEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetBroadcasterClass() -> CharConstStar:
        ...

    def __init__(self, rhs: SBCommandInterpreter):
        ...
    

    def IsValid(self) -> bool:
        ...

    def GetIOHandlerControlSequence(self, ch: Char) -> CharConstStar:
        ...

    def GetPromptOnQuit(self) -> bool:
        ...

    def SetPromptOnQuit(self, b: bool) -> None:
        ...

    def AllowExitCodeOnQuit(self, b: bool) -> None:
        ...

    def HasCustomQuitExitCode(self) -> bool:
        ...

    def GetQuitStatus(self) -> int:
        ...

    def ResolveCommand(self, command_line: CharConstStar, result: SBCommandReturnObject) -> None:
        ...

    def CommandExists(self, cmd: CharConstStar) -> bool:
        ...

    def AliasExists(self, cmd: CharConstStar) -> bool:
        ...

    def GetBroadcaster(self) -> SBBroadcaster:
        ...


    def HasCommands(self) -> bool:
        ...

    def HasAliases(self) -> bool:
        ...

    def HasAliasOptions(self) -> bool:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetDebugger(self) -> SBDebugger:
        ...

    def SourceInitFileInHomeDirectory(self, result: SBCommandReturnObject) -> None:
        ...

    def SourceInitFileInCurrentWorkingDirectory(self, result: SBCommandReturnObject) -> None:
        ...


    @overload
    def HandleCommand(self, command_line: CharConstStar , result: SBCommandReturnObject , add_to_history : bool = False) -> ReturnStatus:
        ...
    @overload
    def HandleCommand(self, command_line: CharConstStar , exe_ctx: SBExecutionContext  ,  result: SBCommandReturnObject , add_to_history : bool = False) -> ReturnStatus:
        ...

    def HandleCommandsFromFile(self, file: SBFileSpec, override_context: SBExecutionContext, options: SBCommandInterpreterRunOptions, result: SBCommandReturnObject) -> None:
        ...

    def HandleCompletion(self, current_line: CharConstStar, cursor_pos: UInt32T, match_start_point: int, max_return_elements: int, matches: SBStringList) -> int:
        ...

    def HandleCompletionWithDescriptions(self, current_line: CharConstStar, cursor_pos: UInt32T, match_start_point: int, max_return_elements: int, matches: SBStringList, descriptions: SBStringList) -> int:
        ...

    def IsActive(self) -> bool:
        ...

    def WasInterrupted(self) -> bool:
        ...


class SBCommandInterpreterRunOptions:
    r"""
    SBCommandInterpreterRunOptions controls how the RunCommandInterpreter runs the code it is fed.

    A default SBCommandInterpreterRunOptions object has:

    * StopOnContinue: false
    * StopOnError:    false
    * StopOnCrash:    false
    * EchoCommands:   true
    * PrintResults:   true
    * PrintErrors:    true
    * AddToHistory:   true


    """

    
    

    def __init__(self):
        ...
    

    def GetStopOnContinue(self) -> bool:
        ...

    def SetStopOnContinue(self, arg2: bool) -> None:
        ...

    def GetStopOnError(self) -> bool:
        ...

    def SetStopOnError(self, arg2: bool) -> None:
        ...

    def GetStopOnCrash(self) -> bool:
        ...

    def SetStopOnCrash(self, arg2: bool) -> None:
        ...

    def GetEchoCommands(self) -> bool:
        ...

    def SetEchoCommands(self, arg2: bool) -> None:
        ...

    def GetPrintResults(self) -> bool:
        ...

    def SetPrintResults(self, arg2: bool) -> None:
        ...

    def GetPrintErrors(self) -> bool:
        ...

    def SetPrintErrors(self, arg2: bool) -> None:
        ...

    def GetAddToHistory(self) -> bool:
        ...

    def SetAddToHistory(self, arg2: bool) -> None:
        ...



class SBCommandReturnObject:
    r"""
    Represents a container which holds the result from command execution.
    It works with :py:class:`SBCommandInterpreter.HandleCommand()` to encapsulate the result
    of command execution.

    See :py:class:`SBCommandInterpreter` for example usage of SBCommandReturnObject.
    """
    
    @overload
    def __init__(self):
        ...
    
    @overload
    def __init__(self , rhs: SBCommandReturnObject):
        ...
    

    def IsValid(self) -> bool:
        ...

    
    def GetOutputSize(self) -> SizeT:
        ...

    def GetErrorSize(self) -> SizeT:
        ...

    def GetOutput(self, only_id_no_immediate: bool = None) -> CharConstStar:
        ...

    def GetError(self, if_no_immediate: bool = None) -> CharConstStar:
        ...
    
    @overload
    def PutOutput(self, file:SBFile ) -> SizeT:
        ...
    @overload
    def PutOutput(self, borrowed:FileSP ) -> SizeT:
        ...
    
    @overload
    def PutError(self, file: SBFile) -> SizeT:
        ...

    @overload
    def PutError(self, borrowed: FileSP) -> SizeT:
        ...

    def Clear(self) -> None:
        ...

    def SetStatus(self, status: ReturnStatus) -> None:
        ...
    
    @overload
    def SetError(self, error_cstr: CharConstStar) -> None:
        ...

    @overload
    def SetError(self, error: SBError  , fallback_error_cstr: CharConstStar) -> None:
        ...

    def GetStatus(self) -> ReturnStatus:
        ...

    def Succeeded(self) -> bool:
        ...

    def HasResult(self) -> bool:
        ...

    def AppendMessage(self, message: CharConstStar) -> None:
        ...

    def AppendWarning(self, message: CharConstStar) -> None:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...
    
    @overload
    def SetImmediateOutputFile(self, file: SBFile) -> None:
        ...

    @overload
    def SetImmediateOutputFile(self, borrowed: FileSP , tranfer_ownership : bool = False ) -> None:
        ...
    
    @overload
    def SetImmediateErrorFile(self, file : SBFile) -> None:
        ...
    
    @overload
    def SetImmediateErrorFile(self, borrowed : FileSP , transfer_ownershop: bool = None) -> None:
        ...
        

    def PutCString(self, string: CharConstStar) -> None:
        ...

    def Print(self, str: CharConstStar) -> None:
        ...

    def write(self, str: CharConstStar) -> None:
        ...

    def flush(self) -> None:
        ...



class SBCommunication:
    r"""Allows sending/receiving data."""

    @staticmethod
    def GetBroadcasterClass() -> CharConstStar:
        ...


    @overload
    def __init__(self):
        ...
    
    @overload
    def __init__(self, broadcaster_name: CharConstStar):
        ...
    

    def IsValid(self) -> bool:
        ...

    def GetBroadcaster(self) -> SBBroadcaster:
        ...


    def AdoptFileDesriptor(self, fd: int, owns_fd: bool) -> ConnectionStatus:
        ...

    def Connect(self, url: CharConstStar) -> ConnectionStatus:
        ...

    def Disconnect(self) -> ConnectionStatus:
        ...

    def IsConnected(self) -> bool:
        ...

    def GetCloseOnEOF(self) -> bool:
        ...

    def SetCloseOnEOF(self, b: bool) -> None:
        ...

    def Read(self, dst: VoidStar, dst_len: SizeT, timeout_usec: UInt32T, status: ConnectionStatus) -> SizeT:
        ...

    def Write(self, src: VoidConstStar, src_len: SizeT, status: ConnectionStatus) -> SizeT:
        ...

    def ReadThreadStart(self) -> bool:
        ...

    def ReadThreadStop(self) -> bool:
        ...

    def ReadThreadIsRunning(self) -> bool:
        ...

    # def SetReadThreadBytesReceivedCallback(self, callback: "SBCommunication::ReadThreadBytesReceived", callback_baton: VoidStar) -> bool:
    #     ...




class SBCompileUnit:
    r"""
    Represents a compilation unit, or compiled source file.

    SBCompileUnit supports line entry iteration. For example,::

        # Now get the SBSymbolContext from this frame.  We want everything. :-)
        context = frame0.GetSymbolContext(lldb.eSymbolContextEverything)
        ...

        compileUnit = context.GetCompileUnit()

        for lineEntry in compileUnit:
            print('line entry: %s:%d' % (str(lineEntry.GetFileSpec()),
                                        lineEntry.GetLine()))
            print('start addr: %s' % str(lineEntry.GetStartAddress()))
            print('end   addr: %s' % str(lineEntry.GetEndAddress()))

    produces: ::

      line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:20
      start addr: a.out[0x100000d98]
      end   addr: a.out[0x100000da3]
      line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:21
      start addr: a.out[0x100000da3]
      end   addr: a.out[0x100000da9]
      line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:22
      start addr: a.out[0x100000da9]
      end   addr: a.out[0x100000db6]
      line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:23
      start addr: a.out[0x100000db6]
      end   addr: a.out[0x100000dbc]
      ...

    See also :py:class:`SBSymbolContext` and :py:class:`SBLineEntry`
    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self, rhs: SBCompileUnit = None):
        ...
    

    def IsValid(self) -> bool:
        ...


    def GetFileSpec(self) -> SBFileSpec:
        ...

    def GetNumLineEntries(self) -> UInt32T:
        ...

    def GetLineEntryAtIndex(self, idx: UInt32T) -> SBLineEntry:
        ...

    def FindLineEntryIndex(self, start_idx: UInt32T , line: UInt32T , inline_file_spec: SBFileSpec , exact: bool = None) -> UInt32T:
        ...

    def GetSupportFileAtIndex(self, idx: UInt32T) -> SBFileSpec:
        ...

    def GetNumSupportFiles(self) -> UInt32T:
        ...

    def FindSupportFileIndex(self, start_idx: UInt32T, sb_file: SBFileSpec, full: bool) -> UInt32T:
        ...

    def GetTypes(self, type_mask : UInt32T = None) -> SBTypeList:
        r"""
        GetTypes(SBCompileUnit self, uint32_t type_mask=eTypeClassAny) -> SBTypeList

             Get all types matching type_mask from debug info in this
             compile unit.

             @param[in] type_mask
                A bitfield that consists of one or more bits logically OR'ed
                together from the lldb::TypeClass enumeration. This allows
                you to request only structure types, or only class, struct
                and union types. Passing in lldb::eTypeClassAny will return
                all types found in the debug information for this compile
                unit.

             @return
                A list of types in this compile unit that match type_mask
        """
        ...

    def GetLanguage(self) -> LanguageType:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __eq__(self, rhs: SBCompileUnit) -> bool:
        ...

    def __ne__(self, rhs: SBCompileUnit) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBLineEntry]:
        ...

    def __len__(self) -> int:
        ...

    file = property(GetFileSpec, None, doc='''A read only property that returns the same result an lldb object that represents the source file (lldb.SBFileSpec) for the compile unit.''')
    num_line_entries = property(GetNumLineEntries, None, doc='''A read only property that returns the number of line entries in a compile unit as an integer.''')


    def __eq__(self, rhs):
        ...

    def __ne__(self, rhs):
        ...

class SBData:
    r"""Represents a data buffer."""



    @staticmethod
    def CreateDataFromCString(endian: ByteOrder, addr_byte_size: UInt32T, data: CharConstStar) -> SBData:
        ...

    @staticmethod
    def CreateDataFromUInt64Array(endian: ByteOrder, addr_byte_size: UInt32T, array: UInt64TStar) -> SBData:
        ...

    @staticmethod
    def CreateDataFromUInt32Array(endian: ByteOrder, addr_byte_size: UInt32T, array: UInt32TStar) -> SBData:
        ...

    @staticmethod
    def CreateDataFromSInt64Array(endian: ByteOrder, addr_byte_size: UInt32T, array: Int64TStar) -> SBData:
        ...

    @staticmethod
    def CreateDataFromSInt32Array(endian: ByteOrder, addr_byte_size: UInt32T, array: Int32TStar) -> SBData:
        ...

    @staticmethod
    def CreateDataFromDoubleArray(endian: ByteOrder, addr_byte_size: UInt32T, array: DoubleStar) -> SBData:
        ...

    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs : SBData):
        ...
    

    def GetAddressByteSize(self) -> UInt8T:
        ...

    def SetAddressByteSize(self, addr_byte_size: UInt8T) -> None:
        ...

    def Clear(self) -> None:
        ...

    def IsValid(self) -> bool:
        ...


    def GetByteSize(self) -> SizeT:
        ...

    def GetByteOrder(self) -> ByteOrder:
        ...

    def SetByteOrder(self, endian: ByteOrder) -> None:
        ...

    def GetFloat(self, error: SBError, offset: OffsetT) -> float:
        ...

    def GetDouble(self, error: SBError, offset: OffsetT) -> double:
        ...

    def GetLongDouble(self, error: SBError, offset: OffsetT) -> LongDouble:
        ...

    def GetAddress(self, error: SBError, offset: OffsetT) -> AddrT:
        ...

    def GetUnsignedInt8(self, error: SBError, offset: OffsetT) -> UInt8T:
        ...

    def GetUnsignedInt16(self, error: SBError, offset: OffsetT) -> UInt16T:
        ...

    def GetUnsignedInt32(self, error: SBError, offset: OffsetT) -> UInt32T:
        ...

    def GetUnsignedInt64(self, error: SBError, offset: OffsetT) -> UInt64T:
        ...

    def GetSignedInt8(self, error: SBError, offset: OffsetT) -> Int8T:
        ...

    def GetSignedInt16(self, error: SBError, offset: OffsetT) -> Int16T:
        ...

    def GetSignedInt32(self, error: SBError, offset: OffsetT) -> Int32T:
        ...

    def GetSignedInt64(self, error: SBError, offset: OffsetT) -> Int64T:
        ...

    def GetString(self, error: SBError, offset: OffsetT) -> CharConstStar:
        ...

    def GetDescription(self, description: SBStream, base_addr: AddrT) -> bool:
        ...

    def ReadRawData(self, error: SBError, offset: OffsetT, buf: VoidStar) -> SizeT:
        ...

    def SetData(self, error: SBError, buf: VoidConstStar, endian: ByteOrder, addr_size: UInt8T) -> None:
        ...

    def Append(self, rhs: SBData) -> bool:
        ...

    def SetDataFromCString(self, data: CharConstStar) -> bool:
        ...

    def SetDataFromUInt64Array(self, array: UInt64TStar) -> bool:
        ...

    def SetDataFromUInt32Array(self, array: UInt32TStar) -> bool:
        ...

    def SetDataFromSInt64Array(self, array: Int64TStar) -> bool:
        ...

    def SetDataFromSInt32Array(self, array: Int32TStar) -> bool:
        ...

    def SetDataFromDoubleArray(self, array: DoubleStar) -> bool:
        ...

    def __str__(self) -> str:
        ...


    class read_data_helper:
        def __init__(self, sbdata, readerfunc, item_size):
            self.sbdata = sbdata
            self.readerfunc = readerfunc
            self.item_size = item_size
        def __getitem__(self,key):
            if isinstance(key,slice):
                list = []
                for x in range(*key.indices(self.__len__())):
                    list.append(self.__getitem__(x))
                return list
            if not (isinstance(key,six.integer_types)):
                raise TypeError('must be int')
            key = key * self.item_size # SBData uses byte-based indexes, but we want to use itemsize-based indexes here
            error = SBError()
            my_data = self.readerfunc(self.sbdata,error,key)
            if error.Fail():
                raise IndexError(error.GetCString())
            else:
                return my_data
        def __len__(self):
            return int(self.sbdata.GetByteSize()/self.item_size)
        def all(self):
            return self[0:len(self)]

    @classmethod
    def CreateDataFromInt (cls, value, size = None, target = None, ptr_size = None, endian = None):
        import sys
        lldbmodule = sys.modules[cls.__module__]
        lldbdict = lldbmodule.__dict__
        if 'target' in lldbdict:
            lldbtarget = lldbdict['target']
        else:
            lldbtarget = None
        if target == None and lldbtarget != None and lldbtarget.IsValid():
            target = lldbtarget
        if ptr_size == None:
            if target and target.IsValid():
                ptr_size = target.addr_size
            else:
                ptr_size = 8
        if endian == None:
            if target and target.IsValid():
                endian = target.byte_order
            else:
                endian = lldbdict['eByteOrderLittle']
        if size == None:
            if value > 2147483647:
                size = 8
            elif value < -2147483648:
                size = 8
            elif value > 4294967295:
                size = 8
            else:
                size = 4
        if size == 4:
            if value < 0:
                return SBData().CreateDataFromSInt32Array(endian, ptr_size, [value])
            return SBData().CreateDataFromUInt32Array(endian, ptr_size, [value])
        if size == 8:
            if value < 0:
                return SBData().CreateDataFromSInt64Array(endian, ptr_size, [value])
            return SBData().CreateDataFromUInt64Array(endian, ptr_size, [value])
        return None

    def _make_helper(self, sbdata, getfunc, itemsize):
        return self.read_data_helper(sbdata, getfunc, itemsize)

    def _make_helper_uint8(self):
        return self._make_helper(self, SBData.GetUnsignedInt8, 1)

    def _make_helper_uint16(self):
        return self._make_helper(self, SBData.GetUnsignedInt16, 2)

    def _make_helper_uint32(self):
        return self._make_helper(self, SBData.GetUnsignedInt32, 4)

    def _make_helper_uint64(self):
        return self._make_helper(self, SBData.GetUnsignedInt64, 8)

    def _make_helper_sint8(self):
        return self._make_helper(self, SBData.GetSignedInt8, 1)

    def _make_helper_sint16(self):
        return self._make_helper(self, SBData.GetSignedInt16, 2)

    def _make_helper_sint32(self):
        return self._make_helper(self, SBData.GetSignedInt32, 4)

    def _make_helper_sint64(self):
        return self._make_helper(self, SBData.GetSignedInt64, 8)

    def _make_helper_float(self):
        return self._make_helper(self, SBData.GetFloat, 4)

    def _make_helper_double(self):
        return self._make_helper(self, SBData.GetDouble, 8)

    def _read_all_uint8(self):
        return self._make_helper_uint8().all()

    def _read_all_uint16(self):
        return self._make_helper_uint16().all()

    def _read_all_uint32(self):
        return self._make_helper_uint32().all()

    def _read_all_uint64(self):
        return self._make_helper_uint64().all()

    def _read_all_sint8(self):
        return self._make_helper_sint8().all()

    def _read_all_sint16(self):
        return self._make_helper_sint16().all()
 
    def _read_all_sint32(self):
        return self._make_helper_sint32().all()

    def _read_all_sint64(self):
        return self._make_helper_sint64().all()

    def _read_all_float(self):
        return self._make_helper_float().all()

    def _read_all_double(self):
        return self._make_helper_double().all()

    uint8 = property(_make_helper_uint8, None, doc='''A read only property that returns an array-like object out of which you can read uint8 values.''')
    uint16 = property(_make_helper_uint16, None, doc='''A read only property that returns an array-like object out of which you can read uint16 values.''')
    uint32 = property(_make_helper_uint32, None, doc='''A read only property that returns an array-like object out of which you can read uint32 values.''')
    uint64 = property(_make_helper_uint64, None, doc='''A read only property that returns an array-like object out of which you can read uint64 values.''')
    sint8 = property(_make_helper_sint8, None, doc='''A read only property that returns an array-like object out of which you can read sint8 values.''')
    sint16 = property(_make_helper_sint16, None, doc='''A read only property that returns an array-like object out of which you can read sint16 values.''')
    sint32 = property(_make_helper_sint32, None, doc='''A read only property that returns an array-like object out of which you can read sint32 values.''')
    sint64 = property(_make_helper_sint64, None, doc='''A read only property that returns an array-like object out of which you can read sint64 values.''')
    float = property(_make_helper_float, None, doc='''A read only property that returns an array-like object out of which you can read float values.''')
    double = property(_make_helper_double, None, doc='''A read only property that returns an array-like object out of which you can read double values.''')
    uint8s = property(_read_all_uint8, None, doc='''A read only property that returns an array with all the contents of this SBData represented as uint8 values.''')
    uint16s = property(_read_all_uint16, None, doc='''A read only property that returns an array with all the contents of this SBData represented as uint16 values.''')
    uint32s = property(_read_all_uint32, None, doc='''A read only property that returns an array with all the contents of this SBData represented as uint32 values.''')
    uint64s = property(_read_all_uint64, None, doc='''A read only property that returns an array with all the contents of this SBData represented as uint64 values.''')
    sint8s = property(_read_all_sint8, None, doc='''A read only property that returns an array with all the contents of this SBData represented as sint8 values.''')
    sint16s = property(_read_all_sint16, None, doc='''A read only property that returns an array with all the contents of this SBData represented as sint16 values.''')
    sint32s = property(_read_all_sint32, None, doc='''A read only property that returns an array with all the contents of this SBData represented as sint32 values.''')
    sint64s = property(_read_all_sint64, None, doc='''A read only property that returns an array with all the contents of this SBData represented as sint64 values.''')
    floats = property(_read_all_float, None, doc='''A read only property that returns an array with all the contents of this SBData represented as float values.''')
    doubles = property(_read_all_double, None, doc='''A read only property that returns an array with all the contents of this SBData represented as double values.''')
    byte_order = property(GetByteOrder, SetByteOrder, doc='''A read/write property getting and setting the endianness of this SBData (data.byte_order = lldb.eByteOrderLittle).''')
    size = property(GetByteSize, None, doc='''A read only property that returns the size the same result as GetByteSize().''')





class SBDebugger:
    r"""
    SBDebugger is the primordial object that creates SBTargets and provides
    access to them.  It also manages the overall debugging experiences.

    For example (from example/disasm.py),::

        import lldb
        import os
        import sys

        def disassemble_instructions (insts):
            for i in insts:
                print i

        ...

        # Create a new debugger instance
        debugger = lldb.SBDebugger.Create()

        # When we step or continue, don't return from the function until the process
        # stops. We do this by setting the async mode to false.
        debugger.SetAsync (False)

        # Create a target from a file and arch
        print('Creating a target for '%s'' % exe)

        target = debugger.CreateTargetWithFileAndArch (exe, lldb.LLDB_ARCH_DEFAULT)

        if target:
            # If the target is valid set a breakpoint at main
            main_bp = target.BreakpointCreateByName (fname, target.GetExecutable().GetFilename());

            print main_bp

            # Launch the process. Since we specified synchronous mode, we won't return
            # from this function until we hit the breakpoint at main
            process = target.LaunchSimple (None, None, os.getcwd())

            # Make sure the launch went ok
            if process:
                # Print some simple process info
                state = process.GetState ()
                print process
                if state == lldb.eStateStopped:
                    # Get the first thread
                    thread = process.GetThreadAtIndex (0)
                    if thread:
                        # Print some simple thread info
                        print thread
                        # Get the first frame
                        frame = thread.GetFrameAtIndex (0)
                        if frame:
                            # Print some simple frame info
                            print frame
                            function = frame.GetFunction()
                            # See if we have debug info (a function)
                            if function:
                                # We do have a function, print some info for the function
                                print function
                                # Now get all instructions for this function and print them
                                insts = function.GetInstructions(target)
                                disassemble_instructions (insts)
                            else:
                                # See if we have a symbol in the symbol table for where we stopped
                                symbol = frame.GetSymbol();
                                if symbol:
                                    # We do have a symbol, print some info for the symbol
                                    print symbol
                                    # Now get all instructions for this symbol and print them
                                    insts = symbol.GetInstructions(target)
                                    disassemble_instructions (insts)

                            registerList = frame.GetRegisters()
                            print('Frame registers (size of register set = %d):' % registerList.GetSize())
                            for value in registerList:
                                #print value
                                print('%s (number of children = %d):' % (value.GetName(), value.GetNumChildren()))
                                for child in value:
                                    print('Name: ', child.GetName(), ' Value: ', child.GetValue())

                    print('Hit the breakpoint at main, enter to continue and wait for program to exit or 'Ctrl-D'/'quit' to terminate the program')
                    next = sys.stdin.readline()
                    if not next or next.rstrip('\n') == 'quit':
                        print('Terminating the inferior process...')
                        process.Kill()
                    else:
                        # Now continue to the program exit
                        process.Continue()
                        # When we return from the above function we will hopefully be at the
                        # program exit. Print out some process info
                        print process
                elif state == lldb.eStateExited:
                    print('Didn't hit the breakpoint at main, program has exited...')
                else:
                    print('Unexpected process state: %s, killing process...' % debugger.StateAsCString (state))
                    process.Kill()

    Sometimes you need to create an empty target that will get filled in later.  The most common use for this
    is to attach to a process by name or pid where you don't know the executable up front.  The most convenient way
    to do this is: ::

        target = debugger.CreateTarget('')
        error = lldb.SBError()
        process = target.AttachToProcessWithName(debugger.GetListener(), 'PROCESS_NAME', False, error)

    or the equivalent arguments for :py:class:`SBTarget.AttachToProcessWithID` .
    """

    
    

    @staticmethod
    def Initialize() -> None:
        ...

    @staticmethod
    def InitializeWithErrorHandling() -> SBError:
        ...

    @staticmethod
    def Terminate() -> None:
        ...

    @staticmethod
    @overload
    def Create() -> SBDebugger:
        ...
    @staticmethod
    @overload
    def Create( source_init_files: bool) -> SBDebugger:
        ...

    @staticmethod
    @overload
    def Create( source_init_files: bool , log_callback: LogOutputCallback) -> SBDebugger:
        ...
    
    @staticmethod
    def Destroy(debugger: SBDebugger) -> None:
        ...

    @staticmethod
    def MemoryPressureDetected() -> None:
        ...
    @staticmethod
    def GetDefaultArchitecture(arch_name: CharStar, arch_name_len: SizeT) -> bool:
        ...

    @staticmethod
    def SetDefaultArchitecture(arch_name: CharConstStar) -> bool:
        ...

    @staticmethod
    def GetVersionString() -> CharConstStar:
        ...

    @staticmethod
    def StateAsCString(state: StateType) -> CharConstStar:
        ...

    @staticmethod
    def GetBuildConfiguration() -> SBStructuredData:
        ...

    @staticmethod
    def StateIsRunningState(state: StateType) -> bool:
        ...

    @staticmethod
    def StateIsStoppedState(state: StateType) -> bool:
        ...

    @staticmethod
    def FindDebuggerWithID(id: int) -> SBDebugger:
        ...

    @staticmethod
    def SetInternalVariable(var_name: CharConstStar, value: CharConstStar, debugger_instance_name: CharConstStar) -> SBError:
        ...

    @staticmethod
    def GetInternalVariableValue(var_name: CharConstStar, debugger_instance_name: CharConstStar) -> SBStringList:
        ...



    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBDebugger):
        ...

    def IsValid(self) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def SetAsync(self, b: bool) -> None:
        ...

    def GetAsync(self) -> bool:
        ...

    def SkipLLDBInitFiles(self, b: bool) -> None:
        ...

    def SetOutputFileHandle(self, file, transfer_ownership):
        "DEPRECATED, use SetOutputFile"
        if file is None:
            import sys
            file = sys.stdout
        self.SetOutputFile(SBFile.Create(file, borrow=True))

    def SetInputFileHandle(self, file, transfer_ownership):
        "DEPRECATED, use SetInputFile"
        if file is None:
            import sys
            file = sys.stdin
        self.SetInputFile(SBFile.Create(file, borrow=True))

    def SetErrorFileHandle(self, file, transfer_ownership):
        "DEPRECATED, use SetErrorFile"
        if file is None:
            import sys
            file = sys.stderr
        self.SetErrorFile(SBFile.Create(file, borrow=True))


    def GetInputFileHandle(self) -> FileSP:
        ...

    def GetOutputFileHandle(self) -> FileSP:
        ...

    def GetErrorFileHandle(self) -> FileSP:
        ...
    
    @overload
    def SetInputFile(self, file: SBFile) -> SBError:
        ...
    @overload
    def SetInputFile(self, file: FileSP) -> SBError:
        ...
        
    @overload
    def SetOutputFile(self, file:SBFile) -> SBError:
        ...
    @overload
    def SetOutputFile(self, file:FileSP) -> SBError:
        ...
    
    @overload
    def SetErrorFile(self, file: SBFile) -> SBError:
        ...

    @overload
    def SetErrorFile(self, file: FileSP) -> SBError:
        ...

    def GetInputFile(self) -> SBFile:
        ...

    def GetOutputFile(self) -> SBFile:
        ...

    def GetErrorFile(self) -> SBFile:
        ...

    def GetCommandInterpreter(self) -> SBCommandInterpreter:
        ...

    def HandleCommand(self, command: CharConstStar) -> None:
        ...

    def GetListener(self) -> SBListener:
        ...
    
    @overload
    def HandleProcessEvent(self, process: SBProcess, event: SBEvent, out: SBFile , err: SBFile) -> None:
        ...
    
    @overload
    def HandleProcessEvent(self, process: SBProcess, event: SBEvent, out: FileSP , err: FileSP) -> None:
        ...

    def CreateTargetWithFileAndTargetTriple(self, filename: CharConstStar, target_triple: CharConstStar) -> SBTarget:
        ...

    def CreateTargetWithFileAndArch(self, filename: CharConstStar, archname: CharConstStar) -> SBTarget:
        ...
    
    @overload
    def CreateTarget(self, filename: CharConstStar ) -> SBTarget:
        ...
    
    @overload
    def CreateTarget(self, filename: CharConstStar , target_triple: CharConstStar, platform_name: CharConstStar,  add_dependent_modules: bool, sb_error: SBError) -> SBTarget:
        ...
        
    def GetDummyTarget(self) -> SBTarget:
        r"""
        The dummy target holds breakpoints and breakpoint names that will prime newly created targets.
        """
        ...

    def DeleteTarget(self, target: SBTarget) -> bool:
        r"""
        Return true if target is deleted from the target list of the debugger.
        """
        ...

    def GetTargetAtIndex(self, idx: UInt32T) -> SBTarget:
        ...

    def GetIndexOfTarget(self, target: SBTarget) -> UInt32T:
        ...

    def FindTargetWithProcessID(self, pid: PIDT) -> SBTarget:
        ...

    def FindTargetWithFileAndArch(self, filename: CharConstStar, arch: CharConstStar) -> SBTarget:
        ...

    def GetNumTargets(self) -> UInt32T:
        ...

    def GetSelectedTarget(self) -> SBTarget:
        ...

    def SetSelectedTarget(self, target: SBTarget) -> None:
        ...

    def GetSelectedPlatform(self) -> SBPlatform:
        ...

    def SetSelectedPlatform(self, platform: SBPlatform) -> None:
        ...

    def GetNumPlatforms(self) -> UInt32T:
        r"""
        Get the number of currently active platforms.
        """
        ...


    def GetPlatformAtIndex(self, idx: UInt32T) -> SBPlatform:
        r"""
        Get one of the currently active platforms.
        """
        ...


    def GetNumAvailablePlatforms(self) -> UInt32T:
        r"""
        Get the number of available platforms.
        """
        ...

    def GetAvailablePlatformInfoAtIndex(self, idx: UInt32T) -> SBStructuredData:
        r"""
            Get the name and description of one of the available platforms.

            @param idx Zero-based index of the platform for which info should be
                       retrieved, must be less than the value returned by
                       GetNumAvailablePlatforms().
        """
        ...

    def GetSourceManager(self) -> SBSourceManager:
        ...

    def SetCurrentPlatform(self, platform_name: CharConstStar) -> SBError:
        ...

    def SetCurrentPlatformSDKRoot(self, sysroot: CharConstStar) -> bool:
        ...

    def SetUseExternalEditor(self, input: bool) -> bool:
        ...

    def GetUseExternalEditor(self) -> bool:
        ...

    def SetUseColor(self, use_color: bool) -> bool:
        ...

    def GetUseColor(self) -> bool:
        ...

    
    def GetScriptingLanguage(self, script_language_name: CharConstStar) -> ScriptLanguage:
        ...

    
    def EnableLog(self, channel: CharConstStar, types: CharConstStarStar) -> bool:
        ...

    def SetLoggingCallback(self, log_callback: LogOutputCallback) -> None:
        ...

    def DispatchInput(self, data: VoidConstStar) -> None:
        ...

    def DispatchInputInterrupt(self) -> None:
        ...

    def DispatchInputEndOfFile(self) -> None:
        ...

    def GetInstanceName(self) -> CharConstStar:
        ...


    def GetDescription(self, description: SBStream) -> bool:
        ...

    def GetTerminalWidth(self) -> UInt32T:
        ...

    def SetTerminalWidth(self, term_width: UInt32T) -> None:
        ...

    def GetID(self) -> UserIdT:
        ...

    def GetPrompt(self) -> CharConstStar:
        ...

    def SetPrompt(self, prompt: CharConstStar) -> None:
        ...

    def GetReproducerPath(self) -> CharConstStar:
        ...

    def GetScriptLanguage(self) -> ScriptLanguage:
        ...

    def SetScriptLanguage(self, script_lang: ScriptLanguage) -> None:
        ...

    def GetCloseInputOnEOF(self) -> bool:
        ...

    def SetCloseInputOnEOF(self, b: bool) -> None:
        ...
    
    @overload
    def GetCategory(self, category_name : CharConstStar) -> SBTypeCategory:
        ...
    
    @overload
    def GetCategory(self, lang_type: LanguageType) -> SBTypeCategory:
        ...

    def CreateCategory(self, category_name: CharConstStar) -> SBTypeCategory:
        ...

    def DeleteCategory(self, category_name: CharConstStar) -> bool:
        ...

    def GetNumCategories(self) -> UInt32T:
        ...

    def GetCategoryAtIndex(self, arg2: UInt32T) -> SBTypeCategory:
        ...

    def GetDefaultCategory(self) -> SBTypeCategory:
        ...

    def GetFormatForType(self, arg2: SBTypeNameSpecifier) -> SBTypeFormat:
        ...

    def GetSummaryForType(self, arg2: SBTypeNameSpecifier) -> SBTypeSummary:
        ...

    def GetFilterForType(self, arg2: SBTypeNameSpecifier) -> SBTypeFilter:
        ...

    def GetSyntheticForType(self, arg2: SBTypeNameSpecifier) -> SBTypeSynthetic:
        ...

    def __str__(self) -> str:
        ...

    def RunCommandInterpreter(self, auto_handle_events: bool, spawn_thread: bool, options: SBCommandInterpreterRunOptions, num_errors: int, quit_requested: bool, stopped_for_crash: bool) -> None:
        r"""
        RunCommandInterpreter(SBDebugger self, bool auto_handle_events, bool spawn_thread, SBCommandInterpreterRunOptions options, int & num_errors, bool & quit_requested, bool & stopped_for_crash)
        Launch a command interpreter session. Commands are read from standard input or
        from the input handle specified for the debugger object. Output/errors are
        similarly redirected to standard output/error or the configured handles.

        @param[in] auto_handle_events If true, automatically handle resulting events.
        @param[in] spawn_thread If true, start a new thread for IO handling.
        @param[in] options Parameter collection of type SBCommandInterpreterRunOptions.
        @param[in] num_errors Initial error counter.
        @param[in] quit_requested Initial quit request flag.
        @param[in] stopped_for_crash Initial crash flag.

        @return
        A tuple with the number of errors encountered by the interpreter, a boolean
        indicating whether quitting the interpreter was requested and another boolean
        set to True in case of a crash.

        Example: ::

            # Start an interactive lldb session from a script (with a valid debugger object
            # created beforehand):
            n_errors, quit_requested, has_crashed = debugger.RunCommandInterpreter(True,
                False, lldb.SBCommandInterpreterRunOptions(), 0, False, False)
        """
        ...

    def RunREPL(self, language: LanguageType, repl_options: CharConstStar) -> SBError:
        ...

    def __iter__(self) -> Iterable[SBTarget]:
        ...

    def __len__(self) -> int:
        ...

class SBDeclaration:
    r"""Specifies an association with a line and column for a variable."""

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs : SBDeclaration):
        ...    

    def IsValid(self) -> bool:
        ...

    



    def GetFileSpec(self) -> SBFileSpec:
        ...

    def GetLine(self) -> UInt32T:
        ...

    def GetColumn(self) -> UInt32T:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def SetFileSpec(self, filespec: SBFileSpec) -> None:
        ...

    def SetLine(self, line: UInt32T) -> None:
        ...

    def SetColumn(self, column: UInt32T) -> None:
        ...

    def __eq__(self, rhs: SBDeclaration) -> bool:
        ...

    def __ne__(self, rhs: SBDeclaration) -> bool:
        ...

    def __str__(self) -> str:
        ...

    file = property(GetFileSpec, None, doc='''A read only property that returns an lldb object that represents the file (lldb.SBFileSpec) for this line entry.''')
    line = property(GetLine, None, doc='''A read only property that returns the 1 based line number for this line entry, a return value of zero indicates that no line information is available.''')
    column = property(GetColumn, None, doc='''A read only property that returns the 1 based column number for this line entry, a return value of zero indicates that no column information is available.''')

class SBError:
    r"""
    Represents a container for holding any error code.

    For example (from test/python_api/hello_world/TestHelloWorld.py), ::

        def hello_world_attach_with_id_api(self):
            '''Create target, spawn a process, and attach to it by id.'''

            target = self.dbg.CreateTarget(self.exe)

            # Spawn a new process and don't display the stdout if not in TraceOn() mode.
            import subprocess
            popen = subprocess.Popen([self.exe, 'abc', 'xyz'],
                                     stdout = open(os.devnull, 'w') if not self.TraceOn() else None)

            listener = lldb.SBListener('my.attach.listener')
            error = lldb.SBError()
            process = target.AttachToProcessWithID(listener, popen.pid, error)

            self.assertTrue(error.Success() and process, PROCESS_IS_VALID)

            # Let's check the stack traces of the attached process.
            import lldbutil
            stacktraces = lldbutil.print_stacktraces(process, string_buffer=True)
            self.expect(stacktraces, exe=False,
                substrs = ['main.c:%d' % self.line2,
                           '(int)argc=3'])

            listener = lldb.SBListener('my.attach.listener')
            error = lldb.SBError()
            process = target.AttachToProcessWithID(listener, popen.pid, error)

            self.assertTrue(error.Success() and process, PROCESS_IS_VALID)

    checks that after the attach, there is no error condition by asserting
    that error.Success() is True and we get back a valid process object.

    And (from test/python_api/event/TestEvent.py), ::

            # Now launch the process, and do not stop at entry point.
            error = lldb.SBError()
            process = target.Launch(listener, None, None, None, None, None, None, 0, False, error)
            self.assertTrue(error.Success() and process, PROCESS_IS_VALID)

    checks that after calling the target.Launch() method there's no error
    condition and we get back a void process object.
    """

    
    
    @overload
    def __init__(self, ):
        ...

    
    @overload
    def __init__(self, rhs : SBError ):
        ...

    def GetCString(self) -> CharConstStar:
        ...

    def Clear(self) -> None:
        ...

    def Fail(self) -> bool:
        ...

    def Success(self) -> bool:
        ...

    def GetError(self) -> UInt32T:
        ...

    def GetType(self) -> ErrorType:
        ...

    def SetError(self, err: UInt32T, type: ErrorType) -> None:
        ...

    def SetErrorToErrno(self) -> None:
        ...

    def SetErrorToGenericError(self) -> None:
        ...

    def SetErrorString(self, err_str: CharConstStar) -> None:
        ...

    def SetErrorStringWithFormat(self, format: CharConstStar, str1: CharStar=None, str2: CharStar=None, str3: CharStar=None) -> int:
        ...

    def IsValid(self) -> bool:
        ...


    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...

    value = property(GetError, None, doc='''A read only property that returns the same result as GetError().''')
    fail = property(Fail, None, doc='''A read only property that returns the same result as Fail().''')
    success = property(Success, None, doc='''A read only property that returns the same result as Success().''')
    description = property(GetCString, None, doc='''A read only property that returns the same result as GetCString().''')
    type = property(GetType, None, doc='''A read only property that returns the same result as GetType().''')




class SBEnvironment:
    r"""
    Represents the environment of a certain process.

    Example: ::

      for entry in lldb.debugger.GetSelectedTarget().GetEnvironment().GetEntries():
        print(entry)


    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBEnvironment):
        ...

    def GetNumValues(self) -> SizeT:
        ...

    def Get(self, name: CharConstStar) -> CharConstStar:
        ...

    def GetNameAtIndex(self, index: SizeT) -> CharConstStar:
        ...

    def GetValueAtIndex(self, index: SizeT) -> CharConstStar:
        ...

    def GetEntries(self) -> SBStringList:
        ...

    def PutEntry(self, name_and_value: CharConstStar) -> None:
        ...

    def SetEntries(self, entries: SBStringList, append: bool) -> None:
        ...

    def Set(self, name: CharConstStar, value: CharConstStar, overwrite: bool) -> bool:
        ...

    def Unset(self, name: CharConstStar) -> bool:
        ...

    def Clear(self) -> None:
        ...



class SBEvent:
    r"""
    API clients can register to receive events.

    For example, check out the following output: ::

        Try wait for event...
        Event description: 0x103d0bb70 Event: broadcaster = 0x1009c8410, type = 0x00000001, data = { process = 0x1009c8400 (pid = 21528), state = running}
        Event data flavor: Process::ProcessEventData
        Process state: running

        Try wait for event...
        Event description: 0x103a700a0 Event: broadcaster = 0x1009c8410, type = 0x00000001, data = { process = 0x1009c8400 (pid = 21528), state = stopped}
        Event data flavor: Process::ProcessEventData
        Process state: stopped

        Try wait for event...
        Event description: 0x103d0d4a0 Event: broadcaster = 0x1009c8410, type = 0x00000001, data = { process = 0x1009c8400 (pid = 21528), state = exited}
        Event data flavor: Process::ProcessEventData
        Process state: exited

        Try wait for event...
        timeout occurred waiting for event...

    from test/python_api/event/TestEventspy: ::

        def do_listen_for_and_print_event(self):
            '''Create a listener and use SBEvent API to print the events received.'''
            exe = os.path.join(os.getcwd(), 'a.out')

            # Create a target by the debugger.
            target = self.dbg.CreateTarget(exe)
            self.assertTrue(target, VALID_TARGET)

            # Now create a breakpoint on main.c by name 'c'.
            breakpoint = target.BreakpointCreateByName('c', 'a.out')

            # Now launch the process, and do not stop at the entry point.
            process = target.LaunchSimple(None, None, os.getcwd())
            self.assertTrue(process.GetState() == lldb.eStateStopped,
                            PROCESS_STOPPED)

            # Get a handle on the process's broadcaster.
            broadcaster = process.GetBroadcaster()

            # Create an empty event object.
            event = lldb.SBEvent()

            # Create a listener object and register with the broadcaster.
            listener = lldb.SBListener('my listener')
            rc = broadcaster.AddListener(listener, lldb.SBProcess.eBroadcastBitStateChanged)
            self.assertTrue(rc, 'AddListener successfully retruns')

            traceOn = self.TraceOn()
            if traceOn:
                lldbutil.print_stacktraces(process)

            # Create MyListeningThread class to wait for any kind of event.
            import threading
            class MyListeningThread(threading.Thread):
                def run(self):
                    count = 0
                    # Let's only try at most 4 times to retrieve any kind of event.
                    # After that, the thread exits.
                    while not count > 3:
                        if traceOn:
                            print('Try wait for event...')
                        if listener.WaitForEventForBroadcasterWithType(5,
                                                                       broadcaster,
                                                                       lldb.SBProcess.eBroadcastBitStateChanged,
                                                                       event):
                            if traceOn:
                                desc = lldbutil.get_description(event))
                                print('Event description:', desc)
                                print('Event data flavor:', event.GetDataFlavor())
                                print('Process state:', lldbutil.state_type_to_str(process.GetState()))
                                print()
                        else:
                            if traceOn:
                                print 'timeout occurred waiting for event...'
                        count = count + 1
                    return

            # Let's start the listening thread to retrieve the events.
            my_thread = MyListeningThread()
            my_thread.start()

            # Use Python API to continue the process.  The listening thread should be
            # able to receive the state changed events.
            process.Continue()

            # Use Python API to kill the process.  The listening thread should be
            # able to receive the state changed event, too.
            process.Kill()

            # Wait until the 'MyListeningThread' terminates.
            my_thread.join()
    """

    
    @staticmethod
    def GetCStringFromEvent(event: SBEvent) -> CharConstStar:
        ...

    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBEvent):
        ...
    

    def IsValid(self) -> bool:
        ...


    def GetDataFlavor(self) -> CharConstStar:
        ...

    def GetType(self) -> UInt32T:
        ...

    def GetBroadcaster(self) -> SBBroadcaster:
        ...

    def GetBroadcasterClass(self) -> CharConstStar:
        ...

    def BroadcasterMatchesRef(self, broadcaster: SBBroadcaster) -> bool:
        ...

    def Clear(self) -> None:
        ...


    def GetDescription(self, description: SBStream) -> bool:
        ...

class SBExecutionContext:
    r"""Describes the program context in which a command should be executed."""

    
    
    @overload
    def __init__(self, rhs : SBExecutionContext):
        ...

    @overload
    def __init__(self, target : SBTarget):
        ...

    @overload
    def __init__(self, process : SBProcess):
        ...

    @overload
    def __init__(self, thread : SBThread):
        ...
    @overload
    def __init__(self, frame : SBFrame):
        ...


    def GetTarget(self) -> SBTarget:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetThread(self) -> SBThread:
        ...

    def GetFrame(self) -> SBFrame:
        ...

    target = property(GetTarget, None, doc='''A read only property that returns the same result as GetTarget().''')
    process = property(GetProcess, None, doc='''A read only property that returns the same result as GetProcess().''')
    thread = property(GetThread, None, doc='''A read only property that returns the same result as GetThread().''')
    frame = property(GetFrame, None, doc='''A read only property that returns the same result as GetFrame().''')

class SBExpressionOptions:
    r"""A container for options to use when evaluating expressions."""

    
    
    @overload
    def __init__(self):
        ...
    
    @overload
    def __init__(self , rhs  : SBExpressionOptions):
        ...
        
    def GetCoerceResultToId(self) -> bool:
        ...

    def SetCoerceResultToId(self, coerce: bool=True) -> None:
        r"""
        Sets whether to coerce the expression result to ObjC id type after evaluation.
        """
        ...


    def GetUnwindOnError(self) -> bool:
        ...

    def SetUnwindOnError(self, unwind: bool=True) -> None:
        r"""
        Sets whether to unwind the expression stack on error.
        """
        ...

    def GetIgnoreBreakpoints(self) -> bool:
        ...

    def SetIgnoreBreakpoints(self, ignore: bool=True) -> None:
        ...

    def GetFetchDynamicValue(self) -> DynamicValueType:
        ...

    def SetFetchDynamicValue(self, dynamic : DynamicValueType = None) -> None:
        r"""
        Sets whether to cast the expression result to its dynamic type.
        """
        ...

    def GetTimeoutInMicroSeconds(self) -> UInt32T:
        ...

    def SetTimeoutInMicroSeconds(self, timeout: UInt32T=0) -> None:
        r"""
        Sets the timeout in microseconds to run the expression for. If try all threads is set to true and the expression doesn't complete within the specified timeout, all threads will be resumed for the same timeout to see if the expression will finish.
        """
        ...

    def GetOneThreadTimeoutInMicroSeconds(self) -> UInt32T:
        ...

    def SetOneThreadTimeoutInMicroSeconds(self, timeout: UInt32T=0) -> None:
        ...

    def GetTryAllThreads(self) -> bool:
        ...

    def SetTryAllThreads(self, run_others: bool=True) -> None:
        r"""
        Sets whether to run all threads if the expression does not complete on one thread.
        """
        ...

    def GetStopOthers(self) -> bool:
        ...

    def SetStopOthers(self, stop_others: bool=True) -> None:
        ...

    def GetTrapExceptions(self) -> bool:
        ...

    def SetTrapExceptions(self, trap_exceptions: bool=True) -> None:
        ...

    def SetLanguage(self, language: LanguageType) -> None:
        r"""
        Sets the language that LLDB should assume the expression is written in
        """
        ...

    def GetGenerateDebugInfo(self) -> bool:
        ...

    def SetGenerateDebugInfo(self, b: bool=True) -> None:
        r"""
        Sets whether to generate debug information for the expression and also controls if a SBModule is generated.
        """
        ...

    def GetSuppressPersistentResult(self) -> bool:
        ...

    def SetSuppressPersistentResult(self, b: bool=False) -> None:
        r"""
        Sets whether to produce a persistent result that can be used in future expressions.
        """
        ...

    def GetPrefix(self) -> CharConstStar:
        r"""
        Gets the prefix to use for this expression.
        """
        ...

    def SetPrefix(self, prefix: CharConstStar) -> None:
        r"""
        Sets the prefix to use for this expression. This prefix gets inserted after the 'target.expr-prefix' prefix contents, but before the wrapped expression function body.
        """
        ...

    def SetAutoApplyFixIts(self, b: bool=True) -> None:
        r"""
        Sets whether to auto-apply fix-it hints to the expression being evaluated.
        """
        

    def GetAutoApplyFixIts(self) -> bool:
        r"""
        Gets whether to auto-apply fix-it hints to an expression.
        """
        ...

    def SetRetriesWithFixIts(self, retries: UInt64T) -> None:
        r"""
        Sets how often LLDB should retry applying fix-its to an expression.
        """
        ...

    def GetRetriesWithFixIts(self) -> UInt64T:
        r"""
        Gets how often LLDB will retry applying fix-its to an expression.
        """
        ...

    def GetTopLevel(self) -> bool:
        ...

    def SetTopLevel(self, b: bool=True) -> None:
        ...

    def GetAllowJIT(self) -> bool:
        r"""
        Gets whether to JIT an expression if it cannot be interpreted.
        """
        ...

    def SetAllowJIT(self, allow: bool) -> None:
        r"""
        Sets whether to JIT an expression if it cannot be interpreted.
        """
        ...



class SBFile:
    r"""Represents a file."""

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , fd: int , mode: CharConstStar , transfer_ownership: bool):
        ...
    @overload
    def __init__(self , file: FileSP):
        ...

    def __init__(self, *args):
        """
        initialize a SBFile from a python file object
        """
        ...

    @staticmethod
    def MakeBorrowed(BORROWED: FileSP) -> SBFile:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    @staticmethod
    def MakeForcingIOMethods(FORCE_IO_METHODS: FileSP) -> SBFile:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    @staticmethod
    def MakeBorrowedForcingIOMethods(BORROWED_FORCE_IO_METHODS: FileSP) -> SBFile:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    @classmethod
    def Create(cls, file, borrow=False, force_io_methods=False) -> SBFile:
        """
        Create a SBFile from a python file object, with options.

        If borrow is set then the underlying file will
        not be closed when the SBFile is closed or destroyed.

        If force_scripting_io is set then the python read/write
        methods will be called even if a file descriptor is available.
        """
        ...
    

    def Read(self, buf: UInt8TStar) -> Tuple[SBError , int]:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    def Write(self, buf: UInt8ConstStar) -> SBError:
        r"""
        Write(buffer) -> SBError, written_read
        initialize a SBFile from a python file object
        """
        ...

    def Flush(self) -> None:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    def IsValid(self) -> bool:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    def Close(self) -> SBError:
        r"""
        initialize a SBFile from a python file object
        """
        ...

    def GetFile(self) -> FileSP:
        r"""
            Convert this SBFile into a python io.IOBase file object.

            If the SBFile is itself a wrapper around a python file object,
            this will return that original object.

            The file returned from here should be considered borrowed,
            in the sense that you may read and write to it, and flush it,
            etc, but you should not close it.   If you want to close the
            SBFile, call SBFile.Close().

            If there is no underlying python file to unwrap, GetFile will
            use the file descriptor, if available to create a new python
            file object using ``open(fd, mode=..., closefd=False)``

        """
        ...



class SBFileSpec:
    r"""
    Represents a file specification that divides the path into a directory and
    basename.  The string values of the paths are put into uniqued string pools
    for fast comparisons and efficient memory usage.

    For example, the following code ::

            lineEntry = context.GetLineEntry()
            self.expect(lineEntry.GetFileSpec().GetDirectory(), 'The line entry should have the correct directory',
                        exe=False,
                substrs = [self.mydir])
            self.expect(lineEntry.GetFileSpec().GetFilename(), 'The line entry should have the correct filename',
                        exe=False,
                substrs = ['main.c'])
            self.assertTrue(lineEntry.GetLine() == self.line,
                            'The line entry's line number should match ')

    gets the line entry from the symbol context when a thread is stopped.
    It gets the file spec corresponding to the line entry and checks that
    the filename and the directory matches what we expect.
    """

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs : SBFileSpec):
        ...
    @overload
    def __init__(self , path : CharConstStar):
        ...
    @overload
    def __init__(self, path: CharConstStar , resolve: bool):
        ...
    
    def __eq__(self, rhs: SBFileSpec) -> bool:
        ...

    def __ne__(self, rhs: SBFileSpec) -> bool:
        ...

    def IsValid(self) -> bool:
        ...


    def Exists(self) -> bool:
        ...

    def ResolveExecutableLocation(self) -> bool:
        ...

    def GetFilename(self) -> CharConstStar:
        ...

    def GetDirectory(self) -> CharConstStar:
        ...

    def SetFilename(self, filename: CharConstStar) -> None:
        ...

    def SetDirectory(self, directory: CharConstStar) -> None:
        ...

    def GetPath(self, dst_path: CharStar, dst_len: SizeT) -> UInt32T:
        ...

    @staticmethod
    def ResolvePath(src_path: CharConstStar, dst_path: CharStar, dst_len: SizeT) -> int:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def AppendPathComponent(self, file_or_directory: CharConstStar) -> None:
        ...

    def __str__(self) -> str:
        ...

    def __get_fullpath__(self):
        spec_dir = self.GetDirectory()
        spec_file = self.GetFilename()
        if spec_dir and spec_file:
            return '%s/%s' % (spec_dir, spec_file)
        elif spec_dir:
            return spec_dir
        elif spec_file:
            return spec_file
        return None

    fullpath = property(__get_fullpath__, None, doc='''A read only property that returns the fullpath as a python string.''')
    basename = property(GetFilename, None, doc='''A read only property that returns the path basename as a python string.''')
    dirname = property(GetDirectory, None, doc='''A read only property that returns the path directory name as a python string.''')
    exists = property(Exists, None, doc='''A read only property that returns a boolean value that indicates if the file exists.''')



class SBFileSpecList:
    r"""Represents a list of :py:class:`SBFileSpec`."""
    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBFileSpecList):
        ...

    def GetSize(self) -> UInt32T:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def Append(self, sb_file: SBFileSpec) -> None:
        ...

    def AppendIfUnique(self, sb_file: SBFileSpec) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def FindFileIndex(self, idx: UInt32T, sb_file: SBFileSpec, full: bool) -> UInt32T:
        ...

    def GetFileSpecAtIndex(self, idx: UInt32T) -> SBFileSpec:
        ...



class SBFrame:
    r"""
    Represents one of the stack frames associated with a thread.

    SBThread contains SBFrame(s). For example (from test/lldbutil.py), ::

        def print_stacktrace(thread, string_buffer = False):
            '''Prints a simple stack trace of this thread.'''

            ...

            for i in range(depth):
                frame = thread.GetFrameAtIndex(i)
                function = frame.GetFunction()

                load_addr = addrs[i].GetLoadAddress(target)
                if not function:
                    file_addr = addrs[i].GetFileAddress()
                    start_addr = frame.GetSymbol().GetStartAddress().GetFileAddress()
                    symbol_offset = file_addr - start_addr
                    print >> output, '  frame #{num}: {addr:#016x} {mod}`{symbol} + {offset}'.format(
                        num=i, addr=load_addr, mod=mods[i], symbol=symbols[i], offset=symbol_offset)
                else:
                    print >> output, '  frame #{num}: {addr:#016x} {mod}`{func} at {file}:{line} {args}'.format(
                        num=i, addr=load_addr, mod=mods[i],
                        func='%s [inlined]' % funcs[i] if frame.IsInlined() else funcs[i],
                        file=files[i], line=lines[i],
                        args=get_args_as_string(frame, showFuncName=False) if not frame.IsInlined() else '()')

            ...

    And, ::

        for frame in thread:
            print frame

    See also SBThread.
    """

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBFrame):
        ...

    def IsEqual(self, rhs: SBFrame) -> bool:
        ...

    def IsValid(self) -> bool:
        ...


    def GetFrameID(self) -> UInt32T:
        ...

    def GetCFA(self) -> AddrT:
        r"""
            Get the Canonical Frame Address for this stack frame.
            This is the DWARF standard's definition of a CFA, a stack address
            that remains constant throughout the lifetime of the function.
            Returns an lldb::addr_t stack address, or LLDB_INVALID_ADDRESS if
            the CFA cannot be determined.
        """
        ...

    def GetPC(self) -> AddrT:
        ...

    def SetPC(self, new_pc: AddrT) -> bool:
        ...

    def GetSP(self) -> AddrT:
        ...

    def GetFP(self) -> AddrT:
        ...

    def GetPCAddress(self) -> SBAddress:
        ...

    def GetSymbolContext(self, resolve_scope: UInt32T) -> SBSymbolContext:
        ...

    def GetModule(self) -> SBModule:
        ...

    def GetCompileUnit(self) -> SBCompileUnit:
        ...

    def GetFunction(self) -> SBFunction:
        ...

    def GetSymbol(self) -> SBSymbol:
        ...

    def GetBlock(self) -> SBBlock:
        r"""
            Gets the deepest block that contains the frame PC.

            See also GetFrameBlock().
        """
        ...

    def GetDisplayFunctionName(self) -> CharConstStar:
        ...

    def GetFunctionName(self, *args) -> Union[CharConstStar , CharConst]:
        r"""
            Get the appropriate function name for this frame. Inlined functions in
            LLDB are represented by Blocks that have inlined function information, so
            just looking at the SBFunction or SBSymbol for a frame isn't enough.
            This function will return the appropriate function, symbol or inlined
            function name for the frame.

            This function returns:
            - the name of the inlined function (if there is one)
            - the name of the concrete function (if there is one)
            - the name of the symbol (if there is one)
            - NULL

            See also IsInlined().
        """
        ...

    def GuessLanguage(self) -> LanguageType:
        r"""
            Returns the language of the frame's SBFunction, or if there.
            is no SBFunction, guess the language from the mangled name.
        """
        ...

    def IsInlined(self) -> bool:
        r"""
            Return true if this frame represents an inlined function.

            See also GetFunctionName().
        """
        ...

    def IsArtificial(self) -> bool:
        r"""
            Return true if this frame is artificial (e.g a frame synthesized to
            capture a tail call). Local variables may not be available in an artificial
            frame.
        """
        ...
    
    @overload
    def EvaluateExpression(self, expr: CharConstStar) -> SBValue:
        ...
    
    @overload
    def EvaluateExpression(self, expr: CharConstStar , use_dynamic: DynamicValueType , unwind_on_error : bool = None) -> SBValue:
        ...

    @overload
    def EvaluateExpression(self, expr: CharConstStar , options: SBExpressionOptions) -> SBValue:
        ...

    def GetFrameBlock(self) -> SBBlock:
        r"""
            Gets the lexical block that defines the stack frame. Another way to think
            of this is it will return the block that contains all of the variables
            for a stack frame. Inlined functions are represented as SBBlock objects
            that have inlined function information: the name of the inlined function,
            where it was called from. The block that is returned will be the first
            block at or above the block for the PC (SBFrame::GetBlock()) that defines
            the scope of the frame. When a function contains no inlined functions,
            this will be the top most lexical block that defines the function.
            When a function has inlined functions and the PC is currently
            in one of those inlined functions, this method will return the inlined
            block that defines this frame. If the PC isn't currently in an inlined
            function, the lexical block that defines the function is returned.
        """
        ...

    def GetLineEntry(self) -> SBLineEntry:
        ...

    def GetThread(self) -> SBThread:
        ...

    def Disassemble(self) -> CharConstStar:
        ...

    def Clear(self) -> None:
        ...

    def __eq__(self, rhs: SBFrame) -> bool:
        ...

    def __ne__(self, rhs: SBFrame) -> bool:
        ...
    
    @overload
    def GetVariables(self, arguments: bool, locals: bool, statics: bool, in_cope_only: bool , use_dynamic: DynamicValueType = None) -> SBValueList:
        ...
    @overload
    def GetVariables(self, options: SBVariablesOptions) -> SBValueList:
        ...

    def GetRegisters(self) -> SBValueList:
        ...

    def FindVariable(self, var_name: CharConstStar , use_dynamic : DynamicValueType = None) -> SBValue:
        r"""
            The version that doesn't supply a 'use_dynamic' value will use the
            target's default.
        """
        ...

    def FindRegister(self, name: CharConstStar) -> SBValue:
        ...

    def GetValueForVariablePath(self, var_path: CharConstStar, use_dynamic: DynamicValueType) -> SBValue:
        r"""
            Get a lldb.SBValue for a variable path.

            Variable paths can include access to pointer or instance members: ::

                rect_ptr->origin.y
                pt.x

            Pointer dereferences: ::

                *this->foo_ptr
                **argv

            Address of: ::

                &pt
                &my_array[3].x

            Array accesses and treating pointers as arrays: ::

                int_array[1]
                pt_ptr[22].x

            Unlike `EvaluateExpression()` which returns :py:class:`SBValue` objects
            with constant copies of the values at the time of evaluation,
            the result of this function is a value that will continue to
            track the current value of the value as execution progresses
            in the current frame.
        """
        ...
    
    def FindValue(self, name: CharConstStar , value_type: ValueType , use_dynamic: DynamicValueType = None) -> SBValue:
        r"""
            Find variables, register sets, registers, or persistent variables using
            the frame as the scope.

            The version that doesn't supply a ``use_dynamic`` value will use the
            target's default.
        """
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def get_all_variables(self) -> SBValueList:
        return self.GetVariables(True,True,True,True)

    def get_parent_frame(self) -> SBFrame:
        parent_idx = self.idx + 1
        if parent_idx >= 0 and parent_idx < len(self.thread.frame):
            return self.thread.frame[parent_idx]
        else:
            return SBFrame()

    def get_arguments(self) -> SBValueList:
        return self.GetVariables(True,False,False,False)

    def get_locals(self) -> SBValueList:
        return self.GetVariables(False,True,False,False)

    def get_statics(self) -> SBValueList:
        return self.GetVariables(False,False,True,False)

    def var(self, var_expr_path) -> SBValue:
        '''Calls through to lldb.SBFrame.GetValueForVariablePath() and returns
        a value that represents the variable expression path'''
        return self.GetValueForVariablePath(var_expr_path)

    def get_registers_access(self):
        class registers_access:
            '''A helper object that exposes a flattened view of registers, masking away the notion of register sets for easy scripting.'''
            def __getitem__(self, key) -> SBValue:
                ...

        return registers_access(self.registers)

    pc = property(GetPC, SetPC)
    addr = property(GetPCAddress, None, doc='''A read only property that returns the program counter (PC) as a section offset address (lldb.SBAddress).''')
    fp = property(GetFP, None, doc='''A read only property that returns the frame pointer (FP) as an unsigned integer.''')
    sp = property(GetSP, None, doc='''A read only property that returns the stack pointer (SP) as an unsigned integer.''')
    module = property(GetModule, None, doc='''A read only property that returns an lldb object that represents the module (lldb.SBModule) for this stack frame.''')
    compile_unit = property(GetCompileUnit, None, doc='''A read only property that returns an lldb object that represents the compile unit (lldb.SBCompileUnit) for this stack frame.''')
    function = property(GetFunction, None, doc='''A read only property that returns an lldb object that represents the function (lldb.SBFunction) for this stack frame.''')
    symbol = property(GetSymbol, None, doc='''A read only property that returns an lldb object that represents the symbol (lldb.SBSymbol) for this stack frame.''')
    block = property(GetBlock, None, doc='''A read only property that returns an lldb object that represents the block (lldb.SBBlock) for this stack frame.''')
    is_inlined = property(IsInlined, None, doc='''A read only property that returns an boolean that indicates if the block frame is an inlined function.''')
    name = property(GetFunctionName, None, doc='''A read only property that retuns the name for the function that this frame represents. Inlined stack frame might have a concrete function that differs from the name of the inlined function (a named lldb.SBBlock).''')
    line_entry = property(GetLineEntry, None, doc='''A read only property that returns an lldb object that represents the line table entry (lldb.SBLineEntry) for this stack frame.''')
    thread = property(GetThread, None, doc='''A read only property that returns an lldb object that represents the thread (lldb.SBThread) for this stack frame.''')
    disassembly = property(Disassemble, None, doc='''A read only property that returns the disassembly for this stack frame as a python string.''')
    idx = property(GetFrameID, None, doc='''A read only property that returns the zero based stack frame index.''')
    variables = property(get_all_variables, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the variables in this stack frame.''')
    vars = property(get_all_variables, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the variables in this stack frame.''')
    locals = property(get_locals, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the local variables in this stack frame.''')
    args = property(get_arguments, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the argument variables in this stack frame.''')
    arguments = property(get_arguments, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the argument variables in this stack frame.''')
    statics = property(get_statics, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the static variables in this stack frame.''')
    registers = property(GetRegisters, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the CPU registers for this stack frame.''')
    regs = property(GetRegisters, None, doc='''A read only property that returns a list() that contains a collection of lldb.SBValue objects that represent the CPU registers for this stack frame.''')
    register = property(get_registers_access, None, doc='''A read only property that returns an helper object providing a flattened indexable view of the CPU registers for this stack frame.''')
    reg = property(get_registers_access, None, doc='''A read only property that returns an helper object providing a flattened indexable view of the CPU registers for this stack frame''')
    parent = property(get_parent_frame, None, doc='''A read only property that returns the parent (caller) frame of the current frame.''')

class SBFunction:
    r"""
    Represents a generic function, which can be inlined or not.

    For example (from test/lldbutil.py, but slightly modified for doc purpose),::

            ...

            frame = thread.GetFrameAtIndex(i)
            addr = frame.GetPCAddress()
            load_addr = addr.GetLoadAddress(target)
            function = frame.GetFunction()
            mod_name = frame.GetModule().GetFileSpec().GetFilename()

            if not function:
                # No debug info for 'function'.
                symbol = frame.GetSymbol()
                file_addr = addr.GetFileAddress()
                start_addr = symbol.GetStartAddress().GetFileAddress()
                symbol_name = symbol.GetName()
                symbol_offset = file_addr - start_addr
                print >> output, '  frame #{num}: {addr:#016x} {mod}`{symbol} + {offset}'.format(
                    num=i, addr=load_addr, mod=mod_name, symbol=symbol_name, offset=symbol_offset)
            else:
                # Debug info is available for 'function'.
                func_name = frame.GetFunctionName()
                file_name = frame.GetLineEntry().GetFileSpec().GetFilename()
                line_num = frame.GetLineEntry().GetLine()
                print >> output, '  frame #{num}: {addr:#016x} {mod}`{func} at {file}:{line} {args}'.format(
                    num=i, addr=load_addr, mod=mod_name,
                    func='%s [inlined]' % func_name] if frame.IsInlined() else func_name,
                    file=file_name, line=line_num, args=get_args_as_string(frame, showFuncName=False))

            ...
    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBFunction):
        ...


    def IsValid(self) -> bool:
        ...


    def GetName(self) -> CharConstStar:
        ...

    def GetDisplayName(self) -> CharConstStar:
        ...

    def GetMangledName(self) -> CharConstStar:
        ...

    def GetInstructions(self, target: SBTarget , flavor : CharConstStar = None) -> SBInstructionList:
        ...

    def GetStartAddress(self) -> SBAddress:
        ...

    def GetEndAddress(self) -> SBAddress:
        ...

    def GetArgumentName(self, arg_idx: UInt32T) -> CharConstStar:
        ...

    def GetPrologueByteSize(self) -> UInt32T:
        ...

    def GetType(self) -> SBType:
        ...

    def GetBlock(self) -> SBBlock:
        ...

    def GetLanguage(self) -> LanguageType:
        ...

    def GetIsOptimized(self) -> bool:
        r"""
            Returns true if the function was compiled with optimization.
            Optimization, in this case, is meant to indicate that the debugger
            experience may be confusing for the user -- variables optimized away,
            stepping jumping between source lines -- and the driver may want to
            provide some guidance to the user about this.
            Returns false if unoptimized, or unknown.
        """
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __eq__(self, rhs: SBFunction) -> bool:
        ...

    def __ne__(self, rhs: SBFunction) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def get_instructions_from_current_target (self) -> SBInstructionList:
        ...

    addr = property(GetStartAddress, None, doc='''A read only property that returns an lldb object that represents the start address (lldb.SBAddress) for this function.''')
    end_addr = property(GetEndAddress, None, doc='''A read only property that returns an lldb object that represents the end address (lldb.SBAddress) for this function.''')
    block = property(GetBlock, None, doc='''A read only property that returns an lldb object that represents the top level lexical block (lldb.SBBlock) for this function.''')
    instructions = property(get_instructions_from_current_target, None, doc='''A read only property that returns an lldb object that represents the instructions (lldb.SBInstructionList) for this function.''')
    mangled = property(GetMangledName, None, doc='''A read only property that returns the mangled (linkage) name for this function as a string.''')
    name = property(GetName, None, doc='''A read only property that returns the name for this function as a string.''')
    prologue_size = property(GetPrologueByteSize, None, doc='''A read only property that returns the size in bytes of the prologue instructions as an unsigned integer.''')
    type = property(GetType, None, doc='''A read only property that returns an lldb object that represents the return type (lldb.SBType) for this function.''')


class SBHostOS:
    r"""Provides information about the host system."""

    @staticmethod
    def GetProgramFileSpec() -> SBFileSpec:
        ...

    @staticmethod
    def GetLLDBPythonPath() -> SBFileSpec:
        ...

    @staticmethod
    def GetLLDBPath(path_type: PathType) -> SBFileSpec:
        ...

    @staticmethod
    def GetUserHomeDirectory() -> SBFileSpec:
        ...

    @staticmethod
    def ThreadCreated(name: CharConstStar) -> None:
        ...

    @staticmethod
    def ThreadCreate(name: CharConstStar, arg2: ThreadFuncT, thread_arg: VoidStar, err: SBError) -> ThreadT:
        ...

    @staticmethod
    def ThreadCancel(thread: ThreadT, err: SBError) -> bool:
        ...

    @staticmethod
    def ThreadDetach(thread: ThreadT, err: SBError) -> bool:
        ...

    @staticmethod
    def ThreadJoin(thread: ThreadT, result: ThreadResultT, err: SBError) -> bool:
        ...
    
    def __init__(self):
        ...
    



class SBInstruction:
    r"""Represents a (machine language) instruction."""
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBInstruction):
        ...

    def IsValid(self) -> bool:
        ...
    
    def GetAddress(self) -> SBAddress:
        ...

    def GetMnemonic(self, target: SBTarget) -> CharConstStar:
        ...

    def GetOperands(self, target: SBTarget) -> CharConstStar:
        ...

    def GetComment(self, target: SBTarget) -> CharConstStar:
        ...

    def GetData(self, target: SBTarget) -> SBData:
        ...

    def GetByteSize(self) -> SizeT:
        ...

    def DoesBranch(self) -> bool:
        ...

    def HasDelaySlot(self) -> bool:
        ...

    def CanSetBreakpoint(self) -> bool:
        ...
    
    @overload
    def Print(self, out: SBFile) -> None:
        ...
    @overload
    def Print(self, borrowed: FileSP) -> None:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def EmulateWithFrame(self, frame: SBFrame, evaluate_options: UInt32T) -> bool:
        ...

    def DumpEmulation(self, triple: CharConstStar) -> bool:
        ...

    def TestEmulation(self, output_stream: SBStream, test_file: CharConstStar) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def __mnemonic_property__ (self) -> CharConstStar:
        ...
    def __operands_property__ (self) -> CharConstStar:
        ...
    def __comment_property__ (self):
        ...
    def __file_addr_property__ (self):
        ...
    def __load_adrr_property__ (self):
        ...

    mnemonic = property(__mnemonic_property__, None, doc='''A read only property that returns the mnemonic for this instruction as a string.''')
    operands = property(__operands_property__, None, doc='''A read only property that returns the operands for this instruction as a string.''')
    comment = property(__comment_property__, None, doc='''A read only property that returns the comment for this instruction as a string.''')
    addr = property(GetAddress, None, doc='''A read only property that returns an lldb object that represents the address (lldb.SBAddress) for this instruction.''')
    size = property(GetByteSize, None, doc='''A read only property that returns the size in bytes for this instruction as an integer.''')
    is_branch = property(DoesBranch, None, doc='''A read only property that returns a boolean value that indicates if this instruction is a branch instruction.''')




class SBInstructionList:
    r"""
    Represents a list of machine instructions.  SBFunction and SBSymbol have
    GetInstructions() methods which return SBInstructionList instances.

    SBInstructionList supports instruction (:py:class:`SBInstruction` instance) iteration.
    For example (see also :py:class:`SBDebugger` for a more complete example), ::

        def disassemble_instructions (insts):
            for i in insts:
                print i

    defines a function which takes an SBInstructionList instance and prints out
    the machine instructions in assembly format.
    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBInstructionList):
        ...
        
    

    def IsValid(self) -> bool:
        ...

    def GetSize(self) -> SizeT:
        ...

    def GetInstructionAtIndex(self, idx: UInt32T) -> SBInstruction:
        ...

    def GetInstructionsCount(self, start: SBAddress, end: SBAddress, canSetBreakpoint: bool) -> SizeT:
        ...

    def Clear(self) -> None:
        ...

    def AppendInstruction(self, inst: SBInstruction) -> None:
        ...

    @overload
    def Print(self, out: SBFile) -> None:
        ...
    @overload
    def Print(self, borrowed: FileSP) -> None:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def DumpEmulationForAllInstructions(self, triple: CharConstStar) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBInstruction]:
        ...

    def __len__(self) -> int: 
        ...

    def __getitem__(self, key) -> Optional[SBInstruction]: 
        ...

class SBLanguageRuntime:
    r"""Utility functions for :ref:`LanguageType`"""

    @staticmethod
    def GetLanguageTypeFromString(string: CharConstStar) -> LanguageType:
        ...

    @staticmethod
    def GetNameForLanguageType(language: LanguageType) -> CharConstStar:
        ...

    def __init__(self):
        ...
    


class SBLaunchInfo:
    r"""Describes how a target or program should be launched."""

    def __init__(self, argv: CharConstStarStar):
        ...

    def GetProcessID(self) -> PIDT:
        ...

    def GetUserID(self) -> UInt32T:
        ...

    def GetGroupID(self) -> UInt32T:
        ...

    def UserIDIsValid(self) -> bool:
        ...

    def GroupIDIsValid(self) -> bool:
        ...

    def SetUserID(self, uid: UInt32T) -> None:
        ...

    def SetGroupID(self, gid: UInt32T) -> None:
        ...

    def GetExecutableFile(self) -> SBFileSpec:
        ...

    def SetExecutableFile(self, exe_file: SBFileSpec, add_as_first_arg: bool) -> None:
        ...

    def GetListener(self) -> SBListener:
        ...

    def SetListener(self, listener: SBListener) -> None:
        ...

    def GetNumArguments(self) -> UInt32T:
        ...

    def GetArgumentAtIndex(self, idx: UInt32T) -> CharConstStar:
        ...

    def SetArguments(self, argv: CharConstStarStar, append: bool) -> None:
        ...

    def GetNumEnvironmentEntries(self) -> UInt32T:
        ...

    def GetEnvironmentEntryAtIndex(self, idx: UInt32T) -> CharConstStar:
        ...

    def SetEnvironmentEntries(self, envp: CharConstStarStar, append: bool) -> None:
        ...

    def SetEnvironment(self, env: SBEnvironment, append: bool) -> None:
        ...

    def GetEnvironment(self) -> SBEnvironment:
        ...

    def Clear(self) -> None:
        ...

    def GetWorkingDirectory(self) -> CharConstStar:
        ...

    def SetWorkingDirectory(self, working_dir: CharConstStar) -> None:
        ...

    def GetLaunchFlags(self) -> UInt32T:
        ...

    def SetLaunchFlags(self, flags: UInt32T) -> None:
        ...

    def GetProcessPluginName(self) -> CharConstStar:
        ...

    def SetProcessPluginName(self, plugin_name: CharConstStar) -> None:
        ...

    def GetShell(self) -> CharConstStar:
        ...

    def SetShell(self, path: CharConstStar) -> None:
        ...

    def GetShellExpandArguments(self) -> bool:
        ...

    def SetShellExpandArguments(self, expand: bool) -> None:
        ...

    def GetResumeCount(self) -> UInt32T:
        ...

    def SetResumeCount(self, c: UInt32T) -> None:
        ...

    def AddCloseFileAction(self, fd: int) -> bool:
        ...

    def AddDuplicateFileAction(self, fd: int, dup_fd: int) -> bool:
        ...

    def AddOpenFileAction(self, fd: int, path: CharConstStar, read: bool, write: bool) -> bool:
        ...

    def AddSuppressFileAction(self, fd: int, read: bool, write: bool) -> bool:
        ...

    def SetLaunchEventData(self, data: CharConstStar) -> None:
        ...

    def GetLaunchEventData(self) -> CharConstStar:
        ...

    def GetDetachOnError(self) -> bool:
        ...

    def SetDetachOnError(self, enable: bool) -> None:
        ...

    def GetScriptedProcessClassName(self) -> CharConstStar:
        ...

    def SetScriptedProcessClassName(self, class_name: CharConstStar) -> None:
        ...

    def GetScriptedProcessDictionary(self) -> SBStructuredData:
        ...

    def SetScriptedProcessDictionary(self, dict: SBStructuredData) -> None:
        ...
    



class SBLineEntry:
    r"""
    Specifies an association with a contiguous range of instructions and
    a source file location.

    :py:class:`SBCompileUnit` contains SBLineEntry(s). For example, ::

        for lineEntry in compileUnit:
            print('line entry: %s:%d' % (str(lineEntry.GetFileSpec()),
                                        lineEntry.GetLine()))
            print('start addr: %s' % str(lineEntry.GetStartAddress()))
            print('end   addr: %s' % str(lineEntry.GetEndAddress()))

    produces: ::

        line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:20
        start addr: a.out[0x100000d98]
        end   addr: a.out[0x100000da3]
        line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:21
        start addr: a.out[0x100000da3]
        end   addr: a.out[0x100000da9]
        line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:22
        start addr: a.out[0x100000da9]
        end   addr: a.out[0x100000db6]
        line entry: /Volumes/data/lldb/svn/trunk/test/python_api/symbol-context/main.c:23
        start addr: a.out[0x100000db6]
        end   addr: a.out[0x100000dbc]
        ...

    See also :py:class:`SBCompileUnit` .
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBLineEntry):
        ...        

    def GetStartAddress(self) -> SBAddress:
        ...

    def GetEndAddress(self) -> SBAddress:
        ...

    def IsValid(self) -> bool:
        ...


    def GetFileSpec(self) -> SBFileSpec:
        ...

    def GetLine(self) -> UInt32T:
        ...

    def GetColumn(self) -> UInt32T:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def SetFileSpec(self, filespec: SBFileSpec) -> None:
        ...

    def SetLine(self, line: UInt32T) -> None:
        ...

    def SetColumn(self, column: UInt32T) -> None:
        ...

    def __eq__(self, rhs: SBLineEntry) -> bool:
        ...

    def __ne__(self, rhs: SBLineEntry) -> bool:
        ...

    def __str__(self) -> str:
        ...

    file = property(GetFileSpec, None, doc='''A read only property that returns an lldb object that represents the file (lldb.SBFileSpec) for this line entry.''')
    line = property(GetLine, None, doc='''A read only property that returns the 1 based line number for this line entry, a return value of zero indicates that no line information is available.''')
    column = property(GetColumn, None, doc='''A read only property that returns the 1 based column number for this line entry, a return value of zero indicates that no column information is available.''')
    addr = property(GetStartAddress, None, doc='''A read only property that returns an lldb object that represents the start address (lldb.SBAddress) for this line entry.''')
    end_addr = property(GetEndAddress, None, doc='''A read only property that returns an lldb object that represents the end address (lldb.SBAddress) for this line entry.''')

class SBListener:
    r"""
    API clients can register its own listener to debugger events.

    See also :py:class:`SBEvent` for example usage of creating and adding a listener.
    """

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , name : CharConstStar):
        ...
    @overload
    def __init__(self , rhs: SBListener):
        ...

    def AddEvent(self, event: SBEvent) -> None:
        ...

    def Clear(self) -> None:
        ...

    def IsValid(self) -> bool:
        ...



    def StartListeningForEventClass(self, debugger: SBDebugger, broadcaster_class: CharConstStar, event_mask: UInt32T) -> UInt32T:
        ...

    def StopListeningForEventClass(self, debugger: SBDebugger, broadcaster_class: CharConstStar, event_mask: UInt32T) -> UInt32T:
        ...

    def StartListeningForEvents(self, broadcaster: SBBroadcaster, event_mask: UInt32T) -> UInt32T:
        ...

    def StopListeningForEvents(self, broadcaster: SBBroadcaster, event_mask: UInt32T) -> bool:
        ...

    def WaitForEvent(self, num_seconds: UInt32T, event: SBEvent) -> bool:
        ...

    def WaitForEventForBroadcaster(self, num_seconds: UInt32T, broadcaster: SBBroadcaster, sb_event: SBEvent) -> bool:
        ...

    def WaitForEventForBroadcasterWithType(self, num_seconds: UInt32T, broadcaster: SBBroadcaster, event_type_mask: UInt32T, sb_event: SBEvent) -> bool:
        ...

    def PeekAtNextEvent(self, sb_event: SBEvent) -> bool:
        ...

    def PeekAtNextEventForBroadcaster(self, broadcaster: SBBroadcaster, sb_event: SBEvent) -> bool:
        ...

    def PeekAtNextEventForBroadcasterWithType(self, broadcaster: SBBroadcaster, event_type_mask: UInt32T, sb_event: SBEvent) -> bool:
        ...

    def GetNextEvent(self, sb_event: SBEvent) -> bool:
        ...

    def GetNextEventForBroadcaster(self, broadcaster: SBBroadcaster, sb_event: SBEvent) -> bool:
        ...

    def GetNextEventForBroadcasterWithType(self, broadcaster: SBBroadcaster, event_type_mask: UInt32T, sb_event: SBEvent) -> bool:
        ...

    def HandleBroadcastEvent(self, event: SBEvent) -> bool:
        ...



class SBMemoryRegionInfo:
    r"""API clients can get information about memory regions in processes."""

    
    
    @overload
    def __init__(self):
        ...
    
    @overload
    def __init__(self ,rhs: SBMemoryRegionInfo):
        ...

    def Clear(self) -> None:
        ...

    def GetRegionBase(self) -> AddrT:
        ...

    def GetRegionEnd(self) -> AddrT:
        ...

    def IsReadable(self) -> bool:
        ...

    def IsWritable(self) -> bool:
        ...

    def IsExecutable(self) -> bool:
        ...

    def IsMapped(self) -> bool:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def HasDirtyMemoryPageList(self) -> bool:
        r"""

        Returns whether this memory region has a list of modified (dirty)
        pages available or not.  When calling GetNumDirtyPages(), you will
        have 0 returned for both "dirty page list is not known" and 
        "empty dirty page list" (that is, no modified pages in this
        memory region).  You must use this method to disambiguate.
        """
        ...

    def GetNumDirtyPages(self) -> UInt32T:
        r"""
        Return the number of dirty (modified) memory pages in this
        memory region, if available.  You must use the 
        SBMemoryRegionInfo::HasDirtyMemoryPageList() method to
        determine if a dirty memory list is available; it will depend
        on the target system can provide this information.
        """
        ...

    def GetDirtyPageAddressAtIndex(self, idx: UInt32T) -> AddrT:
        r"""
        Return the address of a modified, or dirty, page of memory.
        If the provided index is out of range, or this memory region 
        does not have dirty page information, LLDB_INVALID_ADDRESS 
        is returned.
        """
        ...

    def GetPageSize(self) -> int:
        r"""
        Return the size of pages in this memory region.  0 will be returned
        if this information was unavailable.
        """
        ...

    def __eq__(self, rhs: SBMemoryRegionInfo) -> bool:
        ...

    def __ne__(self, rhs: SBMemoryRegionInfo) -> bool:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...



class SBMemoryRegionInfoList:
    r"""Represents a list of :py:class:`SBMemoryRegionInfo`."""


    @overload
    def __init__(self ):
        ...

    @overload
    def __init__(self, rhs: SBMemoryRegionInfoList):
        ...

    def GetSize(self) -> UInt32T:
        ...

    def GetMemoryRegionAtIndex(self, idx: UInt32T, region_info: SBMemoryRegionInfo) -> bool:
        ...
    
    @overload
    def Append(self, region: SBMemoryRegionInfo) -> None:
        ...
    
    @overload
    def Append(self, region: SBMemoryRegionInfoList) -> None:
        ...

    def Clear(self) -> None:
        ...

class SBModule:
    r"""
    Represents an executable image and its associated object and symbol files.

    The module is designed to be able to select a single slice of an
    executable image as it would appear on disk and during program
    execution.

    You can retrieve SBModule from :py:class:`SBSymbolContext` , which in turn is available
    from SBFrame.

    SBModule supports symbol iteration, for example, ::

        for symbol in module:
            name = symbol.GetName()
            saddr = symbol.GetStartAddress()
            eaddr = symbol.GetEndAddress()

    and rich comparison methods which allow the API program to use, ::

        if thisModule == thatModule:
            print('This module is the same as that module')

    to test module equality.  A module also contains object file sections, namely
    :py:class:`SBSection` .  SBModule supports section iteration through section_iter(), for
    example, ::

        print('Number of sections: %d' % module.GetNumSections())
        for sec in module.section_iter():
            print(sec)

    And to iterate the symbols within a SBSection, use symbol_in_section_iter(), ::

        # Iterates the text section and prints each symbols within each sub-section.
        for subsec in text_sec:
            print(INDENT + repr(subsec))
            for sym in exe_module.symbol_in_section_iter(subsec):
                print(INDENT2 + repr(sym))
                print(INDENT2 + 'symbol type: %s' % symbol_type_to_str(sym.GetType()))

    produces this following output: ::

        [0x0000000100001780-0x0000000100001d5c) a.out.__TEXT.__text
            id = {0x00000004}, name = 'mask_access(MaskAction, unsigned int)', range = [0x00000001000017c0-0x0000000100001870)
            symbol type: code
            id = {0x00000008}, name = 'thread_func(void*)', range = [0x0000000100001870-0x00000001000019b0)
            symbol type: code
            id = {0x0000000c}, name = 'main', range = [0x00000001000019b0-0x0000000100001d5c)
            symbol type: code
            id = {0x00000023}, name = 'start', address = 0x0000000100001780
            symbol type: code
        [0x0000000100001d5c-0x0000000100001da4) a.out.__TEXT.__stubs
            id = {0x00000024}, name = '__stack_chk_fail', range = [0x0000000100001d5c-0x0000000100001d62)
            symbol type: trampoline
            id = {0x00000028}, name = 'exit', range = [0x0000000100001d62-0x0000000100001d68)
            symbol type: trampoline
            id = {0x00000029}, name = 'fflush', range = [0x0000000100001d68-0x0000000100001d6e)
            symbol type: trampoline
            id = {0x0000002a}, name = 'fgets', range = [0x0000000100001d6e-0x0000000100001d74)
            symbol type: trampoline
            id = {0x0000002b}, name = 'printf', range = [0x0000000100001d74-0x0000000100001d7a)
            symbol type: trampoline
            id = {0x0000002c}, name = 'pthread_create', range = [0x0000000100001d7a-0x0000000100001d80)
            symbol type: trampoline
            id = {0x0000002d}, name = 'pthread_join', range = [0x0000000100001d80-0x0000000100001d86)
            symbol type: trampoline
            id = {0x0000002e}, name = 'pthread_mutex_lock', range = [0x0000000100001d86-0x0000000100001d8c)
            symbol type: trampoline
            id = {0x0000002f}, name = 'pthread_mutex_unlock', range = [0x0000000100001d8c-0x0000000100001d92)
            symbol type: trampoline
            id = {0x00000030}, name = 'rand', range = [0x0000000100001d92-0x0000000100001d98)
            symbol type: trampoline
            id = {0x00000031}, name = 'strtoul', range = [0x0000000100001d98-0x0000000100001d9e)
            symbol type: trampoline
            id = {0x00000032}, name = 'usleep', range = [0x0000000100001d9e-0x0000000100001da4)
            symbol type: trampoline
        [0x0000000100001da4-0x0000000100001e2c) a.out.__TEXT.__stub_helper
        [0x0000000100001e2c-0x0000000100001f10) a.out.__TEXT.__cstring
        [0x0000000100001f10-0x0000000100001f68) a.out.__TEXT.__unwind_info
        [0x0000000100001f68-0x0000000100001ff8) a.out.__TEXT.__eh_frame

    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBModule):
        ...

    @overload
    def __init__(self , module_spec: SBModuleSpec):
        ...

    @overload
    def __init__(self , process: SBProcess , header_addr: AddrT):
        ...

    def IsValid(self) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def GetFileSpec(self) -> SBFileSpec:
        r"""
            Get const accessor for the module file specification.

            This function returns the file for the module on the host system
            that is running LLDB. This can differ from the path on the
            platform since we might be doing remote debugging.

            @return
                A const reference to the file specification object.
        """
        ...

    def GetPlatformFileSpec(self) -> SBFileSpec:
        r"""
            Get accessor for the module platform file specification.

            Platform file refers to the path of the module as it is known on
            the remote system on which it is being debugged. For local
            debugging this is always the same as Module::GetFileSpec(). But
            remote debugging might mention a file '/usr/lib/liba.dylib'
            which might be locally downloaded and cached. In this case the
            platform file could be something like:
            '/tmp/lldb/platform-cache/remote.host.computer/usr/lib/liba.dylib'
            The file could also be cached in a local developer kit directory.

            @return
                A const reference to the file specification object.
        """
        ...

    def SetPlatformFileSpec(self, platform_file: SBFileSpec) -> bool:
        ...

    def GetRemoteInstallFileSpec(self) -> SBFileSpec:
        ...

    def SetRemoteInstallFileSpec(self, file: SBFileSpec) -> bool:
        ...

    def GetUUIDString(self) -> CharConstStar:
        r"""
        Returns the UUID of the module as a Python string.
        """
        ...

    def __eq__(self, rhs: SBModule) -> bool:
        ...

    def __ne__(self, rhs: SBModule) -> bool:
        ...

    def FindSection(self, sect_name: CharConstStar) -> SBSection:
        ...

    def ResolveFileAddress(self, vm_addr: AddrT) -> SBAddress:
        ...

    def ResolveSymbolContextForAddress(self, addr: SBAddress, resolve_scope: UInt32T) -> SBSymbolContext:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def GetNumCompileUnits(self) -> UInt32T:
        ...

    def GetCompileUnitAtIndex(self, arg2: UInt32T) -> SBCompileUnit:
        ...

    def FindCompileUnits(self, sb_file_spec: SBFileSpec) -> SBSymbolContextList:
        r"""
            Find compile units related to this module and passed source
            file.

            @param[in] sb_file_spec
                A :py:class:`SBFileSpec` object that contains source file
                specification.

            @return
                A :py:class:`SBSymbolContextList` that gets filled in with all of
                the symbol contexts for all the matches.
        """
        ...

    def GetNumSymbols(self) -> SizeT:
        ...

    def GetSymbolAtIndex(self, idx: SizeT) -> SBSymbol:
        ...

    def FindSymbol(self, *args) -> SBSymbol:
        ...

    def FindSymbols(self, *args) -> SBSymbolContextList:
        ...

    def GetNumSections(self) -> SizeT:
        ...

    def GetSectionAtIndex(self, idx: SizeT) -> SBSection:
        ...

    def FindFunctions(self, name: CharConstStar , name_type_mask: UInt32T = None ) -> SBSymbolContextList:
        r"""
            Find functions by name.

            @param[in] name
                The name of the function we are looking for.

            @param[in] name_type_mask
                A logical OR of one or more FunctionNameType enum bits that
                indicate what kind of names should be used when doing the
                lookup. Bits include fully qualified names, base names,
                C++ methods, or ObjC selectors.
                See FunctionNameType for more details.

            @return
                A symbol context list that gets filled in with all of the
                matches.
        """
        ...

    def FindFirstType(self, name: CharConstStar) -> SBType:
        ...

    def FindTypes(self, type: CharConstStar) -> SBTypeList:
        ...

    def GetTypeByID(self, uid: UserIdT) -> SBType:
        ...

    def GetBasicType(self, type: BasicType) -> SBType:
        ...

    def GetTypes(self, *args) -> SBTypeList:
        r"""
            Get all types matching type_mask from debug info in this
            module.

            @param[in] type_mask
                A bitfield that consists of one or more bits logically OR'ed
                together from the lldb::TypeClass enumeration. This allows
                you to request only structure types, or only class, struct
                and union types. Passing in lldb::eTypeClassAny will return
                all types found in the debug information for this module.

            @return
                A list of types in this module that match type_mask
        """
        ...

    def FindGlobalVariables(self, target: SBTarget, name: CharConstStar, max_matches: UInt32T) -> SBValueList:
        r"""
            Find global and static variables by name.

            @param[in] target
                A valid SBTarget instance representing the debuggee.

            @param[in] name
                The name of the global or static variable we are looking
                for.

            @param[in] max_matches
                Allow the number of matches to be limited to max_matches.

            @return
                A list of matched variables in an SBValueList.
        """
        ...

    def FindFirstGlobalVariable(self, target: SBTarget, name: CharConstStar) -> SBValue:
        r"""
            Find the first global (or static) variable by name.

            @param[in] target
                A valid SBTarget instance representing the debuggee.

            @param[in] name
                The name of the global or static variable we are looking
                for.

            @return
                An SBValue that gets filled in with the found variable (if any).
        """
        ...

    def GetByteOrder(self) -> ByteOrder:
        ...

    def GetAddressByteSize(self) -> UInt32T:
        ...

    def GetTriple(self) -> CharConstStar:
        ...

    def GetVersion(self) -> UInt32T:
        ...

    def GetSymbolFileSpec(self) -> SBFileSpec:
        ...

    def GetObjectFileHeaderAddress(self) -> SBAddress:
        ...

    def GetObjectFileEntryPointAddress(self) -> SBAddress:
        ...

    @staticmethod
    def GetNumberAllocatedModules() -> UInt32T:
        r"""
            Returns the number of modules in the module cache. This is an
            implementation detail exposed for testing and should not be relied upon.

            @return
                The number of modules in the module cache.
        """
        ...

    @staticmethod
    def GarbageCollectAllocatedModules() -> None:
        r"""
            Removes all modules which are no longer needed by any part of LLDB from
            the module cache.

            This is an implementation detail exposed for testing and should not be
            relied upon. Use SBDebugger::MemoryPressureDetected instead to reduce
            LLDB's memory consumption during execution.

        """
        ...

    def __str__(self) -> str:
        ...

    def __len__(self) -> int:
        '''Return the number of symbols in a lldb.SBModule object.'''
        ...

    def __iter__(self) -> Iterable[SBSymbol]:
        '''Iterate over all symbols in a lldb.SBModule object.'''
        ...

    def section_iter(self) -> Iterable[SBSection]:
        '''Iterate over all sections in a lldb.SBModule object.'''
        ...

    def compile_unit_iter(self) -> Iterable[SBCompileUnit]:
        '''Iterate over all compile units in a lldb.SBModule object.'''
        ...

    def symbol_in_section_iter(self, section : SBSection) -> Iterable[SBSymbol]:
        '''Given a module and its contained section, returns an iterator on the
        symbols within the section.'''
        ...

    class symbols_access:
        re_compile_type = type(re.compile('.'))
        '''A helper object that will lazily hand out lldb.SBSymbol objects for a module when supplied an index, name, or regular expression.'''
        def __init__(self, sbmodule):
            self.sbmodule = sbmodule

        def __len__(self):
            if self.sbmodule:
                return int(self.sbmodule.GetNumSymbols())
            return 0

        def __getitem__(self, key):
            count = len(self)
            if type(key) is int:
                if key < count:
                    return self.sbmodule.GetSymbolAtIndex(key)
            elif type(key) is str:
                matches = []
                sc_list = self.sbmodule.FindSymbols(key)
                for sc in sc_list:
                    symbol = sc.symbol
                    if symbol:
                        matches.append(symbol)
                return matches
            elif isinstance(key, self.re_compile_type):
                matches = []
                for idx in range(count):
                    symbol = self.sbmodule.GetSymbolAtIndex(idx)
                    added = False
                    name = symbol.name
                    if name:
                        re_match = key.search(name)
                        if re_match:
                            matches.append(symbol)
                            added = True
                    if not added:
                        mangled = symbol.mangled
                        if mangled:
                            re_match = key.search(mangled)
                            if re_match:
                                matches.append(symbol)
                return matches
            else:
                print("error: unsupported item type: %s" % type(key))
            return None

    def get_symbols_access_object(self) -> symbols_access:
        '''An accessor function that returns a symbols_access() object which allows lazy symbol access from a lldb.SBModule object.'''
        return self.symbols_access (self)

    def get_compile_units_access_object (self) -> compile_units_access:
        '''An accessor function that returns a compile_units_access() object which allows lazy compile unit access from a lldb.SBModule object.'''
        return self.compile_units_access (self)

    def get_symbols_array(self) -> List[SBSymbol]:
        '''An accessor function that returns a list() that contains all symbols in a lldb.SBModule object.'''
        ...

    class sections_access:
        re_compile_type = type(re.compile('.'))
        '''A helper object that will lazily hand out lldb.SBSection objects for a module when supplied an index, name, or regular expression.'''
        def __init__(self, sbmodule):
            self.sbmodule = sbmodule

        def __len__(self):
            if self.sbmodule:
                return int(self.sbmodule.GetNumSections())
            return 0

        def __getitem__(self, key):
            count = len(self)
            if type(key) is int:
                if key < count:
                    return self.sbmodule.GetSectionAtIndex(key)
            elif type(key) is str:
                for idx in range(count):
                    section = self.sbmodule.GetSectionAtIndex(idx)
                    if section.name == key:
                        return section
            elif isinstance(key, self.re_compile_type):
                matches = []
                for idx in range(count):
                    section = self.sbmodule.GetSectionAtIndex(idx)
                    name = section.name
                    if name:
                        re_match = key.search(name)
                        if re_match:
                            matches.append(section)
                return matches
            else:
                print("error: unsupported item type: %s" % type(key))
            return None

    class compile_units_access:
        re_compile_type = type(re.compile('.'))
        '''A helper object that will lazily hand out lldb.SBCompileUnit objects for a module when supplied an index, full or partial path, or regular expression.'''
        def __init__(self, sbmodule):
            self.sbmodule = sbmodule

        def __len__(self):
            if self.sbmodule:
                return int(self.sbmodule.GetNumCompileUnits())
            return 0

        def __getitem__(self, key):
            count = len(self)
            if type(key) is int:
                if key < count:
                    return self.sbmodule.GetCompileUnitAtIndex(key)
            elif type(key) is str:
                is_full_path = key[0] == '/'
                for idx in range(count):
                    comp_unit = self.sbmodule.GetCompileUnitAtIndex(idx)
                    if is_full_path:
                        if comp_unit.file.fullpath == key:
                            return comp_unit
                    else:
                        if comp_unit.file.basename == key:
                            return comp_unit
            elif isinstance(key, self.re_compile_type):
                matches = []
                for idx in range(count):
                    comp_unit = self.sbmodule.GetCompileUnitAtIndex(idx)
                    fullpath = comp_unit.file.fullpath
                    if fullpath:
                        re_match = key.search(fullpath)
                        if re_match:
                            matches.append(comp_unit)
                return matches
            else:
                print("error: unsupported item type: %s" % type(key))
            return None

    def get_sections_access_object(self) -> sections_access:
        '''An accessor function that returns a sections_access() object which allows lazy section array access.'''
        ...

    def get_sections_array(self) -> List[SBSection]:
        '''An accessor function that returns an array object that contains all sections in this module object.'''
        ...

    def get_compile_units_array(self) -> List[SBCompileUnit]:
        '''An accessor function that returns an array object that contains all compile_units in this module object.'''
        ...
    
    def get_uuid(self):
        return uuid.UUID (self.GetUUIDString())


    symbols = property(get_symbols_array, None, doc='''A read only property that returns a list() of lldb.SBSymbol objects contained in this module.''')
    symbol = property(get_symbols_access_object, None, doc='''A read only property that can be used to access symbols by index ("symbol = module.symbol[0]"), name ("symbols = module.symbol['main']"), or using a regular expression ("symbols = module.symbol[re.compile(...)]"). The return value is a single lldb.SBSymbol object for array access, and a list() of lldb.SBSymbol objects for name and regular expression access''')
    sections = property(get_sections_array, None, doc='''A read only property that returns a list() of lldb.SBSection objects contained in this module.''')
    compile_units = property(get_compile_units_array, None, doc='''A read only property that returns a list() of lldb.SBCompileUnit objects contained in this module.''')
    section = property(get_sections_access_object, None, doc='''A read only property that can be used to access symbols by index ("section = module.section[0]"), name ("sections = module.section[\'main\']"), or using a regular expression ("sections = module.section[re.compile(...)]"). The return value is a single lldb.SBSection object for array access, and a list() of lldb.SBSection objects for name and regular expression access''')
    section = property(get_sections_access_object, None, doc='''A read only property that can be used to access compile units by index ("compile_unit = module.compile_unit[0]"), name ("compile_unit = module.compile_unit[\'main.cpp\']"), or using a regular expression ("compile_unit = module.compile_unit[re.compile(...)]"). The return value is a single lldb.SBCompileUnit object for array access or by full or partial path, and a list() of lldb.SBCompileUnit objects regular expressions.''')
    uuid = property(get_uuid, None, doc='''A read only property that returns a standard python uuid.UUID object that represents the UUID of this module.''')
    file = property(GetFileSpec, None, doc='''A read only property that returns an lldb object that represents the file (lldb.SBFileSpec) for this object file for this module as it is represented where it is being debugged.''')
    platform_file = property(GetPlatformFileSpec, None, doc='''A read only property that returns an lldb object that represents the file (lldb.SBFileSpec) for this object file for this module as it is represented on the current host system.''')
    byte_order = property(GetByteOrder, None, doc='''A read only property that returns an lldb enumeration value (lldb.eByteOrderLittle, lldb.eByteOrderBig, lldb.eByteOrderInvalid) that represents the byte order for this module.''')
    addr_size = property(GetAddressByteSize, None, doc='''A read only property that returns the size in bytes of an address for this module.''')
    triple = property(GetTriple, None, doc='''A read only property that returns the target triple (arch-vendor-os) for this module.''')
    num_symbols = property(GetNumSymbols, None, doc='''A read only property that returns number of symbols in the module symbol table as an integer.''')
    num_sections = property(GetNumSections, None, doc='''A read only property that returns number of sections in the module as an integer.''')

class SBModuleSpec:
    r"""Proxy of C++ lldb::SBModuleSpec class."""
    
    @overload
    def __init__( self ):
        ...
    @overload
    def __init__(self, rhs: SBModuleSpec):
        ...

    def IsValid(self) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def GetFileSpec(self) -> SBFileSpec:
        r"""
            Get const accessor for the module file.

            This function returns the file for the module on the host system
            that is running LLDB. This can differ from the path on the
            platform since we might be doing remote debugging.

            @return
                A const reference to the file specification object.
        """
        ...

    def SetFileSpec(self, fspec: SBFileSpec) -> None:
        ...

    def GetPlatformFileSpec(self) -> SBFileSpec:
        r"""
            Get accessor for the module platform file.

            Platform file refers to the path of the module as it is known on
            the remote system on which it is being debugged. For local
            debugging this is always the same as Module::GetFileSpec(). But
            remote debugging might mention a file '/usr/lib/liba.dylib'
            which might be locally downloaded and cached. In this case the
            platform file could be something like:
            '/tmp/lldb/platform-cache/remote.host.computer/usr/lib/liba.dylib'
            The file could also be cached in a local developer kit directory.

            @return
                A const reference to the file specification object.
        """
        ...

    def SetPlatformFileSpec(self, fspec: SBFileSpec) -> None:
        ...

    def GetSymbolFileSpec(self) -> SBFileSpec:
        ...

    def SetSymbolFileSpec(self, fspec: SBFileSpec) -> None:
        ...

    def GetObjectName(self) -> CharConstStar:
        ...

    def SetObjectName(self, name: CharConstStar) -> None:
        ...

    def GetTriple(self) -> CharConstStar:
        ...

    def SetTriple(self, triple: CharConstStar) -> None:
        ...

    def GetUUIDBytes(self) -> UInt8ConstStar:
        ...

    def GetUUIDLength(self) -> SizeT:
        ...

    def SetUUIDBytes(self, uuid: UInt8ConstStar, uuid_len: SizeT) -> bool:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...



class SBModuleSpecList:
    r"""Represents a list of :py:class:`SBModuleSpec`."""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBModuleSpecList):
        ...
    

    @staticmethod
    def GetModuleSpecifications(path: CharConstStar) -> SBModuleSpecList:
        ...
    
    @overload
    def Append(self, spec: SBModuleSpec ) -> None:
        ...
    @overload
    def Append(self, spec_list: SBModuleSpecList ) -> None:
        ...
 
    def FindFirstMatchingSpec(self, match_spec: SBModuleSpec) -> SBModuleSpec:
        ...

    def FindMatchingSpecs(self, match_spec: SBModuleSpec) -> SBModuleSpecList:
        ...

    def GetSize(self) -> SizeT:
        ...

    def GetSpecAtIndex(self, i: SizeT) -> SBModuleSpec:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...


class SBPlatformConnectOptions:
    r"""Describes how :py:class:`SBPlatform.ConnectRemote` connects to a remote platform."""

    @overload
    def __init__(self, url: CharConstStar):
        ...
    @overload
    def __init__(self, rhs: SBPlatformConnectOptions):
        ...


    def GetURL(self) -> CharConstStar:
        ...

    def SetURL(self, url: CharConstStar) -> None:
        ...

    def GetRsyncEnabled(self) -> bool:
        ...

    def EnableRsync(self, options: CharConstStar, remote_path_prefix: CharConstStar, omit_remote_hostname: bool) -> None:
        ...

    def DisableRsync(self) -> None:
        ...

    def GetLocalCacheDirectory(self) -> CharConstStar:
        ...

    def SetLocalCacheDirectory(self, path: CharConstStar) -> None:
        ...



class SBPlatformShellCommand:
    r"""Represents a shell command that can be run by :py:class:`SBPlatform.Run`."""

    @overload
    def __init__(self, shell: CharConstStar , shell_command: CharConstStar):
        ...
    @overload
    def __init__(self, shell_command: CharConstStar):
        ...
    @overload
    def __init__(self, rhs : SBPlatformShellCommand):
        ...

    def Clear(self) -> None:
        ...

    def GetShell(self) -> CharConstStar:
        ...

    def SetShell(self, shell_interpreter: CharConstStar) -> None:
        ...

    def GetCommand(self) -> CharConstStar:
        ...

    def SetCommand(self, shell_command: CharConstStar) -> None:
        ...

    def GetWorkingDirectory(self) -> CharConstStar:
        ...

    def SetWorkingDirectory(self, path: CharConstStar) -> None:
        ...

    def GetTimeoutSeconds(self) -> UInt32T:
        ...

    def SetTimeoutSeconds(self, sec: UInt32T) -> None:
        ...

    def GetSignal(self) -> int:
        ...

    def GetStatus(self) -> int:
        ...

    def GetOutput(self) -> CharConstStar:
        ...



class SBPlatform:
    r"""
    A class that represents a platform that can represent the current host or a remote host debug platform.

    The SBPlatform class represents the current host, or a remote host.
    It can be connected to a remote platform in order to provide ways
    to remotely launch and attach to processes, upload/download files,
    create directories, run remote shell commands, find locally cached
    versions of files from the remote system, and much more.

    SBPlatform objects can be created and then used to connect to a remote
    platform which allows the SBPlatform to be used to get a list of the
    current processes on the remote host, attach to one of those processes,
    install programs on the remote system, attach and launch processes,
    and much more.

    Every :py:class:`SBTarget` has a corresponding SBPlatform. The platform can be
    specified upon target creation, or the currently selected platform
    will attempt to be used when creating the target automatically as long
    as the currently selected platform matches the target architecture
    and executable type. If the architecture or executable type do not match,
    a suitable platform will be found automatically.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , arg2 : CharConstStar):
        ...

    @staticmethod
    def GetHostPlatform() -> SBPlatform:
        ...

    def IsValid(self) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def GetWorkingDirectory(self) -> CharConstStar:
        ...

    def SetWorkingDirectory(self, arg2: CharConstStar) -> bool:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def ConnectRemote(self, connect_options: SBPlatformConnectOptions) -> SBError:
        ...

    def DisconnectRemote(self) -> None:
        ...

    def IsConnected(self) -> bool:
        ...

    def GetTriple(self) -> CharConstStar:
        ...

    def GetHostname(self) -> CharConstStar:
        ...

    def GetOSBuild(self) -> CharConstStar:
        ...

    def GetOSDescription(self) -> CharConstStar:
        ...

    def GetOSMajorVersion(self) -> UInt32T:
        ...

    def GetOSMinorVersion(self) -> UInt32T:
        ...

    def GetOSUpdateVersion(self) -> UInt32T:
        ...

    def Get(self, src: SBFileSpec, dst: SBFileSpec) -> SBError:
        ...

    def Put(self, src: SBFileSpec, dst: SBFileSpec) -> SBError:
        ...

    def Install(self, src: SBFileSpec, dst: SBFileSpec) -> SBError:
        ...

    def Run(self, shell_command: SBPlatformShellCommand) -> SBError:
        ...

    def Launch(self, launch_info: SBLaunchInfo) -> SBError:
        ...

    def Kill(self, pid: PIDT) -> SBError:
        ...

    def MakeDirectory(self, *args) -> SBError:
        ...

    def GetFilePermissions(self, path: CharConstStar) -> UInt32T:
        ...

    def SetFilePermissions(self, path: CharConstStar, file_permissions: UInt32T) -> SBError:
        ...

    def GetUnixSignals(self) -> SBUnixSignals:
        ...

    def GetEnvironment(self) -> SBEnvironment:
        ...



class SBProcess:
    r"""
    Represents the process associated with the target program.

    SBProcess supports thread iteration. For example (from test/lldbutil.py), ::

        # ==================================================
        # Utility functions related to Threads and Processes
        # ==================================================

        def get_stopped_threads(process, reason):
            '''Returns the thread(s) with the specified stop reason in a list.

            The list can be empty if no such thread exists.
            '''
            threads = []
            for t in process:
                if t.GetStopReason() == reason:
                    threads.append(t)
            return threads

    """
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBProcess):
        ...
        
    

    @staticmethod
    def GetBroadcasterClassName() -> CharConstStar:
        ...

    def GetPluginName(self) -> CharConstStar:
        ...

    def GetShortPluginName(self) -> CharConstStar:
        ...

    def Clear(self) -> None:
        ...

    def IsValid(self) -> bool:
        ...




    def GetTarget(self) -> SBTarget:
        ...

    def GetByteOrder(self) -> ByteOrder:
        ...

    def PutSTDIN(self, src: CharConstStar) -> SizeT:
        r"""

        Writes data into the current process's stdin. API client specifies a Python
        string as the only argument.
        """
        ...

    def GetSTDOUT(self, dst: CharStar) -> SizeT:
        r"""
        Reads data from the current process's stdout stream. API client specifies
        the size of the buffer to read data into. It returns the byte buffer in a
        Python string.
        """
        ...

    def GetSTDERR(self, dst: CharStar) -> SizeT:
        r"""
        Reads data from the current process's stderr stream. API client specifies
        the size of the buffer to read data into. It returns the byte buffer in a
        Python string.
        """
        ...

    def GetAsyncProfileData(self, dst: CharStar) -> SizeT:
        ...
    
    @overload
    def ReportEventState(self, event: SBEvent , out: SBFile) -> None:
        ...

    @overload
    def ReportEventState(self, event: SBEvent , out: FileSP) -> None:
        ...
        
    def AppendEventStateReport(self, event: SBEvent, result: SBCommandReturnObject) -> None:
        ...

    def RemoteAttachToProcessWithID(self, pid: PIDT, error: SBError) -> bool:
        r"""
            Remote connection related functions. These will fail if the
            process is not in eStateConnected. They are intended for use
            when connecting to an externally managed debugserver instance.
        """

    def RemoteLaunch(self, argv: CharConstStarStar, envp: CharConstStarStar, stdin_path: CharConstStar, stdout_path: CharConstStar, stderr_path: CharConstStar, working_directory: CharConstStar, launch_flags: UInt32T, stop_at_entry: bool, error: SBError) -> bool:
        r"""
        See SBTarget.Launch for argument description and usage.
        """
        ...

    def GetNumThreads(self) -> UInt32T:
        ...

    def GetThreadAtIndex(self, index: SizeT) -> SBThread:
        r"""

        Returns the INDEX'th thread from the list of current threads.  The index
        of a thread is only valid for the current stop.  For a persistent thread
        identifier use either the thread ID or the IndexID.  See help on SBThread
        for more details.
        """
        ...

    def GetThreadByID(self, sb_thread_id: TIdT) -> SBThread:
        r"""

        Returns the thread with the given thread ID.
        """
        ...

    def GetThreadByIndexID(self, index_id: UInt32T) -> SBThread:
        r"""
        Returns the thread with the given thread IndexID.
        """
        ...

    def GetSelectedThread(self) -> SBThread:
        r"""
        Returns the currently selected thread.
        """
        ...

    def CreateOSPluginThread(self, tid: TIdT, context: AddrT) -> SBThread:
        r"""
        Lazily create a thread on demand through the current OperatingSystem plug-in, if the current OperatingSystem plug-in supports it.
        """
        ...

    def SetSelectedThread(self, thread: SBThread) -> bool:
        ...

    def SetSelectedThreadByID(self, tid: TIdT) -> bool:
        ...

    def SetSelectedThreadByIndexID(self, index_id: UInt32T) -> bool:
        ...

    def GetNumQueues(self) -> UInt32T:
        ...

    def GetQueueAtIndex(self, index: UInt32T) -> SBQueue:
        ...

    def GetState(self) -> StateType:
        ...

    def GetExitStatus(self) -> int:
        ...

    def GetExitDescription(self) -> CharConstStar:
        ...

    def GetProcessID(self) -> PIDT:
        r"""

        Returns the process ID of the process.
        """
        ...

    def GetUniqueID(self) -> UInt32T:
        r"""
        Returns an integer ID that is guaranteed to be unique across all process instances. This is not the process ID, just a unique integer for comparison and caching purposes.
        """
        ...

    def GetAddressByteSize(self) -> UInt32T:
        ...

    def Destroy(self) -> SBError:
        r"""
            Kills the process and shuts down all threads that were spawned to
            track and monitor process.
        """
        ...

    def Continue(self) -> SBError:
        ...

    def Stop(self) -> SBError:
        ...

    def Kill(self) -> SBError:
        ...

    def Detach(self) -> SBError:
        ...

    def Signal(self, signal: int) -> SBError:
        r"""
        Sends the process a unix signal.
        """
        ...

    def GetUnixSignals(self) -> SBUnixSignals:
        ...

    def GetStopID(self, include_expression_stops: bool=False) -> UInt32T:
        r"""
            Returns a stop id that will increase every time the process executes.  If
            include_expression_stops is true, then stops caused by expression evaluation
            will cause the returned value to increase, otherwise the counter returned will
            only increase when execution is continued explicitly by the user.  Note, the value
            will always increase, but may increase by more than one per stop.
        """
        ...

    def SendAsyncInterrupt(self) -> None:
        ...

    def ReadMemory(self, addr: AddrT, buf: VoidStar, error: SBError) -> SizeT:
        r"""
        Reads memory from the current process's address space and removes any
        traps that may have been inserted into the memory. It returns the byte
        buffer in a Python string. Example: ::

            # Read 4 bytes from address 'addr' and assume error.Success() is True.
            content = process.ReadMemory(addr, 4, error)
            new_bytes = bytearray(content)
        """
        ...

    def WriteMemory(self, addr: AddrT, buf: VoidConstStar, error: SBError) -> SizeT:
        r"""
        Writes memory to the current process's address space and maintains any
        traps that might be present due to software breakpoints. Example: ::

            # Create a Python string from the byte array.
            new_value = str(bytes)
            result = process.WriteMemory(addr, new_value, error)
            if not error.Success() or result != len(bytes):
                print('SBProcess.WriteMemory() failed!')
        """
        ...

    def ReadCStringFromMemory(self, addr: AddrT, char_buf: VoidStar, error: SBError) -> SizeT:
        r"""
        Reads a NULL terminated C string from the current process's address space.
        It returns a python string of the exact length, or truncates the string if
        the maximum character limit is reached. Example: ::

            # Read a C string of at most 256 bytes from address '0x1000'
            error = lldb.SBError()
            cstring = process.ReadCStringFromMemory(0x1000, 256, error)
            if error.Success():
                print('cstring: ', cstring)
            else
                print('error: ', error)
        """
        ...

    def ReadUnsignedFromMemory(self, addr: AddrT, byte_size: UInt32T, error: SBError) -> UInt64T:
        r"""
        Reads an unsigned integer from memory given a byte size and an address.
        Returns the unsigned integer that was read. Example: ::

            # Read a 4 byte unsigned integer from address 0x1000
            error = lldb.SBError()
            uint = ReadUnsignedFromMemory(0x1000, 4, error)
            if error.Success():
                print('integer: %u' % uint)
            else
                print('error: ', error)
        """
        ...

    def ReadPointerFromMemory(self, addr: AddrT, error: SBError) -> AddrT:
        r"""
        Reads a pointer from memory from an address and returns the value. Example: ::

            # Read a pointer from address 0x1000
            error = lldb.SBError()
            ptr = ReadPointerFromMemory(0x1000, error)
            if error.Success():
                print('pointer: 0x%x' % ptr)
            else
                print('error: ', error)
        """
        ...

    @staticmethod
    def GetStateFromEvent(event: SBEvent) -> StateType:
        ...

    @staticmethod
    def GetRestartedFromEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetNumRestartedReasonsFromEvent(event: SBEvent) -> SizeT:
        ...

    @staticmethod
    def GetRestartedReasonAtIndexFromEvent(event: SBEvent, idx: SizeT) -> CharConstStar:
        ...

    @staticmethod
    def GetProcessFromEvent(event: SBEvent) -> SBProcess:
        ...

    @staticmethod
    def GetInterruptedFromEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetStructuredDataFromEvent(event: SBEvent) -> SBStructuredData:
        ...

    @staticmethod
    def EventIsProcessEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def EventIsStructuredDataEvent(event: SBEvent) -> bool:
        ...

    def GetBroadcaster(self) -> SBBroadcaster:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def GetExtendedCrashInformation(self) -> SBStructuredData:
        r"""
        Returns the process' extended crash information.
        """
        ...

    def GetNumSupportedHardwareWatchpoints(self, error: SBError) -> UInt32T:
        ...

    def LoadImage(self, image_spec: SBFileSpec, error: SBError) -> UInt32T:
        ...

    def LoadImageUsingPaths(self, image_spec: SBFileSpec, paths: SBStringList, loaded_path: SBFileSpec, error: SBError) -> UInt32T:
        r"""
        Load the library whose filename is given by image_spec looking in all the
        paths supplied in the paths argument.  If successful, return a token that
        can be passed to UnloadImage and fill loaded_path with the path that was
        successfully loaded.  On failure, return
        lldb.LLDB_INVALID_IMAGE_TOKEN.
        """
        ...

    def UnloadImage(self, image_token: UInt32T) -> SBError:
        ...

    def SendEventData(self, event_data: CharConstStar) -> SBError:
        ...

    def GetNumExtendedBacktraceTypes(self) -> UInt32T:
        r"""
        Return the number of different thread-origin extended backtraces
        this process can support as a uint32_t.
        When the process is stopped and you have an SBThread, lldb may be
        able to show a backtrace of when that thread was originally created,
        or the work item was enqueued to it (in the case of a libdispatch
        queue).
        """
        ...

    def GetExtendedBacktraceTypeAtIndex(self, idx: UInt32T) -> CharConstStar:
        r"""
        Takes an index argument, returns the name of one of the thread-origin
        extended backtrace methods as a str.
        """
        ...

    def GetHistoryThreads(self, addr: AddrT) -> SBThreadCollection:
        ...

    def IsInstrumentationRuntimePresent(self, type: InstrumentationRuntimeType) -> bool:
        ...

    def SaveCore(self, file_name: CharConstStar) -> SBError:
        ...

    def GetMemoryRegionInfo(self, load_addr: AddrT, region_info: SBMemoryRegionInfo) -> SBError:
        ...

    def GetMemoryRegions(self) -> SBMemoryRegionInfoList:
        ...

    def GetProcessInfo(self) -> SBProcessInfo:
        r"""
        Get information about the process.
        Valid process info will only be returned when the process is alive,
        use IsValid() to check if the info returned is valid. ::

            process_info = process.GetProcessInfo()
            if process_info.IsValid():
                process_info.GetProcessID()
        """
        ...

    def AllocateMemory(self, size: SizeT, permissions: UInt32T, error: SBError) -> AddrT:
        r"""
        Allocates a block of memory within the process, with size and
        access permissions specified in the arguments. The permisssions
        argument is an or-combination of zero or more of
        lldb.ePermissionsWritable, lldb.ePermissionsReadable, and
        lldb.ePermissionsExecutable. Returns the address
        of the allocated buffer in the process, or
        lldb.LLDB_INVALID_ADDRESS if the allocation failed.
        """
        ...

    def DeallocateMemory(self, ptr: AddrT) -> SBError:
        r"""

        Deallocates the block of memory (previously allocated using
        AllocateMemory) given in the argument.
        """
        ...

    def __str__(self) -> str:
        ...

    def __get_is_alive__(self) -> bool:
        '''Returns "True" if the process is currently alive, "False" otherwise'''
        ...

    def __get_is_running__(self) -> bool:
        '''Returns "True" if the process is currently running, "False" otherwise'''
        ...

    def __get_is_stopped__(self) -> bool:
        '''Returns "True" if the process is currently stopped, "False" otherwise'''
        ...

    class threads_access:
        '''A helper object that will lazily hand out thread for a process when supplied an index.'''
        def __init__(self, sbprocess):
            self.sbprocess = sbprocess

        def __len__(self):
            if self.sbprocess:
                return int(self.sbprocess.GetNumThreads())
            return 0

        def __getitem__(self, key):
            if type(key) is int and key < len(self):
                return self.sbprocess.GetThreadAtIndex(key)
            return None

    def get_threads_access_object(self) -> threads_access:
        '''An accessor function that returns a modules_access() object which allows lazy thread access from a lldb.SBProcess object.'''
        ...

    def get_process_thread_list(self) -> List[SBThread]:
        '''An accessor function that returns a list() that contains all threads in a lldb.SBProcess object.'''
        ...

    def __iter__(self) -> Iterable[SBThread]:
        '''Iterate over all threads in a lldb.SBProcess object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of threads in a lldb.SBProcess object.'''
        ...


    threads = property(get_process_thread_list, None, doc='''A read only property that returns a list() of lldb.SBThread objects for this process.''')
    thread = property(get_threads_access_object, None, doc='''A read only property that returns an object that can access threads by thread index (thread = lldb.process.thread[12]).''')
    is_alive = property(__get_is_alive__, None, doc='''A read only property that returns a boolean value that indicates if this process is currently alive.''')
    is_running = property(__get_is_running__, None, doc='''A read only property that returns a boolean value that indicates if this process is currently running.''')
    is_stopped = property(__get_is_stopped__, None, doc='''A read only property that returns a boolean value that indicates if this process is currently stopped.''')
    id = property(GetProcessID, None, doc='''A read only property that returns the process ID as an integer.''')
    target = property(GetTarget, None, doc='''A read only property that an lldb object that represents the target (lldb.SBTarget) that owns this process.''')
    num_threads = property(GetNumThreads, None, doc='''A read only property that returns the number of threads in this process as an integer.''')
    selected_thread = property(GetSelectedThread, SetSelectedThread, doc='''A read/write property that gets/sets the currently selected thread in this process. The getter returns a lldb.SBThread object and the setter takes an lldb.SBThread object.''')
    state = property(GetState, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eState") that represents the current state of this process (running, stopped, exited, etc.).''')
    exit_state = property(GetExitStatus, None, doc='''A read only property that returns an exit status as an integer of this process when the process state is lldb.eStateExited.''')
    exit_description = property(GetExitDescription, None, doc='''A read only property that returns an exit description as a string of this process when the process state is lldb.eStateExited.''')
    broadcaster = property(GetBroadcaster, None, doc='''A read only property that an lldb object that represents the broadcaster (lldb.SBBroadcaster) for this process.''')




class SBProcessInfo:
    r"""
    Describes an existing process and any discoverable information that pertains to
    that process.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBProcessInfo):
        ...
        

    def IsValid(self) -> bool:
        ...


    def GetName(self) -> CharConstStar:
        ...

    def GetExecutableFile(self) -> SBFileSpec:
        ...

    def GetProcessID(self) -> PIDT:
        ...

    def GetUserID(self) -> UInt32T:
        ...

    def GetGroupID(self) -> UInt32T:
        ...

    def UserIDIsValid(self) -> bool:
        ...

    def GroupIDIsValid(self) -> bool:
        ...

    def GetEffectiveUserID(self) -> UInt32T:
        ...

    def GetEffectiveGroupID(self) -> UInt32T:
        ...

    def EffectiveUserIDIsValid(self) -> bool:
        ...

    def EffectiveGroupIDIsValid(self) -> bool:
        ...

    def GetParentProcessID(self) -> PIDT:
        ...

    def GetTriple(self) -> CharConstStar:
        r"""
        Return the target triple (arch-vendor-os) for the described process.
        """
        ...



class SBQueue:
    r"""Represents a libdispatch queue in the process."""

    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , queue_sp: QueueSP):
        ...
    

    def IsValid(self) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetQueueID(self) -> QueueIdT:
        r"""

        Returns an lldb::QueueIdT type unique identifier number for this
        queue that will not be used by any other queue during this process'
        execution.  These ID numbers often start at 1 with the first
        system-created queues and increment from there.
        """
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetKind(self) -> QueueKind:
        r"""
        Returns an lldb::QueueKind enumerated value (e.g. eQueueKindUnknown,
        eQueueKindSerial, eQueueKindConcurrent) describing the type of this
        queue.
        """
        ...

    def GetIndexID(self) -> UInt32T:
        ...

    def GetNumThreads(self) -> UInt32T:
        ...

    def GetThreadAtIndex(self, arg2: UInt32T) -> SBThread:
        ...

    def GetNumPendingItems(self) -> UInt32T:
        ...

    def GetPendingItemAtIndex(self, arg2: UInt32T) -> SBQueueItem:
        ...

    def GetNumRunningItems(self) -> UInt32T:
        ...



class SBQueueItem:
    r"""This class represents an item in an :py:class:`SBQueue`."""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self, queue_item_sp: QueueItemSP):
        ...
    

    def IsValid(self) -> bool:
        ...


    def Clear(self) -> None:
        ...

    def GetKind(self) -> QueueItemKind:
        ...

    def SetKind(self, kind: QueueItemKind) -> None:
        ...

    def GetAddress(self) -> SBAddress:
        ...

    def SetAddress(self, addr: SBAddress) -> None:
        ...

    def SetQueueItem(self, queue_item_sp: QueueItemSP) -> None:
        ...

    def GetExtendedBacktraceThread(self, type: CharConstStar) -> SBThread:
        ...



class SBReproducer:
    r"""Controls LLDB's reproducer functionality."""

    
    

    @staticmethod
    def Capture(path: CharConstStar) -> CharConstStar:
        ...

    @staticmethod
    def PassiveReplay(path: CharConstStar) -> CharConstStar:
        ...

    @staticmethod
    def SetAutoGenerate(b: bool) -> bool:
        ...

    @staticmethod
    def SetWorkingDirectory(path: CharConstStar) -> None:
        ...

    def __init__(self):
        ...
    



class SBSection:
    r"""
    Represents an executable image section.

    SBSection supports iteration through its subsection, represented as SBSection
    as well.  For example, ::

        for sec in exe_module:
            if sec.GetName() == '__TEXT':
                print sec
                break
        print INDENT + 'Number of subsections: %d' % sec.GetNumSubSections()
        for subsec in sec:
            print INDENT + repr(subsec)

    produces: ::

      [0x0000000100000000-0x0000000100002000) a.out.__TEXT
          Number of subsections: 6
          [0x0000000100001780-0x0000000100001d5c) a.out.__TEXT.__text
          [0x0000000100001d5c-0x0000000100001da4) a.out.__TEXT.__stubs
          [0x0000000100001da4-0x0000000100001e2c) a.out.__TEXT.__stub_helper
          [0x0000000100001e2c-0x0000000100001f10) a.out.__TEXT.__cstring
          [0x0000000100001f10-0x0000000100001f68) a.out.__TEXT.__unwind_info
          [0x0000000100001f68-0x0000000100001ff8) a.out.__TEXT.__eh_frame

    See also :py:class:`SBModule` .
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBSection):
        ...
        
    def IsValid(self) -> bool:
        ...


    def GetName(self) -> CharConstStar:
        ...

    def GetParent(self) -> SBSection:
        ...

    def FindSubSection(self, sect_name: CharConstStar) -> SBSection:
        ...

    def GetNumSubSections(self) -> SizeT:
        ...

    def GetSubSectionAtIndex(self, idx: SizeT) -> SBSection:
        ...

    def GetFileAddress(self) -> AddrT:
        ...

    def GetLoadAddress(self, target: SBTarget) -> AddrT:
        ...

    def GetByteSize(self) -> AddrT:
        ...

    def GetFileOffset(self) -> UInt64T:
        ...

    def GetFileByteSize(self) -> UInt64T:
        ...
    
    @overload
    def GetSectionData(self) -> SBData:
        ...

    @overload
    def GetSectionData( self,offset: UInt64T , size: UInt64T) -> SBData:
        ...

    def GetSectionType(self) -> SectionType:
        ...

    def GetPermissions(self) -> UInt32T:
        ...

    def GetTargetByteSize(self) -> UInt32T:
        r"""
            Return the size of a target's byte represented by this section
            in numbers of host bytes. Note that certain architectures have
            varying minimum addressable unit (i.e. byte) size for their
            CODE or DATA buses.

            @return
                The number of host (8-bit) bytes needed to hold a target byte
        """
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __eq__(self, rhs: SBSection) -> bool:
        ...

    def __ne__(self, rhs: SBSection) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBSection]:
        '''Iterate over all subsections in a lldb.SBSection object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of subsections in a lldb.SBSection object.'''
        ...

    def get_addr(self) -> SBAddress:
        return SBAddress(self, 0)

    name = property(GetName, None, doc='''A read only property that returns the name of this section as a string.''')
    addr = property(get_addr, None, doc='''A read only property that returns an lldb object that represents the start address (lldb.SBAddress) for this section.''')
    file_addr = property(GetFileAddress, None, doc='''A read only property that returns an integer that represents the starting "file" address for this section, or the address of the section in the object file in which it is defined.''')
    size = property(GetByteSize, None, doc='''A read only property that returns the size in bytes of this section as an integer.''')
    file_offset = property(GetFileOffset, None, doc='''A read only property that returns the file offset in bytes of this section as an integer.''')
    file_size = property(GetFileByteSize, None, doc='''A read only property that returns the file size in bytes of this section as an integer.''')
    data = property(GetSectionData, None, doc='''A read only property that returns an lldb object that represents the bytes for this section (lldb.SBData) for this section.''')
    type = property(GetSectionType, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eSectionType") that represents the type of this section (code, data, etc.).''')
    target_byte_size = property(GetTargetByteSize, None, doc='''A read only property that returns the size of a target byte represented by this section as a number of host bytes.''')


class SBSourceManager:
    r"""
    Represents a central authority for displaying source code.

    For example (from test/source-manager/TestSourceManager.py), ::

            # Create the filespec for 'main.c'.
            filespec = lldb.SBFileSpec('main.c', False)
            source_mgr = self.dbg.GetSourceManager()
            # Use a string stream as the destination.
            stream = lldb.SBStream()
            source_mgr.DisplaySourceLinesWithLineNumbers(filespec,
                                                         self.line,
                                                         2, # context before
                                                         2, # context after
                                                         '=>', # prefix for current line
                                                         stream)

            #    2
            #    3    int main(int argc, char const *argv[]) {
            # => 4        printf('Hello world.\n'); // Set break point at this line.
            #    5        return 0;
            #    6    }
            self.expect(stream.GetData(), 'Source code displayed correctly',
                        exe=False,
                patterns = ['=> %d.*Hello world' % self.line])
    """

    
    

    def __init__(self, rhs: SBSourceManager):
        ...
    

    def DisplaySourceLinesWithLineNumbers(self, file: SBFileSpec, line: UInt32T, context_before: UInt32T, context_after: UInt32T, current_line_cstr: CharConstStar, s: SBStream) -> SizeT:
        ...

    def DisplaySourceLinesWithLineNumbersAndColumn(self, file: SBFileSpec, line: UInt32T, column: UInt32T, context_before: UInt32T, context_after: UInt32T, current_line_cstr: CharConstStar, s: SBStream) -> SizeT:
        ...



class SBStream:
    r"""
    Represents a destination for streaming data output to. By default, a string
    stream is created.

    For example (from test/source-manager/TestSourceManager.py), ::

            # Create the filespec for 'main.c'.
            filespec = lldb.SBFileSpec('main.c', False)
            source_mgr = self.dbg.GetSourceManager()
            # Use a string stream as the destination.
            stream = lldb.SBStream()
            source_mgr.DisplaySourceLinesWithLineNumbers(filespec,
                                                         self.line,
                                                         2, # context before
                                                         2, # context after
                                                         '=>', # prefix for current line
                                                         stream)

            #    2
            #    3    int main(int argc, char const *argv[]) {
            # => 4        printf('Hello world.\n'); // Set break point at this line.
            #    5        return 0;
            #    6    }
            self.expect(stream.GetData(), 'Source code displayed correctly',
                        exe=False,
                patterns = ['=> %d.*Hello world' % self.line])
    """

    
    

    def __init__(self):
        ...

    def IsValid(self) -> bool:
        ...


    def GetData(self) -> CharConstStar:
        r"""
            If this stream is not redirected to a file, it will maintain a local
            cache for the stream data which can be accessed using this accessor.
        """
        ...

    def GetSize(self) -> SizeT:
        r"""
            If this stream is not redirected to a file, it will maintain a local
            cache for the stream output whose length can be accessed using this
            accessor.
        """
        ...

    def Print(self, str: CharConstStar) -> None:
        ...
    
    @overload
    def RedirectToFile(self, path: CharConstStar , append: bool) -> None:
        ...
    @overload
    def RedirectToFile(self, file: SBFile) -> None:
        ...
    @overload
    def RedirectToFile(self, file: FileSP) -> None:
        ...

    def RedirectToFileHandle(self, file: FileSP, transfer_fh_ownership: bool) -> None:
        ...

    def RedirectToFileDescriptor(self, fd: int, transfer_fh_ownership: bool) -> None:
        ...

    def Clear(self) -> None:
        r"""
        DEPRECATED, use RedirectToFile

            If the stream is redirected to a file, forget about the file and if
            ownership of the file was transferred to this object, close the file.
            If the stream is backed by a local cache, clear this cache.
        """
        ...

    def write(self, str: CharConstStar) -> None:
        ...

    def flush(self) -> None:
        ...



class SBStringList:
    r"""Represents a list of strings."""

    
    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBStringList):
        ...
    

    def IsValid(self) -> bool:
        ...    



    def AppendString(self, str: CharConstStar) -> None:
        ...
    
    @overload
    def AppendList(self, strv: CharConstStarStar , strc: int) -> None:
        ...

    @overload
    def AppendList(self, strings: SBStringList) -> None:
        ...
        

    def GetSize(self) -> UInt32T:
        ...

    def GetStringAtIndex(self, idx: SizeT) -> CharConstStar:
        ...

    def Clear(self) -> None:
        ...

    def __iter__(self) -> Iterable[CharConstStar]:
        '''Iterate over all strings in a lldb.SBStringList object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of strings in a lldb.SBStringList object.'''
        ...

class SBStructuredData:
    r"""
    A class representing a StructuredData event.

    This class wraps the event type generated by StructuredData features.
    """
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBStructuredData):
        ...
    
    @overload
    def __init__( self, even_sp: EventSP):
        ...
        

    def IsValid(self) -> bool:
        ...


    def Clear(self) -> None:
        ...

    def GetType(self) -> StructuredDataType:
        ...

    def GetSize(self) -> SizeT:
        ...

    def GetKeys(self, keys: SBStringList) -> bool:
        ...

    def GetValueForKey(self, key: CharConstStar) -> SBStructuredData:
        ...

    def GetItemAtIndex(self, idx: SizeT) -> SBStructuredData:
        ...

    def GetIntegerValue(self, fail_value: UInt64T=0) -> UInt64T:
        ...

    def GetFloatValue(self, fail_value: float=0.0) -> float:
        ...

    def GetBooleanValue(self, fail_value: bool=False) -> bool:
        ...

    def GetStringValue(self, dst: CharStar) -> SizeT:
        ...

    def GetAsJSON(self, stream: SBStream) -> SBError:
        ...

    def GetDescription(self, stream: SBStream) -> SBError:
        ...
    
    @overload
    def SetFromJSON(self, stream: SBStream) -> SBError:
        ...
    
    @overload
    def SetFromJSON(self, json: CharConstStar) -> SBError:
        ...
        



class SBSymbol:
    r"""
    Represents the symbol possibly associated with a stack frame.
    :py:class:`SBModule` contains SBSymbol(s). SBSymbol can also be retrieved from :py:class:`SBFrame` .
    """
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBSymbol):
        ...
        

    def IsValid(self) -> bool:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetDisplayName(self) -> CharConstStar:
        ...

    def GetMangledName(self) -> CharConstStar:
        ...
    
    def GetInstructions(self, target: SBTarget , flavor_string: CharConstStar) -> SBInstructionList:
        ...

    def GetStartAddress(self) -> SBAddress:
        ...

    def GetEndAddress(self) -> SBAddress:
        ...

    def GetPrologueByteSize(self) -> UInt32T:
        ...

    def GetType(self) -> SymbolType:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def IsExternal(self) -> bool:
        ...

    def IsSynthetic(self) -> bool:
        ...

    def __eq__(self, rhs: SBSymbol) -> bool:
        ...

    def __ne__(self, rhs: SBSymbol) -> bool:
        ...

    def __str__(self) -> str:
        ...

    def get_instructions_from_current_target (self):
        ...

    name = property(GetName, None, doc='''A read only property that returns the name for this symbol as a string.''')
    mangled = property(GetMangledName, None, doc='''A read only property that returns the mangled (linkage) name for this symbol as a string.''')
    type = property(GetType, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eSymbolType") that represents the type of this symbol.''')
    addr = property(GetStartAddress, None, doc='''A read only property that returns an lldb object that represents the start address (lldb.SBAddress) for this symbol.''')
    end_addr = property(GetEndAddress, None, doc='''A read only property that returns an lldb object that represents the end address (lldb.SBAddress) for this symbol.''')
    prologue_size = property(GetPrologueByteSize, None, doc='''A read only property that returns the size in bytes of the prologue instructions as an unsigned integer.''')
    instructions = property(get_instructions_from_current_target, None, doc='''A read only property that returns an lldb object that represents the instructions (lldb.SBInstructionList) for this symbol.''')
    external = property(IsExternal, None, doc='''A read only property that returns a boolean value that indicates if this symbol is externally visiable (exported) from the module that contains it.''')
    synthetic = property(IsSynthetic, None, doc='''A read only property that returns a boolean value that indicates if this symbol was synthetically created from information in module that contains it.''')


class SBSymbolContext:
    r"""
    A context object that provides access to core debugger entities.

    Many debugger functions require a context when doing lookups. This class
    provides a common structure that can be used as the result of a query that
    can contain a single result.

    For example, ::

            exe = os.path.join(os.getcwd(), 'a.out')

            # Create a target for the debugger.
            target = self.dbg.CreateTarget(exe)

            # Now create a breakpoint on main.c by name 'c'.
            breakpoint = target.BreakpointCreateByName('c', 'a.out')

            # Now launch the process, and do not stop at entry point.
            process = target.LaunchSimple(None, None, os.getcwd())

            # The inferior should stop on 'c'.
            from lldbutil import get_stopped_thread
            thread = get_stopped_thread(process, lldb.eStopReasonBreakpoint)
            frame0 = thread.GetFrameAtIndex(0)

            # Now get the SBSymbolContext from this frame.  We want everything. :-)
            context = frame0.GetSymbolContext(lldb.eSymbolContextEverything)

            # Get the module.
            module = context.GetModule()
            ...

            # And the compile unit associated with the frame.
            compileUnit = context.GetCompileUnit()
            ...

    """

    
    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBSymbolContext):
        ...
    

    def IsValid(self) -> bool:
        ...


    def GetModule(self) -> SBModule:
        ...

    def GetCompileUnit(self) -> SBCompileUnit:
        ...

    def GetFunction(self) -> SBFunction:
        ...

    def GetBlock(self) -> SBBlock:
        ...

    def GetLineEntry(self) -> SBLineEntry:
        ...

    def GetSymbol(self) -> SBSymbol:
        ...

    def SetModule(self, module: SBModule) -> None:
        ...

    def SetCompileUnit(self, compile_unit: SBCompileUnit) -> None:
        ...

    def SetFunction(self, function: SBFunction) -> None:
        ...

    def SetBlock(self, block: SBBlock) -> None:
        ...

    def SetLineEntry(self, line_entry: SBLineEntry) -> None:
        ...

    def SetSymbol(self, symbol: SBSymbol) -> None:
        ...

    def GetParentOfInlinedScope(self, curr_frame_pc: SBAddress, parent_frame_addr: SBAddress) -> SBSymbolContext:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def __str__(self) -> str:
        ...

    module = property(GetModule, SetModule, doc='''A read/write property that allows the getting/setting of the module (lldb.SBModule) in this symbol context.''')
    compile_unit = property(GetCompileUnit, SetCompileUnit, doc='''A read/write property that allows the getting/setting of the compile unit (lldb.SBCompileUnit) in this symbol context.''')
    function = property(GetFunction, SetFunction, doc='''A read/write property that allows the getting/setting of the function (lldb.SBFunction) in this symbol context.''')
    block = property(GetBlock, SetBlock, doc='''A read/write property that allows the getting/setting of the block (lldb.SBBlock) in this symbol context.''')
    symbol = property(GetSymbol, SetSymbol, doc='''A read/write property that allows the getting/setting of the symbol (lldb.SBSymbol) in this symbol context.''')
    line_entry = property(GetLineEntry, SetLineEntry, doc='''A read/write property that allows the getting/setting of the line entry (lldb.SBLineEntry) in this symbol context.''')




class SBSymbolContextList:
    r"""
    Represents a list of symbol context object. See also SBSymbolContext.

    For example (from test/python_api/target/TestTargetAPI.py), ::

        def find_functions(self, exe_name):
            '''Exercise SBTaget.FindFunctions() API.'''
            exe = os.path.join(os.getcwd(), exe_name)

            # Create a target by the debugger.
            target = self.dbg.CreateTarget(exe)
            self.assertTrue(target, VALID_TARGET)

            list = lldb.SBSymbolContextList()
            num = target.FindFunctions('c', lldb.eFunctionNameTypeAuto, False, list)
            self.assertTrue(num == 1 and list.GetSize() == 1)

            for sc in list:
                self.assertTrue(sc.GetModule().GetFileSpec().GetFilename() == exe_name)
                self.assertTrue(sc.GetSymbol().GetName() == 'c')
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBSymbolContextList):
        ...
        
    def IsValid(self) -> bool:
        ...

    def GetSize(self) -> UInt32T:
        ...

    def GetContextAtIndex(self, idx: UInt32T) -> SBSymbolContext:
        ...
    
    @overload
    def Append(self, sc: SBSymbolContext) -> None:
        ...

    @overload
    def Append(self, sc_list: SBSymbolContextList) -> None:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def Clear(self) -> None:
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBSymbolContext]:
        '''Iterate over all symbol contexts in a lldb.SBSymbolContextList
        object.'''
        ...

    def __len__(self) -> int:
        ...

    def __getitem__(self, key) -> SBSymbolContext:
        ...

    def get_module_array(self) -> List[SBModule]:
        ...

    def get_compile_unit_array(self) -> List[SBCompileUnit]:
        ...

    def get_function_array(self) -> List[SBFunction]:
        ...

    def get_block_array(self) -> List[SBBlock]:
        ...

    def get_symbol_array(self) -> List[SBSymbol]:
        ...

    def get_line_entry_array(self) -> List[SBLineEntry]:
        ...

    modules = property(get_module_array, None, doc='''Returns a list() of lldb.SBModule objects, one for each module in each SBSymbolContext object in this list.''')
    compile_units = property(get_compile_unit_array, None, doc='''Returns a list() of lldb.SBCompileUnit objects, one for each compile unit in each SBSymbolContext object in this list.''')
    functions = property(get_function_array, None, doc='''Returns a list() of lldb.SBFunction objects, one for each function in each SBSymbolContext object in this list.''')
    blocks = property(get_block_array, None, doc='''Returns a list() of lldb.SBBlock objects, one for each block in each SBSymbolContext object in this list.''')
    line_entries = property(get_line_entry_array, None, doc='''Returns a list() of lldb.SBLineEntry objects, one for each line entry in each SBSymbolContext object in this list.''')
    symbols = property(get_symbol_array, None, doc='''Returns a list() of lldb.SBSymbol objects, one for each symbol in each SBSymbolContext object in this list.''')




class SBTarget:
    r"""
    Represents the target program running under the debugger.

    SBTarget supports module, breakpoint, and watchpoint iterations. For example, ::

        for m in target.module_iter():
            print m

    produces: ::

        (x86_64) /Volumes/data/lldb/svn/trunk/test/python_api/lldbutil/iter/a.out
        (x86_64) /usr/lib/dyld
        (x86_64) /usr/lib/libstdc++.6.dylib
        (x86_64) /usr/lib/libSystem.B.dylib
        (x86_64) /usr/lib/system/libmathCommon.A.dylib
        (x86_64) /usr/lib/libSystem.B.dylib(__commpage)

    and, ::

        for b in target.breakpoint_iter():
            print b

    produces: ::

        SBBreakpoint: id = 1, file ='main.cpp', line = 66, locations = 1
        SBBreakpoint: id = 2, file ='main.cpp', line = 85, locations = 1

    and, ::

        for wp_loc in target.watchpoint_iter():
            print wp_loc

    produces: ::

        Watchpoint 1: addr = 0x1034ca048 size = 4 state = enabled type = rw
            declare @ '/Volumes/data/lldb/svn/trunk/test/python_api/watchpoint/main.c:12'
            hw_index = 0  hit_count = 2     ignore_count = 0
    """
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBTarget):
        ...
    

    @staticmethod
    def GetBroadcasterClassName() -> CharConstStar:
        ...

    def IsValid(self) -> bool:
        ...

    @staticmethod
    def EventIsTargetEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetTargetFromEvent(event: SBEvent) -> SBTarget:
        ...

    @staticmethod
    def GetNumModulesFromEvent(event: SBEvent) -> UInt32T:
        ...

    @staticmethod
    def GetModuleAtIndexFromEvent(idx: UInt32T, event: SBEvent) -> SBModule:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetPlatform(self) -> SBPlatform:
        r"""
            Return the platform object associated with the target.

            After return, the platform object should be checked for
            validity.

            @return
                A platform object.
        """
        ...

    def Install(self) -> SBError:
        r"""
            Install any binaries that need to be installed.

            This function does nothing when debugging on the host system.
            When connected to remote platforms, the target's main executable
            and any modules that have their install path set will be
            installed on the remote platform. If the main executable doesn't
            have an install location set, it will be installed in the remote
            platform's working directory.

            @return
                An error describing anything that went wrong during
                installation.
        """
        ...

    def LaunchSimple(self, argv: CharConstStarStar, envp: CharConstStarStar, working_directory: CharConstStar) -> SBProcess:
        r"""
            Launch a new process with sensible defaults.

            :param argv: The argument array.
            :param envp: The environment array.
            :param working_directory: The working directory to have the child process run in
            :return: The newly created process.
            :rtype: SBProcess

            A pseudo terminal will be used as stdin/stdout/stderr.
            No launch flags are passed and the target's debuger is used as a listener.

            For example, ::

                process = target.LaunchSimple(['X', 'Y', 'Z'], None, os.getcwd())

            launches a new process by passing 'X', 'Y', 'Z' as the args to the
            executable.
        """
        ...
    
    @overload
    def Launch(self, listener: SBListener, argv: CharConstStarStar, envp: CharConstStarStar , stdin_path: CharConstStar, stdout_path: CharConstStar , stderr_path: CharConstStar , working_directory: CharConstStar, launch_flags: UInt32T , stop_at_entry: bool, error: SBError) -> SBProcess:
        ...
    
    @overload
    def Launch(self, launch_info: SBLaunchInfo, error: SBError) -> SBProcess:
        ...
    
    def Launch(self, *args) -> SBProcess:
        
        r"""
            Launch a new process.

            Launch a new process by spawning a new process using the
            target object's executable module's file as the file to launch.
            Arguments are given in argv, and the environment variables
            are in envp. Standard input and output files can be
            optionally re-directed to stdin_path, stdout_path, and
            stderr_path.

            @param[in] listener
                An optional listener that will receive all process events.
                If listener is valid then listener will listen to all
                process events. If not valid, then this target's debugger
                (SBTarget::GetDebugger()) will listen to all process events.

            @param[in] argv
                The argument array.

            @param[in] envp
                The environment array.

            @param[in] launch_flags
                Flags to modify the launch (@see lldb::LaunchFlags)

            @param[in] stdin_path
                The path to use when re-directing the STDIN of the new
                process. If all stdXX_path arguments are NULL, a pseudo
                terminal will be used.

            @param[in] stdout_path
                The path to use when re-directing the STDOUT of the new
                process. If all stdXX_path arguments are NULL, a pseudo
                terminal will be used.

            @param[in] stderr_path
                The path to use when re-directing the STDERR of the new
                process. If all stdXX_path arguments are NULL, a pseudo
                terminal will be used.

            @param[in] working_directory
                The working directory to have the child process run in

            @param[in] launch_flags
                Some launch options specified by logical OR'ing
                lldb::LaunchFlags enumeration values together.

            @param[in] stop_at_entry
                If false do not stop the inferior at the entry point.

            @param[out]
                An error object. Contains the reason if there is some failure.

            @return
                 A process object for the newly created process.

            For example,

                process = target.Launch(self.dbg.GetListener(), None, None,
                                        None, '/tmp/stdout.txt', None,
                                        None, 0, False, error)

            launches a new process by passing nothing for both the args and the envs
            and redirect the standard output of the inferior to the /tmp/stdout.txt
            file. It does not specify a working directory so that the debug server
            will use its idea of what the current working directory is for the
            inferior. Also, we ask the debugger not to stop the inferior at the
            entry point. If no breakpoint is specified for the inferior, it should
            run to completion if no user interaction is required.
        """
        ...
    
    
    def LoadCore(self, core_file: CharConstStar , error: SBError = None) -> SBProcess:
        r"""
            Load a core file

            @param[in] core_file
                File path of the core dump.

            @param[out] error
                An error explaining what went wrong if the operation fails.
                (Optional)

            @return
                 A process object for the newly created core file.

            For example,

                process = target.LoadCore('./a.out.core')

            loads a new core file and returns the process object.
        """
        ...

    def Attach(self, attach_info: SBAttachInfo, error: SBError) -> SBProcess:
        ...

    def AttachToProcessWithID(self, listener: SBListener, pid: PIDT, error: SBError) -> SBProcess:
        r"""
            Attach to process with pid.

            @param[in] listener
                An optional listener that will receive all process events.
                If listener is valid then listener will listen to all
                process events. If not valid, then this target's debugger
                (SBTarget::GetDebugger()) will listen to all process events.

            @param[in] pid
                The process ID to attach to.

            @param[out]
                An error explaining what went wrong if attach fails.

            @return
                 A process object for the attached process.
        """
        ...

    def AttachToProcessWithName(self, listener: SBListener, name: CharConstStar, wait_for: bool, error: SBError) -> SBProcess:
        r"""
            Attach to process with name.

            @param[in] listener
                An optional listener that will receive all process events.
                If listener is valid then listener will listen to all
                process events. If not valid, then this target's debugger
                (SBTarget::GetDebugger()) will listen to all process events.

            @param[in] name
                Basename of process to attach to.

            @param[in] wait_for
                If true wait for a new instance of 'name' to be launched.

            @param[out]
                An error explaining what went wrong if attach fails.

            @return
                 A process object for the attached process.
        """
        ...

    def ConnectRemote(self, listener: SBListener, url: CharConstStar, plugin_name: CharConstStar, error: SBError) -> SBProcess:
        r"""
            Connect to a remote debug server with url.

            @param[in] listener
                An optional listener that will receive all process events.
                If listener is valid then listener will listen to all
                process events. If not valid, then this target's debugger
                (SBTarget::GetDebugger()) will listen to all process events.

            @param[in] url
                The url to connect to, e.g., 'connect://localhost:12345'.

            @param[in] plugin_name
                The plugin name to be used; can be NULL.

            @param[out]
                An error explaining what went wrong if the connect fails.

            @return
                 A process object for the connected process.
        """
        ...

    def GetExecutable(self) -> SBFileSpec:
        ...

    def AppendImageSearchPath(self, _from: CharConstStar, to: CharConstStar, error: SBError) -> None:
        r"""
            Append the path mapping (from -> to) to the target's paths mapping list.
        """
        ...
    
    @overload
    def AddModule(self, module: SBModule) -> SBModule:
        ...
    @overload
    def AddModule(self, path: CharConstStar, triple: CharConstStar, uuid: CharConstStar) -> SBModule:
        ...
        
    @overload
    def AddModule(self, path: CharConstStar, triple: CharConstStar,  uuid_cstr: CharConstStar, symfile: CharConstStar) -> SBModule:
        ...
    @overload
    def AddModule(self, module_spec: SBModuleSpec):
        ...

    def GetNumModules(self) -> UInt32T:
        ...

    def GetModuleAtIndex(self, idx: UInt32T) -> SBModule:
        ...

    def RemoveModule(self, module: SBModule) -> bool:
        ...

    def GetDebugger(self) -> SBDebugger:
        ...

    def FindModule(self, file_spec: SBFileSpec) -> SBModule:
        ...

    def FindCompileUnits(self, sb_file_spec: SBFileSpec) -> SBSymbolContextList:
        r"""
            Find compile units related to this target and passed source
            file.

            :param sb_file_spec: A :py:class:`lldb::SBFileSpec` object that contains source file
                specification.
            :return: The symbol contexts for all the matches.
            :rtype: SBSymbolContextList
        """
        ...

    def GetByteOrder(self) -> ByteOrder:
        ...

    def GetAddressByteSize(self) -> UInt32T:
        ...

    def GetTriple(self) -> CharConstStar:
        ...

    def GetDataByteSize(self) -> UInt32T:
        r"""
            Architecture data byte width accessor

            :return: The size in 8-bit (host) bytes of a minimum addressable unit from the Architecture's data bus.
        """
        ...

    def GetCodeByteSize(self) -> UInt32T:
        r"""
            Architecture code byte width accessor.

            :return: The size in 8-bit (host) bytes of a minimum addressable unit from the Architecture's code bus.
        """
        ...

    def SetSectionLoadAddress(self, section: SBSection, section_base_addr: AddrT) -> SBError:
        ...

    def ClearSectionLoadAddress(self, section: SBSection) -> SBError:
        ...

    def SetModuleLoadAddress(self, module: SBModule, sections_offset: Int64T) -> SBError:
        ...

    def ClearModuleLoadAddress(self, module: SBModule) -> SBError:
        ...

    def FindFunctions(self, name: CharConstStar , name_type_mask : UInt32T = None) -> SBSymbolContextList:
        r"""
            Find functions by name.

            :param name: The name of the function we are looking for.

            :param name_type_mask:
                A logical OR of one or more FunctionNameType enum bits that
                indicate what kind of names should be used when doing the
                lookup. Bits include fully qualified names, base names,
                C++ methods, or ObjC selectors.
                See FunctionNameType for more details.

            :return:
                A lldb::SBSymbolContextList that gets filled in with all of
                the symbol contexts for all the matches.
        """
        ...

    def FindFirstType(self, type: CharConstStar) -> SBType:
        ...

    def FindTypes(self, type: CharConstStar) -> SBTypeList:
        ...

    def GetBasicType(self, type: BasicType) -> SBType:
        ...

    def GetSourceManager(self) -> SBSourceManager:
        ...

    def FindFirstGlobalVariable(self, name: CharConstStar) -> SBValue:
        r"""
            Find the first global (or static) variable by name.

            @param[in] name
                The name of the global or static variable we are looking
                for.

            @return
                An SBValue that gets filled in with the found variable (if any).
        """
        ...

    def FindGlobalVariables(self, name: CharConstStar, max_match: UInt32T , matchtype: MatchType) -> SBValueList:
        r"""
            Find global and static variables by name.

            @param[in] name
                The name of the global or static variable we are looking
                for.

            @param[in] max_matches
                Allow the number of matches to be limited to max_matches.

            @return
                A list of matched variables in an SBValueList.
        """
        ...

    def FindGlobalFunctions(self, name: CharConstStar, max_matches: UInt32T, matchtype: MatchType) -> SBSymbolContextList:
        ...

    def Clear(self) -> None:
        ...

    def ResolveFileAddress(self, file_addr: AddrT) -> SBAddress:
        r"""
            Resolve a current file address into a section offset address.

            @param[in] file_addr

            @return
                An SBAddress which will be valid if...
        """
        ...

    def ResolveLoadAddress(self, vm_addr: AddrT) -> SBAddress:
        ...

    def ResolvePastLoadAddress(self, stop_id: UInt32T, vm_addr: AddrT) -> SBAddress:
        ...

    def ResolveSymbolContextForAddress(self, addr: SBAddress, resolve_scope: UInt32T) -> SBSymbolContext:
        ...

    def ReadMemory(self, addr: SBAddress, buf: VoidStar, error: SBError) -> SizeT:
        r"""
            Read target memory. If a target process is running then memory
            is read from here. Otherwise the memory is read from the object
            files. For a target whose bytes are sized as a multiple of host
            bytes, the data read back will preserve the target's byte order.

            @param[in] addr
                A target address to read from.

            @param[out] buf
                The buffer to read memory into.

            @param[in] size
                The maximum number of host bytes to read in the buffer passed
                into this call

            @param[out] error
                Error information is written here if the memory read fails.

            @return
                The amount of data read in host bytes.
        """
        ...
    
    def BreakpointCreateByLocation(self, *args) -> SBBreakpoint:
        r"""
        BreakpointCreateByLocation(SBTarget self, char const * file, uint32_t line) -> SBBreakpoint
        BreakpointCreateByLocation(SBTarget self, SBFileSpec file_spec, uint32_t line) -> SBBreakpoint
        BreakpointCreateByLocation(SBTarget self, SBFileSpec file_spec, uint32_t line, lldb::addr_t offset) -> SBBreakpoint
        BreakpointCreateByLocation(SBTarget self, SBFileSpec file_spec, uint32_t line, lldb::addr_t offset, SBFileSpecList module_list) -> SBBreakpoint
        BreakpointCreateByLocation(SBTarget self, SBFileSpec file_spec, uint32_t line, uint32_t column, lldb::addr_t offset, SBFileSpecList module_list) -> SBBreakpoint
        BreakpointCreateByLocation(SBTarget self, SBFileSpec file_spec, uint32_t line, uint32_t column, lldb::addr_t offset, SBFileSpecList module_list, bool move_to_nearest_code) -> SBBreakpoint
        """
        ...

    def BreakpointCreateByName(self, *args) -> SBBreakpoint:
        r"""
        BreakpointCreateByName(SBTarget self, char const * symbol_name, char const * module_name=None) -> SBBreakpoint
        BreakpointCreateByName(SBTarget self, char const * symbol_name, uint32_t func_name_type, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        BreakpointCreateByName(SBTarget self, char const * symbol_name, uint32_t func_name_type, lldb::LanguageType symbol_language, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        """
        ...

    def BreakpointCreateByNames(self, *args) -> SBBreakpoint:
        r"""
        BreakpointCreateByNames(SBTarget self, char const ** symbol_name, uint32_t name_type_mask, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        BreakpointCreateByNames(SBTarget self, char const ** symbol_name, uint32_t name_type_mask, lldb::LanguageType symbol_language, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        BreakpointCreateByNames(SBTarget self, char const ** symbol_name, uint32_t name_type_mask, lldb::LanguageType symbol_language, lldb::addr_t offset, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        """
        ...

    def BreakpointCreateByRegex(self, *args) -> SBBreakpoint:
        r"""
        BreakpointCreateByRegex(SBTarget self, char const * symbol_name_regex, char const * module_name=None) -> SBBreakpoint
        BreakpointCreateByRegex(SBTarget self, char const * symbol_name_regex, lldb::LanguageType symbol_language, SBFileSpecList module_list, SBFileSpecList comp_unit_list) -> SBBreakpoint
        """
        ...

    def BreakpointCreateBySourceRegex(self, *args) -> SBBreakpoint:
        r"""
        BreakpointCreateBySourceRegex(SBTarget self, char const * source_regex, SBFileSpec source_file, char const * module_name=None) -> SBBreakpoint
        BreakpointCreateBySourceRegex(SBTarget self, char const * source_regex, SBFileSpecList module_list, SBFileSpecList file_list) -> SBBreakpoint
        BreakpointCreateBySourceRegex(SBTarget self, char const * source_regex, SBFileSpecList module_list, SBFileSpecList source_file, SBStringList func_names) -> SBBreakpoint
        """
        ...

    def BreakpointCreateForException(self, language: LanguageType, catch_bp: bool, throw_bp: bool) -> SBBreakpoint:
        ...

    def BreakpointCreateByAddress(self, address: AddrT) -> SBBreakpoint:
        ...

    def GetEnvironment(self) -> SBEnvironment:
        ...

    def BreakpointCreateBySBAddress(self, sb_address: SBAddress) -> SBBreakpoint:
        ...

    def BreakpointCreateFromScript(self, class_name: CharConstStar, extra_args: SBStructuredData, module_list: SBFileSpecList, file_list: SBFileSpecList, request_hardware: bool=False) -> SBBreakpoint:
        r"""
            Create a breakpoint using a scripted resolver.

            @param[in] class_name
               This is the name of the class that implements a scripted resolver.
               The class should have the following signature: ::

                   class Resolver:
                       def __init__(self, bkpt, extra_args):
                           # bkpt - the breakpoint for which this is the resolver.  When
                           # the resolver finds an interesting address, call AddLocation
                           # on this breakpoint to add it.
                           #
                           # extra_args - an SBStructuredData that can be used to
                           # parametrize this instance.  Same as the extra_args passed
                           # to BreakpointCreateFromScript.

                       def __get_depth__ (self):
                           # This is optional, but if defined, you should return the
                           # depth at which you want the callback to be called.  The
                           # available options are:
                           #    lldb.eSearchDepthModule
                           #    lldb.eSearchDepthCompUnit
                           # The default if you don't implement this method is
                           # eSearchDepthModule.

                       def __callback__(self, sym_ctx):
                           # sym_ctx - an SBSymbolContext that is the cursor in the
                           # search through the program to resolve breakpoints.
                           # The sym_ctx will be filled out to the depth requested in
                           # __get_depth__.
                           # Look in this sym_ctx for new breakpoint locations,
                           # and if found use bkpt.AddLocation to add them.
                           # Note, you will only get called for modules/compile_units that
                           # pass the SearchFilter provided by the module_list & file_list
                           # passed into BreakpointCreateFromScript.

                       def get_short_help(self):
                           # Optional, but if implemented return a short string that will
                           # be printed at the beginning of the break list output for the
                           # breakpoint.

            @param[in] extra_args
               This is an SBStructuredData object that will get passed to the
               constructor of the class in class_name.  You can use this to
               reuse the same class, parametrizing it with entries from this
               dictionary.

            @param module_list
               If this is non-empty, this will be used as the module filter in the
               SearchFilter created for this breakpoint.

            @param file_list
               If this is non-empty, this will be used as the comp unit filter in the
               SearchFilter created for this breakpoint.

            @return
                An SBBreakpoint that will set locations based on the logic in the
                resolver's search callback.
        """
        ...

    def GetNumBreakpoints(self) -> UInt32T:
        ...

    def GetBreakpointAtIndex(self, idx: UInt32T) -> SBBreakpoint:
        ...

    def BreakpointDelete(self, break_id: BreakIdT) -> bool:
        ...

    def FindBreakpointByID(self, break_id: BreakIdT) -> SBBreakpoint:
        ...

    def FindBreakpointsByName(self, name: CharConstStar, bkpt_list: SBBreakpointList) -> bool:
        ...

    def DeleteBreakpointName(self, name: CharConstStar) -> None:
        ...

    def GetBreakpointNames(self, names: SBStringList) -> None:
        ...

    def EnableAllBreakpoints(self) -> bool:
        ...

    def DisableAllBreakpoints(self) -> bool:
        ...

    def DeleteAllBreakpoints(self) -> bool:
        ...
    
    @overload
    def BreakpointsCreateFromFile(self, source_file: SBFileSpec, breakpoint_list: SBBreakpointList) -> SBError:
        ...

    @overload
    def BreakpointsCreateFromFile(self, source_file: SBFileSpec, matching_names: SBStringList,  new_breakpionts: SBBreakpointList) -> SBError:
        ...
    def BreakpointsCreateFromFile(self, *args) -> SBError:
        r"""
            Read breakpoints from source_file and return the newly created
            breakpoints in bkpt_list.

            @param[in] source_file
               The file from which to read the breakpoints

            @param[in] matching_names
               Only read in breakpoints whose names match one of the names in this
               list.

            @param[out] bkpt_list
               A list of the newly created breakpoints.

            @return
                An SBError detailing any errors in reading in the breakpoints.
        """
        ...

    def BreakpointsWriteToFile(self, *args) -> SBError:
        r"""
        BreakpointsWriteToFile(SBTarget self, SBFileSpec dest_file) -> SBError
        BreakpointsWriteToFile(SBTarget self, SBFileSpec dest_file, SBBreakpointList bkpt_list, bool append=False) -> SBError
        """
        ...

    def GetNumWatchpoints(self) -> UInt32T:
        ...

    def GetWatchpointAtIndex(self, idx: UInt32T) -> SBWatchpoint:
        ...

    def DeleteWatchpoint(self, watch_id: WatchIdT) -> bool:
        ...

    def FindWatchpointByID(self, watch_id: WatchIdT) -> SBWatchpoint:
        ...

    def EnableAllWatchpoints(self) -> bool:
        ...

    def DisableAllWatchpoints(self) -> bool:
        ...

    def DeleteAllWatchpoints(self) -> bool:
        ...

    def WatchAddress(self, addr: AddrT, size: SizeT, read: bool, write: bool, error: SBError) -> SBWatchpoint:
        ...

    def GetBroadcaster(self) -> SBBroadcaster:
        ...

    def CreateValueFromAddress(self, name: CharConstStar, addr: SBAddress, type: SBType) -> SBValue:
        r"""
            Create an SBValue with the given name by treating the memory starting at addr as an entity of type.

            @param[in] name
                The name of the resultant SBValue

            @param[in] addr
                The address of the start of the memory region to be used.

            @param[in] type
                The type to use to interpret the memory starting at addr.

            @return
                An SBValue of the given type, may be invalid if there was an error reading
                the underlying memory.
        """
        ...

    def CreateValueFromData(self, name: CharConstStar, data: SBData, type: SBType) -> SBValue:
        ...

    def CreateValueFromExpression(self, name: CharConstStar, expr: CharConstStar) -> SBValue:
        ...

    def ReadInstructions(self, *args) -> SBInstructionList:
        r"""
        ReadInstructions(SBTarget self, SBAddress base_addr, uint32_t count) -> SBInstructionList
        ReadInstructions(SBTarget self, SBAddress base_addr, uint32_t count, char const * flavor_string) -> SBInstructionList

            Disassemble a specified number of instructions starting at an address.

            :param base_addr: the address to start disassembly from.
            :param count: the number of instructions to disassemble.
            :param flavor_string: may be 'intel' or 'att' on x86 targets to specify that style of disassembly.
            :rtype: SBInstructionList

        """
        ...

    def GetInstructions(self, base_addr: SBAddress, buf: VoidConstStar) -> SBInstructionList:
        r"""
            Disassemble the bytes in a buffer and return them in an SBInstructionList.

            :param base_addr: used for symbolicating the offsets in the byte stream when disassembling.
            :param buf: bytes to be disassembled.
            :param size: (C++) size of the buffer.
            :rtype: SBInstructionList

        """
        ...

    def GetInstructionsWithFlavor(self, base_addr: SBAddress, flavor_string: CharConstStar, buf: VoidConstStar) -> SBInstructionList:
        r"""
            Disassemble the bytes in a buffer and return them in an SBInstructionList, with a supplied flavor.

            :param base_addr: used for symbolicating the offsets in the byte stream when disassembling.
            :param flavor:  may be 'intel' or 'att' on x86 targets to specify that style of disassembly.
            :param buf: bytes to be disassembled.
            :param size: (C++) size of the buffer.
            :rtype: SBInstructionList

        """
        ...

    def FindSymbols(self, *args) -> SBSymbolContextList:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def GetStackRedZoneSize(self) -> AddrT:
        ...

    def IsLoaded(self, module: SBModule) -> bool:
        r"""
            Returns true if the module has been loaded in this `SBTarget`.
            A module can be loaded either by the dynamic loader or by being manually
            added to the target (see `SBTarget.AddModule` and the `target module add` command).

            :rtype: bool

        """
        ...

    def GetLaunchInfo(self) -> SBLaunchInfo:
        ...

    def SetLaunchInfo(self, launch_info: SBLaunchInfo) -> None:
        ...

    def SetCollectingStats(self, v: bool) -> None:
        ...

    def GetCollectingStats(self) -> bool:
        ...

    def GetStatistics(self) -> SBStructuredData:
        ...

    def __eq__(self, rhs: SBTarget) -> bool:
        ...

    def __ne__(self, rhs: SBTarget) -> bool:
        ...
    
    
    def EvaluateExpression(self, expr: CharConstStar , options: SBExpressionOptions = None) -> SBValue:
        ...

    def __str__(self) -> str:
        ...

    def GetTrace(self) -> SBTrace:
        ...

    def CreateTrace(self, error: SBError) -> SBTrace:
        ...

    class modules_access:
        '''A helper object that will lazily hand out lldb.SBModule objects for a target when supplied an index, or by full or partial path.'''
        def __init__(self, sbtarget):
            self.sbtarget = sbtarget

        def __len__(self):
            if self.sbtarget:
                return int(self.sbtarget.GetNumModules())
            return 0

        def __getitem__(self, key) -> Optional[SBModule]:
            ...

    def get_modules_access_object(self) -> modules_access:
        '''An accessor function that returns a modules_access() object which allows lazy module access from a lldb.SBTarget object.'''
        ...

    def get_modules_array(self) -> List[SBModule]:
        '''An accessor function that returns a list() that contains all modules in a lldb.SBTarget object.'''
        ...

    def module_iter(self) -> Iterable[ SBModule]:
        '''Returns an iterator over all modules in a lldb.SBTarget
        object.'''
        ...

    def breakpoint_iter(self) -> Iterable[SBBreakpoint]:
        '''Returns an iterator over all breakpoints in a lldb.SBTarget
        object.'''
        ...

    def watchpoint_iter(self) -> Iterable[SBWatchpoint]:
        '''Returns an iterator over all watchpoints in a lldb.SBTarget
        object.'''
        ...

    modules = property(get_modules_array, None, doc='''A read only property that returns a list() of lldb.SBModule objects contained in this target. This list is a list all modules that the target currently is tracking (the main executable and all dependent shared libraries).''')
    module = property(get_modules_access_object, None, doc=r'''A read only property that returns an object that implements python operator overloading with the square brackets().\n    target.module[<int>] allows array access to any modules.\n    target.module[<str>] allows access to modules by basename, full path, or uuid string value.\n    target.module[uuid.UUID()] allows module access by UUID.\n    target.module[re] allows module access using a regular expression that matches the module full path.''')
    process = property(GetProcess, None, doc='''A read only property that returns an lldb object that represents the process (lldb.SBProcess) that this target owns.''')
    executable = property(GetExecutable, None, doc='''A read only property that returns an lldb object that represents the main executable module (lldb.SBModule) for this target.''')
    debugger = property(GetDebugger, None, doc='''A read only property that returns an lldb object that represents the debugger (lldb.SBDebugger) that owns this target.''')
    num_breakpoints = property(GetNumBreakpoints, None, doc='''A read only property that returns the number of breakpoints that this target has as an integer.''')
    num_watchpoints = property(GetNumWatchpoints, None, doc='''A read only property that returns the number of watchpoints that this target has as an integer.''')
    broadcaster = property(GetBroadcaster, None, doc='''A read only property that an lldb object that represents the broadcaster (lldb.SBBroadcaster) for this target.''')
    byte_order = property(GetByteOrder, None, doc='''A read only property that returns an lldb enumeration value (lldb.eByteOrderLittle, lldb.eByteOrderBig, lldb.eByteOrderInvalid) that represents the byte order for this target.''')
    addr_size = property(GetAddressByteSize, None, doc='''A read only property that returns the size in bytes of an address for this target.''')
    triple = property(GetTriple, None, doc='''A read only property that returns the target triple (arch-vendor-os) for this target as a string.''')
    data_byte_size = property(GetDataByteSize, None, doc='''A read only property that returns the size in host bytes of a byte in the data address space for this target.''')
    code_byte_size = property(GetCodeByteSize, None, doc='''A read only property that returns the size in host bytes of a byte in the code address space for this target.''')
    platform = property(GetPlatform, None, doc='''A read only property that returns the platform associated with with this target.''')


class SBThread:
    r"""
    Represents a thread of execution. :py:class:`SBProcess` contains SBThread(s).

    SBThreads can be referred to by their ID, which maps to the system specific thread
    identifier, or by IndexID.  The ID may or may not be unique depending on whether the
    system reuses its thread identifiers.  The IndexID is a monotonically increasing identifier
    that will always uniquely reference a particular thread, and when that thread goes
    away it will not be reused.

    SBThread supports frame iteration. For example (from test/python_api/
    lldbutil/iter/TestLLDBIterator.py), ::

            from lldbutil import print_stacktrace
            stopped_due_to_breakpoint = False
            for thread in process:
                if self.TraceOn():
                    print_stacktrace(thread)
                ID = thread.GetThreadID()
                if thread.GetStopReason() == lldb.eStopReasonBreakpoint:
                    stopped_due_to_breakpoint = True
                for frame in thread:
                    self.assertTrue(frame.GetThread().GetThreadID() == ID)
                    if self.TraceOn():
                        print frame

            self.assertTrue(stopped_due_to_breakpoint)

    See also :py:class:`SBFrame` .
    """

    
    @overload
    def __init__(self, ):
        ...

    @overload
    def __init__(self, thread : SBThread):
        ...
        
    

    @staticmethod
    def GetBroadcasterClassName() -> CharConstStar:
        ...

    @staticmethod
    def EventIsThreadEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetStackFrameFromEvent(event: SBEvent) -> SBFrame:
        ...

    @staticmethod
    def GetThreadFromEvent(event: SBEvent) -> SBThread:
        ...

    def IsValid(self) -> bool:
        ...


    def Clear(self) -> None:
        ...

    def GetStopReason(self) -> StopReason:
        ...

    def GetStopReasonDataCount(self) -> SizeT:
        r"""
            Get the number of words associated with the stop reason.
            See also GetStopReasonDataAtIndex().
        """
        ...

    def GetStopReasonDataAtIndex(self, idx: UInt32T) -> UInt64T:
        r"""
            Get information associated with a stop reason.

            Breakpoint stop reasons will have data that consists of pairs of
            breakpoint IDs followed by the breakpoint location IDs (they always come
            in pairs).

            Stop Reason              Count Data Type
            ======================== ===== =========================================
            eStopReasonNone          0
            eStopReasonTrace         0
            eStopReasonBreakpoint    N     duple: {breakpoint id, location id}
            eStopReasonWatchpoint    1     watchpoint id
            eStopReasonSignal        1     unix signal number
            eStopReasonException     N     exception data
            eStopReasonExec          0
            eStopReasonFork          1     pid of the child process
            eStopReasonVFork         1     pid of the child process
            eStopReasonVForkDone     0
            eStopReasonPlanComplete  0
        """
        ...

    def GetStopReasonExtendedInfoAsJSON(self, stream: SBStream) -> bool:
        r"""
        Collects a thread's stop reason extended information dictionary and prints it
        into the SBStream in a JSON format. The format of this JSON dictionary depends
        on the stop reason and is currently used only for instrumentation plugins.
        """
        ...

    def GetStopReasonExtendedBacktraces(self, type: InstrumentationRuntimeType) -> SBThreadCollection:
        r"""
        Returns a collection of historical stack traces that are significant to the
        current stop reason. Used by ThreadSanitizer, where we provide various stack
        traces that were involved in a data race or other type of detected issue.
        """
        ...

    def GetStopDescription(self, dst_or_null: CharStar) -> SizeT:
        r"""
        Pass only an (int)length and expect to get a Python string describing the
        stop reason.
        """
        ...

    def GetStopReturnValue(self) -> SBValue:
        ...

    def GetThreadID(self) -> TIdT:
        r"""
        Returns a unique thread identifier (type lldb::tid_t, typically a 64-bit type)
        for the current SBThread that will remain constant throughout the thread's
        lifetime in this process and will not be reused by another thread during this
        process lifetime.  On Mac OS X systems, this is a system-wide unique thread
        identifier; this identifier is also used by other tools like sample which helps
        to associate data from those tools with lldb.  See related GetIndexID.
        """
        ...

    def GetIndexID(self) -> UInt32T:
        r"""
        Return the index number for this SBThread.  The index number is the same thing
        that a user gives as an argument to 'thread select' in the command line lldb.
        These numbers start at 1 (for the first thread lldb sees in a debug session)
        and increments up throughout the process lifetime.  An index number will not be
        reused for a different thread later in a process - thread 1 will always be
        associated with the same thread.  See related GetThreadID.
        This method returns a uint32_t index number, takes no arguments.
        """
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetQueueName(self) -> CharConstStar:
        r"""
        Return the queue name associated with this thread, if any, as a str.
        For example, with a libdispatch (aka Grand Central Dispatch) queue.
        """
        ...

    def GetQueueID(self) -> QueueIdT:
        r"""
        Return the dispatch_queue_id for this thread, if any, as a lldb::QueueIdT.
        For example, with a libdispatch (aka Grand Central Dispatch) queue.
        """
        ...

    def GetInfoItemByPathAsString(self, path: CharConstStar, strm: SBStream) -> bool:
        r"""
            Takes a path string and a SBStream reference as parameters, returns a bool.
            Collects the thread's 'info' dictionary from the remote system, uses the path
            argument to descend into the dictionary to an item of interest, and prints
            it into the SBStream in a natural format.  Return bool is to indicate if
            anything was printed into the stream (true) or not (false).
        """
        ...

    def GetQueue(self) -> SBQueue:
        r"""
        Return the SBQueue for this thread.  If this thread is not currently associated
        with a libdispatch queue, the SBQueue object's IsValid() method will return false.
        If this SBThread is actually a HistoryThread, we may be able to provide QueueID
        and QueueName, but not provide an SBQueue.  Those individual attributes may have
        been saved for the HistoryThread without enough information to reconstitute the
        entire SBQueue at that time.
        This method takes no arguments, returns an SBQueue.
        """
        ...

    def StepOver(self, *args) -> None:
        ...

    def StepInto(self, *args) -> None:
        r"""
        StepInto(SBThread self, lldb::RunMode stop_other_threads=eOnlyDuringStepping)
        StepInto(SBThread self, char const * target_name, lldb::RunMode stop_other_threads=eOnlyDuringStepping)
            Step the current thread from the current source line to the line given by end_line, stopping if
            the thread steps into the function given by target_name.  If target_name is None, then stepping will stop
            in any of the places we would normally stop.
            Step the current thread from the current source line to the line given by end_line, stopping if
            the thread steps into the function given by target_name.  If target_name is None, then stepping will stop
            in any of the places we would normally stop.
        """
        ...

    def StepOut(self, *args) -> None:
        ...

    def StepOutOfFrame(self, *args) -> None:
        ...

    def StepInstruction(self, *args) -> None:
        ...

    def StepOverUntil(self, frame: SBFrame, file_spec: SBFileSpec, line: UInt32T) -> SBError:
        ...

    def StepUsingScriptedThreadPlan(self, *args) -> SBError:
        r"""
        StepUsingScriptedThreadPlan(SBThread self, char const * script_class_name) -> SBError
        StepUsingScriptedThreadPlan(SBThread self, char const * script_class_name, bool resume_immediately) -> SBError
        StepUsingScriptedThreadPlan(SBThread self, char const * script_class_name, SBStructuredData args_data, bool resume_immediately) -> SBError
        """
        ...

    def JumpToLine(self, file_spec: SBFileSpec, line: UInt32T) -> SBError:
        ...

    def RunToAddress(self, addr: AddrT , error: SBError) -> None:
        ...

    def ReturnFromFrame(self, frame: SBFrame, return_value: SBValue) -> SBError:
        r"""
        Force a return from the frame passed in (and any frames younger than it)
        without executing any more code in those frames.  If return_value contains
        a valid SBValue, that will be set as the return value from frame.  Note, at
        present only scalar return values are supported.
        """
        ...

    def UnwindInnermostExpression(self) -> SBError:
        r"""
        Unwind the stack frames from the innermost expression evaluation.
        This API is equivalent to 'thread return -x'.
        """
        ...

    def Suspend(self, error: SBError = None) -> bool:
        r"""
            LLDB currently supports process centric debugging which means when any
            thread in a process stops, all other threads are stopped. The Suspend()
            call here tells our process to suspend a thread and not let it run when
            the other threads in a process are allowed to run. So when
            SBProcess::Continue() is called, any threads that aren't suspended will
            be allowed to run. If any of the SBThread functions for stepping are
            called (StepOver, StepInto, StepOut, StepInstruction, RunToAddres), the
            thread will now be allowed to run and these functions will simply return.

            Eventually we plan to add support for thread centric debugging where
            each thread is controlled individually and each thread would broadcast
            its state, but we haven't implemented this yet.

            Likewise the SBThread::Resume() call will again allow the thread to run
            when the process is continued.

            Suspend() and Resume() functions are not currently reference counted, if
            anyone has the need for them to be reference counted, please let us
            know.
        """
        ...

    def Resume(self, error : SBError = None) -> bool:
        ...

    def IsSuspended(self) -> bool:
        ...

    def IsStopped(self) -> bool:
        ...

    def GetNumFrames(self) -> UInt32T:
        ...

    def GetFrameAtIndex(self, idx: UInt32T) -> SBFrame:
        ...

    def GetSelectedFrame(self) -> SBFrame:
        ...

    def SetSelectedFrame(self, frame_idx: UInt32T) -> SBFrame:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetDescription(self, description: SBStream , stop_format: bool) -> bool:
        r"""
            Get the description strings for this thread that match what the
            lldb driver will present, using the thread-format (stop_format==false)
            or thread-stop-format (stop_format = true).
        """
        ...


    def GetStatus(self, status: SBStream) -> bool:
        ...

    def __eq__(self, rhs: SBThread) -> bool:
        ...

    def __ne__(self, rhs: SBThread) -> bool:
        ...

    def GetExtendedBacktraceThread(self, type: CharConstStar) -> SBThread:
        r"""
        Given an argument of str to specify the type of thread-origin extended
        backtrace to retrieve, query whether the origin of this thread is
        available.  An SBThread is retured; SBThread.IsValid will return true
        if an extended backtrace was available.  The returned SBThread is not
        a part of the SBProcess' thread list and it cannot be manipulated like
        normal threads -- you cannot step or resume it, for instance -- it is
        intended to used primarily for generating a backtrace.  You may request
        the returned thread's own thread origin in turn.
        """
        ...

    def GetExtendedBacktraceOriginatingIndexID(self) -> UInt32T:
        r"""
        Takes no arguments, returns a uint32_t.
        If this SBThread is an ExtendedBacktrace thread, get the IndexID of the
        original thread that this ExtendedBacktrace thread represents, if
        available.  The thread that was running this backtrace in the past may
        not have been registered with lldb's thread index (if it was created,
        did its work, and was destroyed without lldb ever stopping execution).
        In that case, this ExtendedBacktrace thread's IndexID will be returned.
        """
        ...

    def GetCurrentException(self) -> SBValue:
        r"""

        Returns an SBValue object represeting the current exception for the thread,
        if there is any. Currently, this works for Obj-C code and returns an SBValue
        representing the NSException object at the throw site or that's currently
        being processes.
        """
        ...

    def GetCurrentExceptionBacktrace(self) -> SBThread:
        r"""

        Returns a historical (fake) SBThread representing the stack trace of an
        exception, if there is one for the thread. Currently, this works for Obj-C
        code, and can retrieve the throw-site backtrace of an NSException object
        even when the program is no longer at the throw site.
        """
        ...

    def SafeToCallFunctions(self) -> bool:
        r"""

        Takes no arguments, returns a bool.
        lldb may be able to detect that function calls should not be executed
        on a given thread at a particular point in time.  It is recommended that
        this is checked before performing an inferior function call on a given
        thread.
        """
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBFrame]:
        '''Iterate over all frames in a lldb.SBThread object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of frames in a lldb.SBThread object.'''
        ...

    class frames_access:
        '''A helper object that will lazily hand out frames for a thread when supplied an index.'''
        def __init__(self, sbthread):
            self.sbthread = sbthread

        def __len__(self) -> int:
            ...

        def __getitem__(self, key) -> Optional[SBFrame]:
            ...

    def get_frames_access_object(self) -> frames_access:
        '''An accessor function that returns a frames_access() object which allows lazy frame access from a lldb.SBThread object.'''
        ...

    def get_thread_frames(self) -> List[SBFrame]:
        '''An accessor function that returns a list() that contains all frames in a lldb.SBThread object.'''
        ...

    id = property(GetThreadID, None, doc='''A read only property that returns the thread ID as an integer.''')
    idx = property(GetIndexID, None, doc='''A read only property that returns the thread index ID as an integer. Thread index ID values start at 1 and increment as threads come and go and can be used to uniquely identify threads.''')
    return_value = property(GetStopReturnValue, None, doc='''A read only property that returns an lldb object that represents the return value from the last stop (lldb.SBValue) if we just stopped due to stepping out of a function.''')
    process = property(GetProcess, None, doc='''A read only property that returns an lldb object that represents the process (lldb.SBProcess) that owns this thread.''')
    num_frames = property(GetNumFrames, None, doc='''A read only property that returns the number of stack frames in this thread as an integer.''')
    frames = property(get_thread_frames, None, doc='''A read only property that returns a list() of lldb.SBFrame objects for all frames in this thread.''')
    frame = property(get_frames_access_object, None, doc='''A read only property that returns an object that can be used to access frames as an array ("frame_12 = lldb.thread.frame[12]").''')
    name = property(GetName, None, doc='''A read only property that returns the name of this thread as a string.''')
    queue = property(GetQueueName, None, doc='''A read only property that returns the dispatch queue name of this thread as a string.''')
    queue_id = property(GetQueueID, None, doc='''A read only property that returns the dispatch queue id of this thread as an integer.''')
    stop_reason = property(GetStopReason, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eStopReason") that represents the reason this thread stopped.''')
    is_suspended = property(IsSuspended, None, doc='''A read only property that returns a boolean value that indicates if this thread is suspended.''')
    is_stopped = property(IsStopped, None, doc='''A read only property that returns a boolean value that indicates if this thread is stopped but not exited.''')


class SBThreadCollection:
    r"""Represents a collection of SBThread objects."""

    
    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , rhs: SBThreadCollection):
        ...
    

    def IsValid(self) -> bool:
        ...



    def GetSize(self) -> SizeT:
        ...

    def GetThreadAtIndex(self, idx: SizeT) -> SBThread:
        ...



class SBThreadPlan:
    r"""
    Represents a plan for the execution control of a given thread.

    See also :py:class:`SBThread` and :py:class:`SBFrame`.
    """

    
    

    def __init__(self, *args):
        r"""
        __init__(SBThreadPlan self) -> SBThreadPlan
        __init__(SBThreadPlan self, SBThreadPlan threadPlan) -> SBThreadPlan
        __init__(SBThreadPlan self, lldb::ThreadPlanSP const & lldb_object_sp) -> SBThreadPlan
        __init__(SBThreadPlan self, SBThread thread, char const * class_name) -> SBThreadPlan
        """
        ...
    

    def IsValid(self, *args) -> bool:
        ...


    def Clear(self) -> None:
        ...

    def GetStopReason(self) -> StopReason:
        ...

    def GetStopReasonDataCount(self) -> SizeT:
        r"""
            Get the number of words associated with the stop reason.
            See also GetStopReasonDataAtIndex().
        """
        ...

    def GetStopReasonDataAtIndex(self, idx: UInt32T) -> UInt64T:
        r"""
            Get information associated with a stop reason.

            Breakpoint stop reasons will have data that consists of pairs of
            breakpoint IDs followed by the breakpoint location IDs (they always come
            in pairs).

            Stop Reason              Count Data Type
            ======================== ===== =========================================
            eStopReasonNone          0
            eStopReasonTrace         0
            eStopReasonBreakpoint    N     duple: {breakpoint id, location id}
            eStopReasonWatchpoint    1     watchpoint id
            eStopReasonSignal        1     unix signal number
            eStopReasonException     N     exception data
            eStopReasonExec          0
            eStopReasonFork          1     pid of the child process
            eStopReasonVFork         1     pid of the child process
            eStopReasonVForkDone     0
            eStopReasonPlanComplete  0
        """
        ...

    def GetThread(self) -> SBThread:
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def SetPlanComplete(self, success: bool) -> None:
        ...

    def IsPlanComplete(self) -> bool:
        ...

    def IsPlanStale(self) -> bool:
        ...

    def GetStopOthers(self) -> bool:
        r"""
        GetStopOthers(SBThreadPlan self) -> bool
        Return whether this plan will ask to stop other threads when it runs.
        """
        ...

    def SetStopOthers(self, stop_others: bool) -> None:
        ...

    def QueueThreadPlanForStepOverRange(self, start_address: SBAddress, range_size: AddrT) -> SBThreadPlan:
        ...

    def QueueThreadPlanForStepInRange(self, start_address: SBAddress, range_size: AddrT) -> SBThreadPlan:
        ...

    def QueueThreadPlanForStepOut(self, frame_idx_to_step_to: UInt32T, first_insn: bool=False) -> SBThreadPlan:
        ...

    def QueueThreadPlanForRunToAddress(self, address: SBAddress) -> SBThreadPlan:
        ...

    def QueueThreadPlanForStepScripted(self, *args) -> SBThreadPlan:
        r"""
        QueueThreadPlanForStepScripted(SBThreadPlan self, char const * script_class_name) -> SBThreadPlan
        QueueThreadPlanForStepScripted(SBThreadPlan self, char const * script_class_name, SBError error) -> SBThreadPlan
        QueueThreadPlanForStepScripted(SBThreadPlan self, char const * script_class_name, SBStructuredData args_data, SBError error) -> SBThreadPlan
        """
        ...



class SBTrace:
    r"""Represents a processor trace."""

    def __init__(self):
        ...

    def GetStartConfigurationHelp(self) -> CharConstStar:
        ...
    
    @overload
    def Start(self, configuration: SBStructuredData) -> SBError:
        ...
    @overload
    def Start(self, thread: SBThread, configuration: SBStructuredData) -> SBError:
        ...

    def Stop(self, thread: SBThread = None) -> SBError:
        ...


    def IsValid(self) -> bool:
        ...
    



class SBTypeMember:
    r"""Represents a member of a type."""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBTypeMember):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def GetName(self) -> CharConstStar:
        ...

    def GetType(self) -> SBType:
        ...

    def GetOffsetInBytes(self) -> UInt64T:
        ...

    def GetOffsetInBits(self) -> UInt64T:
        ...

    def IsBitfield(self) -> bool:
        ...

    def GetBitfieldSizeInBits(self) -> UInt32T:
        ...

    def __str__(self) -> str:
        ...

    name = property(GetName, None, doc='''A read only property that returns the name for this member as a string.''')
    type = property(GetType, None, doc='''A read only property that returns an lldb object that represents the type (lldb.SBType) for this member.''')
    byte_offset = property(GetOffsetInBytes, None, doc='''A read only property that returns offset in bytes for this member as an integer.''')
    bit_offset = property(GetOffsetInBits, None, doc='''A read only property that returns offset in bits for this member as an integer.''')
    is_bitfield = property(IsBitfield, None, doc='''A read only property that returns true if this member is a bitfield.''')
    bitfield_bit_size = property(GetBitfieldSizeInBits, None, doc='''A read only property that returns the bitfield size in bits for this member as an integer, or zero if this member is not a bitfield.''')




class SBTypeMemberFunction:
    r"""Represents a member function of a type."""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBTypeMemberFunction):
        ...
        
    

    def IsValid(self) -> bool:
        ...


    def GetName(self) -> CharConstStar:
        ...

    def GetDemangledName(self) -> CharConstStar:
        ...

    def GetMangledName(self) -> CharConstStar:
        ...

    def GetType(self) -> SBType:
        ...

    def GetReturnType(self) -> SBType:
        ...

    def GetNumberOfArguments(self) -> UInt32T:
        ...

    def GetArgumentTypeAtIndex(self, arg2: UInt32T) -> SBType:
        ...

    def GetKind(self) -> MemberFunctionKind:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __str__(self) -> str:
        ...



class SBType:
    r"""
    Represents a data type in lldb.

    The actual characteristics of each type are defined by the semantics of the
    programming language and the specific language implementation that was used
    to compile the target program. See the language-specific notes in the
    documentation of each method.

    SBType instances can be obtained by a variety of methods.
    `SBTarget.FindFirstType` and `SBModule.FindFirstType` can be used to create
    `SBType` representations of types in executables/libraries with debug
    information. For some languages such as C, C++ and Objective-C it is possible
    to create new types by evaluating expressions that define a new type.

    Note that most `SBType` properties are computed independently of any runtime
    information so for dynamic languages the functionality can be very limited.
    `SBValue` can be used to represent runtime values which then can be more
    accurately queried for certain information such as byte size.


    SBType supports the eq/ne operator. For example,::

        //main.cpp:

        class Task {
        public:
            int id;
            Task *next;
            Task(int i, Task *n):
                id(i),
                next(n)
            {}
        };

        int main (int argc, char const *argv[])
        {
            Task *task_head = new Task(-1, NULL);
            Task *task1 = new Task(1, NULL);
            Task *task2 = new Task(2, NULL);
            Task *task3 = new Task(3, NULL); // Orphaned.
            Task *task4 = new Task(4, NULL);
            Task *task5 = new Task(5, NULL);

            task_head->next = task1;
            task1->next = task2;
            task2->next = task4;
            task4->next = task5;

            int total = 0;
            Task *t = task_head;
            while (t != NULL) {
                if (t->id >= 0)
                    ++total;
                t = t->next;
            }
            printf('We have a total number of %d tasks\n', total);

            // This corresponds to an empty task list.
            Task *empty_task_head = new Task(-1, NULL);

            return 0; // Break at this line
        }

        # find_type.py:

                # Get the type 'Task'.
                task_type = target.FindFirstType('Task')
                self.assertTrue(task_type)

                # Get the variable 'task_head'.
                frame0.FindVariable('task_head')
                task_head_type = task_head.GetType()
                self.assertTrue(task_head_type.IsPointerType())

                # task_head_type is 'Task *'.
                task_pointer_type = task_type.GetPointerType()
                self.assertTrue(task_head_type == task_pointer_type)

                # Get the child mmember 'id' from 'task_head'.
                id = task_head.GetChildMemberWithName('id')
                id_type = id.GetType()

                # SBType.GetBasicType() takes an enum 'BasicType' (lldb-enumerations.h).
                int_type = id_type.GetBasicType(lldb.eBasicTypeInt)
                # id_type and int_type should be the same type!
                self.assertTrue(id_type == int_type)


    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBType):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def GetByteSize(self) -> UInt64T:
        r"""
        Returns the number of bytes a variable with the given types occupies in memory.

            Returns ``0`` if the size can't be determined.

            If a type occupies ``N`` bytes + ``M`` bits in memory, this function returns
            the rounded up amount of bytes (i.e., if ``M`` is ``0``,
            this function returns ``N`` and otherwise ``N + 1``).

            Language-specific behaviour:

            * C: The output is expected to match the value of ``sizeof(Type)``. If
              ``sizeof(Type)`` is not a valid expression for the given type, the
              function returns ``0``.
            * C++: Same as in C.
            * Objective-C: Same as in C. For Objective-C classes this always returns
              `0`` as the actual size depends on runtime information.

        """
        ...

    def IsPointerType(self) -> bool:
        r"""
        Returns true if this type is a pointer type.

            Language-specific behaviour:

            * C: Returns true for C pointer types (or typedefs of these types).
            * C++: Pointer types include the C pointer types as well as pointers to data
              mebers or member functions.
            * Objective-C: Pointer types include the C pointer types. ``id``, ``Class``
              and pointers to blocks are also considered pointer types.

        """
        ...

    def IsReferenceType(self) -> bool:
        r"""
        Returns true if this type is a reference type.

            Language-specific behaviour:

            * C: Returns false for all types.
            * C++: Both l-value and r-value references are considered reference types.
            * Objective-C: Returns false for all types.

        """
        ...

    def IsFunctionType(self) -> bool:
        ...

    def IsPolymorphicClass(self) -> bool:
        r"""
        Returns true if this type is a polymorphic type.

            Language-specific behaviour:

            * C: Returns false for all types.
            * C++: Returns true if the type is a class type that contains at least one
              virtual member function or if at least one of its base classes is
              considered a polymorphic type.
            * Objective-C: Returns false for all types.

        """
        ...

    def IsArrayType(self) -> bool:
        r"""
        Returns true if this type is an array type.

            Language-specific behaviour:

            * C: Returns true if the types is an array type. This includes incomplete
              array types ``T[]`` and array types with integer (``T[1]``) or variable
              length (``T[some_variable]``). Pointer types are not considered arrays.
            * C++: Includes C's array types and dependent array types (i.e., array types
              in templates which size depends on template arguments).
            * Objective-C: Same as in C.

        """
        ...

    def IsVectorType(self) -> bool:
        r"""
        Returns true if this type is a vector type.

            Language-specific behaviour:

            * C: Returns true if the types is a vector type created with
              GCC's ``vector_size`` or Clang's ``ext_vector_type`` feature.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def IsTypedefType(self) -> bool:
        r"""
        Returns true if this type is a typedef.

            Language-specific behaviour:

            * C: Returns true if the type is a C typedef.
            * C++: Same as in C. Also treats type aliases as typedefs.
            * Objective-C: Same as in C.

        """
        ...

    def IsAnonymousType(self) -> bool:
        r"""
        Returns true if this type is an anonymous type.

            Language-specific behaviour:

            * C: Returns true for anonymous unions. Also returns true for
              anonymous structs (which are a GNU language extension).
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def IsScopedEnumerationType(self) -> bool:
        r"""
        Returns true if this type is a scoped enum.

            Language-specific behaviour:

            * C: Returns false for all types.
            * C++: Return true only for C++11 scoped enums.
            * Objective-C: Returns false for all types.

        """
        ...

    def GetPointerType(self) -> SBType:
        r"""
        Returns a type that represents a pointer to this type.

            If the type system of the current language can't represent a pointer to this
            type or this type is invalid, an invalid `SBType` is returned.

            Language-specific behaviour:

            * C: Returns the pointer type of this type.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetPointeeType(self) -> SBType:
        r"""
        Returns the underlying pointee type.

            If this type is a pointer type as specified by `IsPointerType` then this
            returns the underlying type. If this is not a pointer type or an invalid
            `SBType` then this returns an invalid `SBType`.

            Language-specific behaviour:

            * C: Returns the underlying type for for C pointer types or typedefs of
              these types). For example, ``int *`` will return ``int``.
            * C++: Same as in C. Returns an `SBType` representation for data members/
              member functions in case the `SBType` is a pointer to data member or
              pointer to member function.
            * Objective-C: Same as in C. The pointee type of ``id`` and ``Class`` is
              an invalid `SBType`. The pointee type of pointers Objective-C types is an
              `SBType` for the non-pointer type of the respective type. For example,
              ``NSString *`` will return ``NSString`` as a pointee type.

        """
        ...

    def GetReferenceType(self) -> SBType:
        r"""
        Returns a type that represents a reference to this type.

            If the type system of the current language can't represent a reference to
            this type, an invalid `SBType` is returned.

            Language-specific behaviour:

            * C: Currently assumes the type system is C++ and returns an l-value
              reference type. For example, ``int`` will return ``int&``. This behavior
              is likely to change in the future and shouldn't be relied on.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetTypedefedType(self) -> SBType:
        r"""
        Returns the underlying type of a typedef.

            If this type is a typedef as designated by `IsTypedefType`, then the
            underlying type is being returned. Otherwise an invalid `SBType` is
            returned.

            Language-specific behaviour:

            * C: Returns the underlying type of a typedef type.
            * C++: Same as in C. For type aliases, the underlying type is returned.
            * Objective-C: Same as in C.

        """
        ...

    def GetDereferencedType(self) -> SBType:
        r"""
        Returns the underlying type of a reference type.

            If this type is a reference as designated by `IsReferenceType`, then the
            underlying type is being returned. Otherwise an invalid `SBType` is
            returned.

            Language-specific behaviour:

            * C: Always returns an invalid type.
            * C++: For l-value and r-value references the underlying type is returned.
              For example, ``int &`` will return ``int``.
            * Objective-C: Same as in C.

        """
        ...

    def GetUnqualifiedType(self) -> SBType:
        r"""
        Returns the unqualified version of this type.

            Language-specific behaviour:

            * C: If this type with any const or volatile specifier removed.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetCanonicalType(self) -> SBType:
        ...

    def GetEnumerationIntegerType(self) -> SBType:
        r"""
        Returns the underlying integer type if this is an enumeration type.

            If this type is an invalid `SBType` or not an enumeration type an invalid
            `SBType` is returned.

            Language-specific behaviour:

            * C: Returns the underlying type for enums.
            * C++: Same as in C but also returns the underlying type for scoped enums.
            * Objective-C: Same as in C.

        """
        ...

    def GetArrayElementType(self) -> SBType:
        r"""
        Returns the array element type if this type is an array type.

            Otherwise returns an invalid `SBType` if this type is invalid or not an
            array type.

            Language-specific behaviour:

            * C: If this is an array type (see `IsArrayType`) such as ``T[]``, returns
              the element type.
            * C++: Same as in C.
            * Objective-C: Same as in C.

            See also `IsArrayType`.

        """
        ...

    def GetArrayType(self, size: UInt64T) -> SBType:
        r"""
        Returns the array type with the given constant size.

            Language-specific behaviour:

            * C: Returns a constant-size array `T[size]` for any non-void type.
            * C++: Same as in C.
            * Objective-C: Same as in C.

            See also `IsArrayType` and `GetArrayElementType`.

        """
        ...

    def GetVectorElementType(self) -> SBType:
        r"""
        Returns the vector element type if this type is a vector type.

            Otherwise returns an invalid `SBType` if this type is invalid or not a
            vector type.

            Language-specific behaviour:

            * C: If this is a vector type (see `IsVectorType`), returns the element
              type.
            * C++: Same as in C.
            * Objective-C: Same as in C.

            See also `IsVectorType`.

        """
        ...
    

    def GetBasicType(self, type : BasicType = None) -> SBType:
        r"""
        GetBasicType(SBType self) -> lldb::BasicType
        GetBasicType(SBType self, lldb::BasicType type) -> SBType
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetNumberOfFields(self) -> UInt32T:
        r"""
        Returns the number of fields of this type.

            Returns ``0`` if this type does not have fields.

            Language-specific behaviour:

            * C: Returns the number of fields if the type is a struct. If the type
              contains an anonymous struct/union it only counts as a single field (even
              if the struct/union contains several fields).
            * C++: Returns the number of non-static fields if the type is a
              struct/class. If the type contains an anonymous struct/union it only
              counts as a single field (even if the struct/union contains several
              fields). The fields of any base classes are not included in the count.
            * Objective-C: Same as in C for structs. For Objective-C classes the number
              of ivars is returned.

            See also `GetFieldAtIndex`.

        """
        ...

    def GetNumberOfDirectBaseClasses(self) -> UInt32T:
        r"""
        Returns the number of base/parent classes of this type.

            Returns ``0`` if this type doesn't have any base classes.

            Language-specific behaviour:

            * C: Returns always ``0``.
            * C++: The number of direct non-virtual base classes if this type is
              a class.
            * Objective-C: The number of super classes for Objective-C classes.
              As Objective-C doesn't have multiple inheritance this is usually returns 1
              except for NSObject.

        """
        ...

    def GetNumberOfVirtualBaseClasses(self) -> UInt32T:
        r"""
        Returns the number of virtual base/parent classes of this type

            Returns ``0`` if this type doesn't have any base classes.

            Language-specific behaviour:

            * C: Returns always ``0``.
            * C++: The number of direct virtual base classes if this type is a
              class.
            * Objective-C: Returns always ``0``.

        """
        ...

    def GetFieldAtIndex(self, idx: UInt32T) -> SBTypeMember:
        r"""
        Returns the field at the given index.

            Returns an invalid `SBType` if the index is out of range or the current
            type doesn't have any fields.

            Language-specific behaviour:

            * C: Returns the field with the given index for struct types. Fields are
              ordered/indexed starting from ``0`` for the first field in a struct (as
              declared in the definition).
            * C++: Returns the non-static field with the given index for struct types.
              Fields are ordered/indexed starting from ``0`` for the first field in a
              struct (as declared in the definition).
            * Objective-C: Same as in C for structs. For Objective-C classes the ivar
              with the given index is returned. ivars are indexed starting from ``0``.

        """
        ...

    def GetDirectBaseClassAtIndex(self, idx: UInt32T) -> SBTypeMember:
        r"""
        Returns the direct base class as indexed by `GetNumberOfDirectBaseClasses`.

            Returns an invalid SBTypeMember if the index is invalid or this SBType is
            invalid.

        """
        ...

    def GetVirtualBaseClassAtIndex(self, idx: UInt32T) -> SBTypeMember:
        r"""
        Returns the virtual base class as indexed by
            `GetNumberOfVirtualBaseClasses`.

            Returns an invalid SBTypeMember if the index is invalid or this SBType is
            invalid.

        """
        ...

    def GetEnumMembers(self) -> SBTypeEnumMemberList:
        r"""
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetModule(self) -> SBModule:
        r"""
        Returns the `SBModule` this `SBType` belongs to.

            Returns no `SBModule` if this type does not belong to any specific
            `SBModule` or this `SBType` is invalid. An invalid `SBModule` might also
            indicate that once came from an `SBModule` but LLDB could no longer
            determine the original module.

        """
        ...

    def GetName(self) -> CharConstStar:
        r"""
        Returns the name of this type.

            Returns an empty string if an error occurred or this type is invalid.

            Use this function when trying to match a specific type by name in a script.
            The names returned by this function try to uniquely identify a name but
            conflicts can occur (for example, if a C++ program contains two different
            classes with the same name in different translation units. `GetName` can
            return the same name for both class types.)


            Language-specific behaviour:

            * C: The name of the type. For structs the ``struct`` prefix is omitted.
            * C++: Returns the qualified name of the type (including anonymous/inline
              namespaces and all template arguments).
            * Objective-C: Same as in C.

        """
        ...

    def GetDisplayTypeName(self) -> CharConstStar:
        r"""
        Returns the name of this type in a user-friendly format.

            Returns an empty string if an error occurred or this type is invalid.

            Use this function when displaying a type name to the user.

            Language-specific behaviour:

            * C: Returns the type name. For structs the ``struct`` prefix is omitted.
            * C++: Returns the qualified name. Anonymous/inline namespaces are omitted.
              Template arguments that match their default value might also be hidden
              (this functionality depends on whether LLDB can determine the template's
              default arguments).
            * Objective-C: Same as in C.

        """
        ...

    def GetTypeClass(self) -> TypeClass:
        r"""
        Returns the `TypeClass` for this type.

            Returns an `eTypeClassInvalid` if this `SBType` is invalid.

            See `TypeClass` for the language-specific meaning of each `TypeClass` value.

        """
        ...
    def GetNumberOfTemplateArguments(self) -> UInt32T:
        r"""
        Returns the number of template arguments of this type.

            Returns ``0`` if this type is not a template.

            Language-specific behaviour:

            * C: Always returns ``0``.
            * C++: If this type is a class template instantiation then this returns the
              number of template parameters that were used in this instantiation. This i
              cludes both explicit and implicit template parameters.
            * Objective-C: Always returns ``0``.

        """
        ...

    def GetTemplateArgumentType(self, idx: UInt32T) -> SBType:
        r"""
        Returns the type of the template argument with the given index.

            Returns an invalid `SBType` if there is no template argument with the given
            index or this type is not a template. The first template  argument has the
            index ``0``.

            Language-specific behaviour:

            * C: Always returns an invalid SBType.
            * C++: If this type is a class template instantiation and the template
              parameter with the given index is a type template parameter, then this
              returns the type of that parameter. Otherwise returns an invalid `SBType`.
            * Objective-C: Always returns an invalid SBType.

        """
        ...

    def GetTemplateArgumentKind(self, idx: UInt32T) -> TemplateArgumentKind:
        r"""
        Returns the kind of the template argument with the given index.

            Returns `eTemplateArgumentKindNull` if there is no template argument
            with the given index or this type is not a template. The first template
            argument has the index ``0``.

            Language-specific behaviour:

            * C: Always returns `eTemplateArgumentKindNull`.
            * C++: If this type is a class template instantiation then this returns
              the appropriate `TemplateArgument` value for the parameter with the given
              index. See the documentation of `TemplateArgument` for how certain C++
              template parameter kinds are mapped to `TemplateArgument` values.
            * Objective-C: Always returns `eTemplateArgumentKindNull`.

        """
        ...

    def GetFunctionReturnType(self) -> SBType:
        r"""
        Returns the return type if this type represents a function.

            Returns an invalid `SBType` if this type is not a function type or invalid.

            Language-specific behaviour:

            * C: For functions return the return type. Returns an invalid `SBType` if
              this type is a function pointer type.
            * C++: Same as in C for functions and instantiated template functions.
              Member functions are also considered functions. For functions that have
              their return type specified by a placeholder type specifier (``auto``)
              this returns the deduced return type.
            * Objective-C: Same as in C for functions. For Objective-C methods this
              returns the return type of the method.

        """
        ...

    def GetFunctionArgumentTypes(self) -> SBTypeList:
        r"""
        Returns the list of argument types if this type represents a function.

            Returns an invalid `SBType` if this type is not a function type or invalid.

            Language-specific behaviour:

            * C: For functions return the types of each parameter. Returns an invalid
              `SBType` if this type is a function pointer. For variadic functions this
              just returns the list of parameters before the variadic arguments.
            * C++: Same as in C for functions and instantiated template functions.
              Member functions are also considered functions.
            * Objective-C: Always returns an invalid SBType for Objective-C methods.

        """
        ...
    def GetNumberOfMemberFunctions(self) -> UInt32T:
        r"""
        Returns the number of member functions of this type.

            Returns ``0`` if an error occurred or this type is invalid.

            Language-specific behaviour:

            * C: Always returns ``0``.
            * C++: If this type represents a struct/class, then the number of
              member functions (static and non-static) is returned. The count includes
              constructors and destructors (both explicit and implicit). Member
              functions of base classes are not included in the count.
            * Objective-C: If this type represents a struct/class, then the
              number of methods is returned. Methods in categories or super classes
              are not counted.

        """
        ...

    def GetMemberFunctionAtIndex(self, idx: UInt32T) -> SBTypeMemberFunction:
        r"""
        Returns the member function of this type with the given index.

            Returns an invalid `SBTypeMemberFunction` if the index is invalid or this
            type is invalid.

            Language-specific behaviour:

            * C: Always returns an invalid `SBTypeMemberFunction`.
            * C++: Returns the member function or constructor/destructor with the given
              index.
            * Objective-C: Returns the method with the given index.

            See `GetNumberOfMemberFunctions` for what functions can be queried by this
            function.

        """
        ...
    def IsTypeComplete(self) -> bool:
        r"""
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def GetTypeFlags(self) -> UInt32T:
        r"""
        Returns the `TypeFlags` values for this type.

            See the respective `TypeFlags` values for what values can be set. Returns an
            integer in which each `TypeFlags` value is represented by a bit. Specific
            flags can be checked via Python's bitwise operators. For example, the
            `eTypeIsInteger` flag can be checked like this:

            ``(an_sb_type.GetTypeFlags() & lldb.eTypeIsInteger) != 0``

            If this type is invalid this returns ``0``.

            See the different values for `TypeFlags` for the language-specific meanings
            of each `TypeFlags` value.

        """
        ...

    def __eq__(self, rhs: SBType) -> bool:
        r"""
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def __ne__(self, rhs: SBType) -> bool:
        r"""
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def __str__(self) -> str:
        r"""
        __str__(SBType self) -> std::string
        Returns the `BasicType` value that is most appropriate to this type.

            Returns `eBasicTypeInvalid` if no appropriate `BasicType` was found or this
            type is invalid. See the `BasicType` documentation for the language-specific m
            aning of each `BasicType` value.

            **Overload behaviour:** When called with a `BasicType` parameter, the
            following behaviour applies:

            Returns the `SBType` that represents the passed `BasicType` value. Returns
            an invalid `SBType` if no fitting `SBType` could be created.

            Language-specific behaviour:

            * C: Returns the respective builtin type. Note that some types
              (e.g. ``__uint128_t``) might even be successfully created even if they are
              not available on the target platform. C++ and Objective-C specific types
              might also be created even if the target program is not written in C++ or
              Objective-C.
            * C++: Same as in C.
            * Objective-C: Same as in C.

        """
        ...

    def template_arg_array(self) -> Optional[List[TemplateArgumentKind]]:
        ...

    module = property(GetModule, None, doc='''A read only property that returns the module in which type is defined.''')
    name = property(GetName, None, doc='''A read only property that returns the name for this type as a string.''')
    size = property(GetByteSize, None, doc='''A read only property that returns size in bytes for this type as an integer.''')
    is_pointer = property(IsPointerType, None, doc='''A read only property that returns a boolean value that indicates if this type is a pointer type.''')
    is_reference = property(IsReferenceType, None, doc='''A read only property that returns a boolean value that indicates if this type is a reference type.''')
    is_reference = property(IsReferenceType, None, doc='''A read only property that returns a boolean value that indicates if this type is a function type.''')
    num_fields = property(GetNumberOfFields, None, doc='''A read only property that returns number of fields in this type as an integer.''')
    num_bases = property(GetNumberOfDirectBaseClasses, None, doc='''A read only property that returns number of direct base classes in this type as an integer.''')
    num_vbases = property(GetNumberOfVirtualBaseClasses, None, doc='''A read only property that returns number of virtual base classes in this type as an integer.''')
    num_template_args = property(GetNumberOfTemplateArguments, None, doc='''A read only property that returns number of template arguments in this type as an integer.''')
    template_args = property(template_arg_array, None, doc='''A read only property that returns a list() of lldb.SBType objects that represent all template arguments in this type.''')
    type = property(GetTypeClass, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eTypeClass") that represents a classification for this type.''')
    is_complete = property(IsTypeComplete, None, doc='''A read only property that returns a boolean value that indicates if this type is a complete type (True) or a forward declaration (False).''')

    def get_bases_array(self) -> List[SBTypeMember]:
        '''An accessor function that returns a list() that contains all direct base classes in a lldb.SBType object.'''
        ...

    def get_vbases_array(self) -> List[SBTypeMember]: 
        '''An accessor function that returns a list() that contains all fields in a lldb.SBType object.'''
        ...

    def get_fields_array(self) -> List[SBTypeMember]:
        '''An accessor function that returns a list() that contains all fields in a lldb.SBType object.'''
        ...

    def get_members_array(self) -> List[SBTypeMember]:
        '''An accessor function that returns a list() that contains all members (base classes and fields) in a lldb.SBType object in ascending bit offset order.'''
        ...

    def get_enum_members_array(self) -> List[SBTypeEnumMemberList]:
        '''An accessor function that returns a list() that contains all enum members in an lldb.SBType object.'''
        ...

    bases = property(get_bases_array, None, doc='''A read only property that returns a list() of lldb.SBTypeMember objects that represent all of the direct base classes for this type.''')
    vbases = property(get_vbases_array, None, doc='''A read only property that returns a list() of lldb.SBTypeMember objects that represent all of the virtual base classes for this type.''')
    fields = property(get_fields_array, None, doc='''A read only property that returns a list() of lldb.SBTypeMember objects that represent all of the fields for this type.''')
    members = property(get_members_array, None, doc='''A read only property that returns a list() of all lldb.SBTypeMember objects that represent all of the base classes, virtual base classes and fields for this type in ascending bit offset order.''')
    enum_members = property(get_enum_members_array, None, doc='''A read only property that returns a list() of all lldb.SBTypeEnumMember objects that represent the enum members for this type.''')




class SBTypeList:
    r"""
    Represents a list of :py:class:`SBType` s.

    The FindTypes() method of :py:class:`SBTarget`/:py:class:`SBModule` returns a SBTypeList.

    SBTypeList supports :py:class:`SBType` iteration. For example,

    .. code-block:: cpp

        // main.cpp:

        class Task {
        public:
            int id;
            Task *next;
            Task(int i, Task *n):
                id(i),
                next(n)
            {}
        };

    .. code-block:: python

        # find_type.py:

        # Get the type 'Task'.
        type_list = target.FindTypes('Task')
        self.assertTrue(len(type_list) == 1)
        # To illustrate the SBType iteration.
        for type in type_list:
            # do something with type


    """

    
    

    def __init__(self):
        ...

    def IsValid(self) -> bool:
        ...


    def Append(self, type: SBType) -> None:
        ...

    def GetTypeAtIndex(self, index: UInt32T) -> SBType:
        ...

    def GetSize(self) -> UInt32T:
        ...
    
    def __iter__(self) -> List[SBType]:
        '''Iterate over all types in a lldb.SBTypeList object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of types in a lldb.SBTypeList object.'''
        ...




class SBTypeCategory:
    r"""Represents a category that can contain formatters for types."""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBTypeCategory):
        ...


    def IsValid(self) -> bool:
        ...


    def GetEnabled(self) -> bool:
        ...

    def SetEnabled(self, arg2: bool) -> None:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetLanguageAtIndex(self, idx: UInt32T) -> LanguageType:
        ...

    def GetNumLanguages(self) -> UInt32T:
        ...

    def AddLanguage(self, language: LanguageType) -> None:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def GetNumFormats(self) -> UInt32T:
        ...

    def GetNumSummaries(self) -> UInt32T:
        ...

    def GetNumFilters(self) -> UInt32T:
        ...

    def GetNumSynthetics(self) -> UInt32T:
        ...

    def GetTypeNameSpecifierForFilterAtIndex(self, arg2: UInt32T) -> SBTypeNameSpecifier:
        ...

    def GetTypeNameSpecifierForFormatAtIndex(self, arg2: UInt32T) -> SBTypeNameSpecifier:
        ...

    def GetTypeNameSpecifierForSummaryAtIndex(self, arg2: UInt32T) -> SBTypeNameSpecifier:
        ...

    def GetTypeNameSpecifierForSyntheticAtIndex(self, arg2: UInt32T) -> SBTypeNameSpecifier:
        ...

    def GetFilterForType(self, arg2: SBTypeNameSpecifier) -> SBTypeFilter:
        ...

    def GetFormatForType(self, arg2: SBTypeNameSpecifier) -> SBTypeFormat:
        ...

    def GetSummaryForType(self, arg2: SBTypeNameSpecifier) -> SBTypeSummary:
        ...

    def GetSyntheticForType(self, arg2: SBTypeNameSpecifier) -> SBTypeSynthetic:
        ...

    def GetFilterAtIndex(self, arg2: UInt32T) -> SBTypeFilter:
        ...

    def GetFormatAtIndex(self, arg2: UInt32T) -> SBTypeFormat:
        ...

    def GetSummaryAtIndex(self, arg2: UInt32T) -> SBTypeSummary:
        ...

    def GetSyntheticAtIndex(self, arg2: UInt32T) -> SBTypeSynthetic:
        ...

    def AddTypeFormat(self, arg2: SBTypeNameSpecifier, arg3: SBTypeFormat) -> bool:
        ...

    def DeleteTypeFormat(self, arg2: SBTypeNameSpecifier) -> bool:
        ...

    def AddTypeSummary(self, arg2: SBTypeNameSpecifier, arg3: SBTypeSummary) -> bool:
        ...

    def DeleteTypeSummary(self, arg2: SBTypeNameSpecifier) -> bool:
        ...

    def AddTypeFilter(self, arg2: SBTypeNameSpecifier, arg3: SBTypeFilter) -> bool:
        ...

    def DeleteTypeFilter(self, arg2: SBTypeNameSpecifier) -> bool:
        ...

    def AddTypeSynthetic(self, arg2: SBTypeNameSpecifier, arg3: SBTypeSynthetic) -> bool:
        ...

    def DeleteTypeSynthetic(self, arg2: SBTypeNameSpecifier) -> bool:
        ...

    def __str__(self) -> str:
        ...


    def get_formats_array(self) -> List[SBTypeFormat]:
        '''An accessor function that returns a list() that contains all formats in a lldb.SBCategory object.'''
        ...

    def get_summaries_access_object(self) :
        '''An accessor function that returns an accessor object which allows lazy summary access from a lldb.SBTypeCategory object.'''
        ...

    def get_summaries_array(self) -> List[SBTypeSummary]:
        '''An accessor function that returns a list() that contains all summaries in a lldb.SBCategory object.'''
        ...


    def get_synthetics_array(self) -> List[SBTypeSynthetic]:
        '''An accessor function that returns a list() that contains all synthetic children providers in a lldb.SBCategory object.'''
        ...


    def get_filters_array(self) -> List[SBTypeFilter]:
        '''An accessor function that returns a list() that contains all filters in a lldb.SBCategory object.'''
        ...

    formats = property(get_formats_array, None, doc='''A read only property that returns a list() of lldb.SBTypeFormat objects contained in this category''')
    summaries = property(get_summaries_array, None, doc='''A read only property that returns a list() of lldb.SBTypeSummary objects contained in this category''')
    summary = property(get_summaries_access_object, None, doc=r'''A read only property that returns an object that you can use to look for summaries by index or type name or regular expression.''')
    filters = property(get_filters_array, None, doc='''A read only property that returns a list() of lldb.SBTypeFilter objects contained in this category''')
    synthetics = property(get_synthetics_array, None, doc='''A read only property that returns a list() of lldb.SBTypeSynthetic objects contained in this category''')
    num_formats = property(GetNumFormats, None)
    num_summaries = property(GetNumSummaries, None)
    num_filters = property(GetNumFilters, None)
    num_synthetics = property(GetNumSynthetics, None)
    name = property(GetName, None)
    enabled = property(GetEnabled, SetEnabled)




class SBTypeEnumMember:
    r"""Represents a member of an enum in lldb."""

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self, rhs: SBTypeEnumMember):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def GetValueAsSigned(self) -> Int64T:
        ...

    def GetValueAsUnsigned(self) -> UInt64T:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetType(self) -> SBType:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __str__(self) -> str:
        ...

    name = property(GetName, None, doc='''A read only property that returns the name for this enum member as a string.''')
    type = property(GetType, None, doc='''A read only property that returns an lldb object that represents the type (lldb.SBType) for this enum member.''')
    signed = property(GetValueAsSigned, None, doc='''A read only property that returns the value of this enum member as a signed integer.''')
    unsigned = property(GetValueAsUnsigned, None, doc='''A read only property that returns the value of this enum member as a unsigned integer.''')




class SBTypeEnumMemberList:
    r"""
    Represents a list of SBTypeEnumMembers.

    SBTypeEnumMemberList supports SBTypeEnumMember iteration.
    It also supports [] access either by index, or by enum
    element name by doing: ::

      myType = target.FindFirstType('MyEnumWithElementA')
      members = myType.GetEnumMembers()
      first_elem = members[0]
      elem_A = members['A']


    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBTypeEnumMemberList):
        ...
        
    

    def IsValid(self) -> bool:
        ...


    def Append(self, entry: SBTypeEnumMember) -> None:
        ...

    def GetTypeEnumMemberAtIndex(self, index: UInt32T) -> SBTypeEnumMember:
        ...

    def GetSize(self) -> UInt32T:
        ...

    def __iter__(self) -> Iterable[SBTypeEnumMember]:
        '''Iterate over all members in a lldb.SBTypeEnumMemberList object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of members in a lldb.SBTypeEnumMemberList object.'''
        ...

    def __getitem__(self, key) -> Optional[SBTypeEnumMember]:
        ...



class SBTypeFilter:
    r"""Represents a filter that can be associated to one or more types."""

    
    
    @overload
    def __init__(self):
        ...
    @overload
    def __init__(self , options: UInt32T) :
        ...

    @overload
    def __init__(self , rhs: SBTypeFilter):
        ...
    

    def IsValid(self) -> bool:
        ...




    def IsEqualTo(self, rhs: SBTypeFilter) -> bool:
        ...

    def GetNumberOfExpressionPaths(self) -> UInt32T:
        ...

    def GetExpressionPathAtIndex(self, i: UInt32T) -> CharConstStar:
        ...

    def ReplaceExpressionPathAtIndex(self, i: UInt32T, item: CharConstStar) -> bool:
        ...

    def AppendExpressionPath(self, item: CharConstStar) -> None:
        ...

    def Clear(self) -> None:
        ...

    def GetOptions(self) -> UInt32T:
        ...

    def SetOptions(self, arg2: UInt32T) -> None:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __eq__(self, rhs: SBTypeFilter) -> bool:
        ...

    def __ne__(self, rhs: SBTypeFilter) -> bool:
        ...

    def __str__(self) -> str:
        ...

    options = property(GetOptions, SetOptions)
    count = property(GetNumberOfExpressionPaths)


class SBTypeFormat:
    r"""Represents a format that can be associated to one or more types."""

    
    
    @overload
    def __init__(self, format: Format , options : UInt32T = 0):
        ...

    @overload
    def __init__(self, type: CharConstStar , options : UInt32T = 0):
        ...
    @overload
    def __init__(self, rhs : SBTypeFormat):
        ...
    

    def IsValid(self) -> bool:
        ...

    def IsEqualTo(self, rhs: SBTypeFormat) -> bool:
        ...

    def GetFormat(self) -> Format:
        ...

    def GetTypeName(self) -> CharConstStar:
        ...

    def GetOptions(self) -> UInt32T:
        ...

    def SetFormat(self, arg2: Format) -> None:
        ...

    def SetTypeName(self, arg2: CharConstStar) -> None:
        ...

    def SetOptions(self, arg2: UInt32T) -> None:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __eq__(self, rhs: SBTypeFormat) -> bool:
        ...

    def __ne__(self, rhs: SBTypeFormat) -> bool:
        ...

    def __str__(self) -> str:
        ...

    format = property(GetFormat, SetFormat)
    options = property(GetOptions, SetOptions)




class SBTypeNameSpecifier:
    r"""Represents a general way to provide a type name to LLDB APIs."""

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , name : CharConstStar, is_regex= False):
        ...
    @overload
    def __init__(self , type : SBType):
        ...
    @overload
    def __init__(self , rhs: SBTypeNameSpecifier):
        ...
            

    def IsValid(self) -> bool:
        ...

    def IsEqualTo(self, rhs: SBTypeNameSpecifier) -> bool:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetType(self) -> SBType:
        ...

    def IsRegex(self) -> bool:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __eq__(self, rhs: SBTypeNameSpecifier) -> bool:
        ...

    def __ne__(self, rhs: SBTypeNameSpecifier) -> bool:
        ...

    def __str__(self) -> str:
        ...

    name = property(GetName)
    is_regex = property(IsRegex)


class SBTypeSummaryOptions:
    r"""Proxy of C++ lldb::SBTypeSummaryOptions class."""

    
        
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self, rhs: SBTypeSummaryOptions):
        ...
        
    

    def IsValid(self) -> bool:
        ...


    def GetLanguage(self) -> LanguageType:
        ...

    def GetCapping(self) -> TypeSummaryCapping:
        ...

    def SetLanguage(self, arg2: LanguageType) -> None:
        ...

    def SetCapping(self, arg2: TypeSummaryCapping) -> None:
        ...



class SBTypeSummary:
    r"""Represents a summary that can be associated to one or more types."""

    
    

    @staticmethod
    def CreateWithSummaryString(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
        ...

    @staticmethod
    def CreateWithFunctionName(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
        ...

    @staticmethod
    def CreateWithScriptCode(data: CharConstStar, options: UInt32T=0) -> SBTypeSummary:
        ...
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs : SBTypeSummary):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def IsEqualTo(self, rhs: SBTypeSummary) -> bool:
        ...

    def IsFunctionCode(self) -> bool:
        ...

    def IsFunctionName(self) -> bool:
        ...

    def IsSummaryString(self) -> bool:
        ...

    def GetData(self) -> CharConstStar:
        ...

    def SetSummaryString(self, data: CharConstStar) -> None:
        ...

    def SetFunctionName(self, data: CharConstStar) -> None:
        ...

    def SetFunctionCode(self, data: CharConstStar) -> None:
        ...

    def GetOptions(self) -> UInt32T:
        ...

    def SetOptions(self, arg2: UInt32T) -> None:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __eq__(self, rhs: SBTypeSummary) -> bool:
        ...

    def __ne__(self, rhs: SBTypeSummary) -> bool:
        ...

    def __str__(self) -> str:
        ...

    options = property(GetOptions, SetOptions)
    is_summary_string = property(IsSummaryString)
    is_function_name = property(IsFunctionName)
    is_function_name = property(IsFunctionCode)
    summary_data = property(GetData)




class SBTypeSynthetic:
    r"""Represents a summary that can be associated to one or more types."""
    

    @staticmethod
    def CreateWithClassName(data: CharConstStar, options: UInt32T=0) -> SBTypeSynthetic:
        ...

    @staticmethod
    def CreateWithScriptCode(data: CharConstStar, options: UInt32T=0) -> SBTypeSynthetic:
        ...
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBTypeSynthetic):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def IsEqualTo(self, rhs: SBTypeSynthetic) -> bool:
        ...

    def IsClassCode(self) -> bool:
        ...

    def GetData(self) -> CharConstStar:
        ...

    def SetClassName(self, data: CharConstStar) -> None:
        ...

    def SetClassCode(self, data: CharConstStar) -> None:
        ...

    def GetOptions(self) -> UInt32T:
        ...

    def SetOptions(self, arg2: UInt32T) -> None:
        ...

    def GetDescription(self, description: SBStream, description_level: DescriptionLevel) -> bool:
        ...

    def __eq__(self, rhs: SBTypeSynthetic) -> bool:
        ...

    def __ne__(self, rhs: SBTypeSynthetic) -> bool:
        ...

    def __str__(self) -> str:
        ...

    options = property(GetOptions, SetOptions)
    contains_code = property(IsClassCode, None)
    synthetic_data = property(GetData, None)




class SBUnixSignals:
    r"""Allows you to manipulate LLDB's signal disposition"""

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBUnixSignals):
        ...
    

    def Clear(self) -> None:
        ...

    def IsValid(self) -> bool:
        ...



    def GetSignalAsCString(self, signo: Int32T) -> CharConstStar:
        ...

    def GetSignalNumberFromName(self, name: CharConstStar) -> Int32T:
        ...

    def GetShouldSuppress(self, signo: Int32T) -> bool:
        ...

    def SetShouldSuppress(self, signo: Int32T, value: bool) -> bool:
        ...

    def GetShouldStop(self, signo: Int32T) -> bool:
        ...

    def SetShouldStop(self, signo: Int32T, value: bool) -> bool:
        ...

    def GetShouldNotify(self, signo: Int32T) -> bool:
        ...

    def SetShouldNotify(self, signo: Int32T, value: bool) -> bool:
        ...

    def GetNumSignals(self) -> Int32T:
        ...

    def GetSignalAtIndex(self, index: Int32T) -> Int32T:
        ...

    def get_unix_signals_list(self) -> List[ Int32T ]:
        ...

    threads = property(get_unix_signals_list, None, doc='''A read only property that returns a list() of valid signal numbers for this platform.''')




class SBValue:
    r"""
    Represents the value of a variable, a register, or an expression.

    SBValue supports iteration through its child, which in turn is represented
    as an SBValue.  For example, we can get the general purpose registers of a
    frame as an SBValue, and iterate through all the registers,::

        registerSet = frame.registers # Returns an SBValueList.
        for regs in registerSet:
            if 'general purpose registers' in regs.name.lower():
                GPRs = regs
                break

        print('%s (number of children = %d):' % (GPRs.name, GPRs.num_children))
        for reg in GPRs:
            print('Name: ', reg.name, ' Value: ', reg.value)

    produces the output: ::

        General Purpose Registers (number of children = 21):
        Name:  rax  Value:  0x0000000100000c5c
        Name:  rbx  Value:  0x0000000000000000
        Name:  rcx  Value:  0x00007fff5fbffec0
        Name:  rdx  Value:  0x00007fff5fbffeb8
        Name:  rdi  Value:  0x0000000000000001
        Name:  rsi  Value:  0x00007fff5fbffea8
        Name:  rbp  Value:  0x00007fff5fbffe80
        Name:  rsp  Value:  0x00007fff5fbffe60
        Name:  r8  Value:  0x0000000008668682
        Name:  r9  Value:  0x0000000000000000
        Name:  r10  Value:  0x0000000000001200
        Name:  r11  Value:  0x0000000000000206
        Name:  r12  Value:  0x0000000000000000
        Name:  r13  Value:  0x0000000000000000
        Name:  r14  Value:  0x0000000000000000
        Name:  r15  Value:  0x0000000000000000
        Name:  rip  Value:  0x0000000100000dae
        Name:  rflags  Value:  0x0000000000000206
        Name:  cs  Value:  0x0000000000000027
        Name:  fs  Value:  0x0000000000000010
        Name:  gs  Value:  0x0000000000000048

    See also linked_list_iter() for another perspective on how to iterate through an
    SBValue instance which interprets the value object as representing the head of a
    linked list.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBValue):
        ...
        

    def IsValid(self) -> bool:
        ...




    def Clear(self) -> None:
        ...

    def GetError(self) -> SBError:
        ...

    def GetID(self) -> UserIdT:
        ...

    def GetName(self) -> CharConstStar:
        ...

    def GetTypeName(self) -> CharConstStar:
        ...

    def GetDisplayTypeName(self) -> CharConstStar:
        ...

    def GetByteSize(self) -> SizeT:
        ...

    def IsInScope(self) -> bool:
        ...

    def GetFormat(self) -> Format:
        ...

    def SetFormat(self, format: Format) -> None:
        ...

    def GetValue(self) -> CharConstStar:
        ...
    
    @overload
    def GetValueAsSigned(self, error : SBError , fail_value : Int64T = 0) -> Int64T:
        ...

    @overload
    def GetValueAsSigned(self, fail_value : Int64T = 0) -> Int64T:
        ...


    def GetValueAsUnsigned(self, *args) -> UInt64T:
        r"""
        GetValueAsUnsigned(SBValue self, SBError error, uint64_t fail_value=0) -> uint64_t
        GetValueAsUnsigned(SBValue self, uint64_t fail_value=0) -> uint64_t
        """
        ...

    def GetValueType(self) -> ValueType:
        ...

    def GetValueDidChange(self) -> bool:
        ...
        
    @overload
    def GetSummary(self) -> CharConst:
        ...

    @overload
    def GetSummary(self , stream : SBStream , options:  SBTypeSummaryOptions) -> CharConstStar:
        ...
        

    def GetObjectDescription(self) -> CharConstStar:
        ...

    def GetDynamicValue(self, use_dynamic: DynamicValueType) -> SBValue:
        ...

    def GetStaticValue(self) -> SBValue:
        ...

    def GetNonSyntheticValue(self) -> SBValue:
        ...

    def GetPreferDynamicValue(self) -> DynamicValueType:
        ...

    def SetPreferDynamicValue(self, use_dynamic: DynamicValueType) -> None:
        ...

    def GetPreferSyntheticValue(self) -> bool:
        ...

    def SetPreferSyntheticValue(self, use_synthetic: bool) -> None:
        ...

    def IsDynamic(self) -> bool:
        ...

    def IsSynthetic(self) -> bool:
        ...

    def IsSyntheticChildrenGenerated(self) -> bool:
        ...

    def SetSyntheticChildrenGenerated(self, arg2: bool) -> None:
        ...

    def GetLocation(self) -> CharConstStar:
        ...
    

    def SetValueFromCString(self, value_str : CharConstStar , error  :SBError) -> bool:
        ...

    def GetTypeFormat(self) -> SBTypeFormat:
        ...

    def GetTypeSummary(self) -> SBTypeSummary:
        ...

    def GetTypeFilter(self) -> SBTypeFilter:
        ...

    def GetTypeSynthetic(self) -> SBTypeSynthetic:
        ...

    def GetChildAtIndex(self, idx: UInt32T , use_dynamic: DynamicValueType = None, can_create_synthetic: bool = None) -> SBValue:
        r"""
            Get a child value by index from a value.

            Structs, unions, classes, arrays and pointers have child
            values that can be access by index.

            Structs and unions access child members using a zero based index
            for each child member. For

            Classes reserve the first indexes for base classes that have
            members (empty base classes are omitted), and all members of the
            current class will then follow the base classes.

            Pointers differ depending on what they point to. If the pointer
            points to a simple type, the child at index zero
            is the only child value available, unless synthetic_allowed
            is true, in which case the pointer will be used as an array
            and can create 'synthetic' child values using positive or
            negative indexes. If the pointer points to an aggregate type
            (an array, class, union, struct), then the pointee is
            transparently skipped and any children are going to be the indexes
            of the child values within the aggregate type. For example if
            we have a 'Point' type and we have a SBValue that contains a
            pointer to a 'Point' type, then the child at index zero will be
            the 'x' member, and the child at index 1 will be the 'y' member
            (the child at index zero won't be a 'Point' instance).

            If you actually need an SBValue that represents the type pointed
            to by a SBValue for which GetType().IsPointeeType() returns true,
            regardless of the pointee type, you can do that with the SBValue.Dereference
            method (or the equivalent deref property).

            Arrays have a preset number of children that can be accessed by
            index and will returns invalid child values for indexes that are
            out of bounds unless the synthetic_allowed is true. In this
            case the array can create 'synthetic' child values for indexes
            that aren't in the array bounds using positive or negative
            indexes.

            @param[in] idx
                The index of the child value to get

            @param[in] use_dynamic
                An enumeration that specifies whether to get dynamic values,
                and also if the target can be run to figure out the dynamic
                type of the child value.

            @param[in] synthetic_allowed
                If true, then allow child values to be created by index
                for pointers and arrays for indexes that normally wouldn't
                be allowed.

            @return
                A new SBValue object that represents the child member value.
        """
        ...

    def CreateChildAtOffset(self, name: CharConstStar, offset: UInt32T, type: SBType) -> SBValue:
        ...

    def Cast(self, type: SBType) -> SBValue:
        ...

    def CreateValueFromExpression(self, name: CharConstStar , expression: CharConstStar , options : SBExpressionOptions = None) -> SBValue:
        ...

    def CreateValueFromAddress(self, name: CharConstStar, address: AddrT, type: SBType) -> SBValue:
        ...

    def CreateValueFromData(self, name: CharConstStar, data: SBData, type: SBType) -> SBValue:
        ...

    def GetType(self) -> SBType:
        ...

    def GetIndexOfChildWithName(self, name: CharConstStar) -> UInt32T:
        r"""
            Returns the child member index.

            Matches children of this object only and will match base classes and
            member names if this is a clang typed object.

            @param[in] name
                The name of the child value to get

            @return
                An index to the child member value.
        """
        ...

    def GetChildMemberWithName(self, name  : CharConstStar , use_dynamic: DynamicValueType = None) -> SBValue:
        r"""
            Returns the child member value.

            Matches child members of this object and child members of any base
            classes.

            @param[in] name
                The name of the child value to get

            @param[in] use_dynamic
                An enumeration that specifies whether to get dynamic values,
                and also if the target can be run to figure out the dynamic
                type of the child value.

            @return
                A new SBValue object that represents the child member value.
        """
        ...

    def GetValueForExpressionPath(self, expr_path: CharConstStar) -> SBValue:
        r"""
        Expands nested expressions like .a->b[0].c[1]->d.
        """
        ...

    def GetDeclaration(self) -> SBDeclaration:
        ...

    def MightHaveChildren(self) -> bool:
        ...

    def IsRuntimeSupportValue(self) -> bool:
        ...

    def GetNumChildren(self, max : UInt32T = None) -> UInt32T:
        ...

    def GetOpaqueType(self) -> VoidStar:
        ...

    def Dereference(self) -> SBValue:
        ...

    def AddressOf(self) -> SBValue:
        ...

    def TypeIsPointerType(self) -> bool:
        ...

    def GetTarget(self) -> SBTarget:
        ...

    def GetProcess(self) -> SBProcess:
        ...

    def GetThread(self) -> SBThread:
        ...

    def GetFrame(self) -> SBFrame:
        ...

    def Watch(self, resolve_location: bool, read: bool, write: bool, error: SBError) -> SBWatchpoint:
        r"""
            Find and watch a variable.
            It returns an SBWatchpoint, which may be invalid.
        """
        ...

    def WatchPointee(self, resolve_location: bool, read: bool, write: bool, error: SBError) -> SBWatchpoint:
        r"""
            Find and watch the location pointed to by a variable.
            It returns an SBWatchpoint, which may be invalid.
        """
        ...

    def GetDescription(self, description: SBStream) -> bool:
        ...

    def GetPointeeData(self, item_idx: UInt32T=0, item_count: UInt32T=1) -> SBData:
        r"""
            Get an SBData wrapping what this SBValue points to.

            This method will dereference the current SBValue, if its
            data type is a ``T\*`` or ``T[]``, and extract ``item_count`` elements
            of type ``T`` from it, copying their contents in an :py:class:`SBData`.

            :param item_idx: The index of the first item to retrieve. For an array
                this is equivalent to array[item_idx], for a pointer
                to ``\*(pointer + item_idx)``. In either case, the measurement
                unit for item_idx is the ``sizeof(T)`` rather than the byte
            :param item_count: How many items should be copied into the output. By default
                only one item is copied, but more can be asked for.
            :return: The contents of the copied items on success. An empty :py:class:`SBData` otherwise.
            :rtype: SBData

        """
        ...

    def GetData(self) -> SBData:
        r"""
            Get an SBData wrapping the contents of this SBValue.

            This method will read the contents of this object in memory
            and copy them into an SBData for future use.

            @return
                An SBData with the contents of this SBValue, on success.
                An empty SBData otherwise.
        """
        ...

    def SetData(self, data: SBData, error: SBError) -> bool:
        ...

    def GetLoadAddress(self) -> AddrT:
        ...

    def GetAddress(self) -> SBAddress:
        ...

    def Persist(self) -> SBValue:
        ...

    def GetExpressionPath(self, description: SBStream , qualify_cxx_base_classes: bool) -> bool:
        r"""
        Returns an expression path for this value.
        """
        ...
    
    def EvaluateExpression(self, expr: CharConstStar , options : SBExpressionOptions = None , name : CharConstStar = None) -> SBValue:
        ...

    def __str__(self) -> str:
        ...

    def __get_dynamic__ (self) :
        '''Helper function for the "SBValue.dynamic" property.'''
        ...

    class children_access:
        '''A helper object that will lazily hand out thread for a process when supplied an index.'''

        def __init__(self, sbvalue):
            self.sbvalue = sbvalue

        def __len__(self) -> int:
            ...

        def __getitem__(self, key) -> Optional[SBValue]:
            ...

    def get_child_access_object(self) :
        '''An accessor function that returns a children_access() object which allows lazy member variable access from a lldb.SBValue object.'''
        ...

    def get_value_child_list(self) -> List[SBValue]:
        '''An accessor function that returns a list() that contains all children in a lldb.SBValue object.'''
        ...

    def __iter__(self) -> Iterable[SBValue]:
        '''Iterate over all child values of a lldb.SBValue object.'''
        ...

    def __len__(self) -> int:
        '''Return the number of child values of a lldb.SBValue object.'''
        ...

    children = property(get_value_child_list, None, doc='''A read only property that returns a list() of lldb.SBValue objects for the children of the value.''')
    child = property(get_child_access_object, None, doc='''A read only property that returns an object that can access children of a variable by index (child_value = value.children[12]).''')
    name = property(GetName, None, doc='''A read only property that returns the name of this value as a string.''')
    type = property(GetType, None, doc='''A read only property that returns a lldb.SBType object that represents the type for this value.''')
    size = property(GetByteSize, None, doc='''A read only property that returns the size in bytes of this value.''')
    is_in_scope = property(IsInScope, None, doc='''A read only property that returns a boolean value that indicates whether this value is currently lexically in scope.''')
    format = property(GetName, SetFormat, doc='''A read/write property that gets/sets the format used for lldb.SBValue().GetValue() for this value. See enumerations that start with "lldb.eFormat".''')
    value = property(GetValue, SetValueFromCString, doc='''A read/write property that gets/sets value from a string.''')
    value_type = property(GetValueType, None, doc='''A read only property that returns an lldb enumeration value (see enumerations that start with "lldb.eValueType") that represents the type of this value (local, argument, global, register, etc.).''')
    changed = property(GetValueDidChange, None, doc='''A read only property that returns a boolean value that indicates if this value has changed since it was last updated.''')
    data = property(GetData, None, doc='''A read only property that returns an lldb object (lldb.SBData) that represents the bytes that make up the value for this object.''')
    load_addr = property(GetLoadAddress, None, doc='''A read only property that returns the load address of this value as an integer.''')
    addr = property(GetAddress, None, doc='''A read only property that returns an lldb.SBAddress that represents the address of this value if it is in memory.''')
    deref = property(Dereference, None, doc='''A read only property that returns an lldb.SBValue that is created by dereferencing this value.''')
    address_of = property(AddressOf, None, doc='''A read only property that returns an lldb.SBValue that represents the address-of this value.''')
    error = property(GetError, None, doc='''A read only property that returns the lldb.SBError that represents the error from the last time the variable value was calculated.''')
    summary = property(GetSummary, None, doc='''A read only property that returns the summary for this value as a string''')
    description = property(GetObjectDescription, None, doc='''A read only property that returns the language-specific description of this value as a string''')
    dynamic = property(__get_dynamic__, None, doc='''A read only property that returns an lldb.SBValue that is created by finding the dynamic type of this value.''')
    location = property(GetLocation, None, doc='''A read only property that returns the location of this value as a string.''')
    target = property(GetTarget, None, doc='''A read only property that returns the lldb.SBTarget that this value is associated with.''')
    process = property(GetProcess, None, doc='''A read only property that returns the lldb.SBProcess that this value is associated with, the returned value might be invalid and should be tested.''')
    thread = property(GetThread, None, doc='''A read only property that returns the lldb.SBThread that this value is associated with, the returned value might be invalid and should be tested.''')
    frame = property(GetFrame, None, doc='''A read only property that returns the lldb.SBFrame that this value is associated with, the returned value might be invalid and should be tested.''')
    num_children = property(GetNumChildren, None, doc='''A read only property that returns the number of child lldb.SBValues that this value has.''')
    unsigned = property(GetValueAsUnsigned, None, doc='''A read only property that returns the value of this SBValue as an usigned integer.''')
    signed = property(GetValueAsSigned, None, doc='''A read only property that returns the value of this SBValue as a signed integer.''')

    def get_expr_path(self):
        s = SBStream()
        self.GetExpressionPath (s)
        return s.GetData()

    path = property(get_expr_path, None, doc='''A read only property that returns the expression path that one can use to reach this value in an expression.''')

    def synthetic_child_from_expression(self, name, expr, options=None):
        if options is None: options = lldb.SBExpressionOptions()
        child = self.CreateValueFromExpression(name, expr, options)
        child.SetSyntheticChildrenGenerated(True)
        return child

    def synthetic_child_from_data(self, name, data, type):
        child = self.CreateValueFromData(name, data, type)
        child.SetSyntheticChildrenGenerated(True)
        return child

    def synthetic_child_from_address(self, name, addr, type):
        child = self.CreateValueFromAddress(name, addr, type)
        child.SetSyntheticChildrenGenerated(True)
        return child

    def __eol_test(val):
        """Default function for end of list test takes an SBValue object.

        Return True if val is invalid or it corresponds to a null pointer.
        Otherwise, return False.
        """
        if not val or val.GetValueAsUnsigned() == 0:
            return True
        else:
            return False

    # ==================================================
    # Iterator for lldb.SBValue treated as a linked list
    # ==================================================
    def linked_list_iter(self, next_item_name, end_of_list_test=__eol_test):
        """Generator adaptor to support iteration for SBValue as a linked list.

        linked_list_iter() is a special purpose iterator to treat the SBValue as
        the head of a list data structure, where you specify the child member
        name which points to the next item on the list and you specify the
        end-of-list test function which takes an SBValue for an item and returns
        True if EOL is reached and False if not.

        linked_list_iter() also detects infinite loop and bails out early.

        The end_of_list_test arg, if omitted, defaults to the __eol_test
        function above.

        For example,

    # Get Frame #0.
        ...

    # Get variable 'task_head'.
        task_head = frame0.FindVariable('task_head')
        ...

        for t in task_head.linked_list_iter('next'):
            print t
        """
        if end_of_list_test(self):
            return
        item = self
        visited = set()
        try:
            while not end_of_list_test(item) and not item.GetValueAsUnsigned() in visited:
                visited.add(item.GetValueAsUnsigned())
                yield item
    # Prepare for the next iteration.
                item = item.GetChildMemberWithName(next_item_name)
        except:
    # Exception occurred.  Stop the generator.
            pass

        return




class SBValueList:
    r"""
    Represents a collection of SBValues.  Both :py:class:`SBFrame.GetVariables()` and
    :py:class:`SBFrame.GetRegisters()` return a SBValueList.

    SBValueList supports :py:class:`SBValue` iteration. For example (from test/lldbutil.py),::

        def get_registers(frame, kind):
            '''Returns the registers given the frame and the kind of registers desired.

            Returns None if there's no such kind.
            '''
            registerSet = frame.GetRegisters() # Return type of SBValueList.
            for value in registerSet:
                if kind.lower() in value.GetName().lower():
                    return value

            return None

        def get_GPRs(frame):
            '''Returns the general purpose registers of the frame as an SBValue.

            The returned SBValue object is iterable.  An example:
                ...
                from lldbutil import get_GPRs
                regs = get_GPRs(frame)
                for reg in regs:
                    print('%s => %s' % (reg.GetName(), reg.GetValue()))
                ...
            '''
            return get_registers(frame, 'general purpose')

        def get_FPRs(frame):
            '''Returns the floating point registers of the frame as an SBValue.

            The returned SBValue object is iterable.  An example:
                ...
                from lldbutil import get_FPRs
                regs = get_FPRs(frame)
                for reg in regs:
                    print('%s => %s' % (reg.GetName(), reg.GetValue()))
                ...
            '''
            return get_registers(frame, 'floating point')

        def get_ESRs(frame):
            '''Returns the exception state registers of the frame as an SBValue.

            The returned SBValue object is iterable.  An example:
                ...
                from lldbutil import get_ESRs
                regs = get_ESRs(frame)
                for reg in regs:
                    print('%s => %s' % (reg.GetName(), reg.GetValue()))
                ...
            '''
            return get_registers(frame, 'exception state')

    """

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBValueList):
        ...
        
    

    def IsValid(self) -> bool:
        ...



    def Clear(self) -> None:
        ...
    
    @overload
    def Append(self, val_obj : SBValue) -> None:
        ...

    @overload
    def Append(self, value_list : SBValueList) -> None:
        ...


    def GetSize(self) -> UInt32T:
        ...

    def GetValueAtIndex(self, idx: UInt32T) -> SBValue:
        ...

    def FindValueObjectByUID(self, uid: UserIdT) -> SBValue:
        ...

    def GetFirstValueByName(self, name: CharConstStar) -> SBValue:
        ...

    def __str__(self) -> str:
        ...

    def __iter__(self) -> Iterable[SBValue]:
        '''Iterate over all values in a lldb.SBValueList object.'''
        ...

    def __len__(self) -> int:
        ...

    def __getitem__(self, key):
        count = len(self)
    #------------------------------------------------------------
    # Access with int to get Nth item in the list
    #------------------------------------------------------------
        if type(key) is int:
            if key < count:
                return self.GetValueAtIndex(key)
    #------------------------------------------------------------
    # Access with "str" to get values by name
    #------------------------------------------------------------
        elif type(key) is str:
            matches = []
            for idx in range(count):
                value = self.GetValueAtIndex(idx)
                if value.name == key:
                    matches.append(value)
            return matches
    #------------------------------------------------------------
    # Match with regex
    #------------------------------------------------------------
        elif isinstance(key, type(re.compile('.'))):
            matches = []
            for idx in range(count):
                value = self.GetValueAtIndex(idx)
                re_match = key.search(value.name)
                if re_match:
                    matches.append(value)
            return matches





class SBVariablesOptions:
    r"""Describes which variables should be returned from :py:class:`SBFrame.GetVariables`."""

    
    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBVariablesOptions):
        ...
        
    

    def IsValid(self) -> bool:
        ...


    def GetIncludeArguments(self) -> bool:
        ...

    def SetIncludeArguments(self, arg2: bool) -> None:
        ...

    def GetIncludeRecognizedArguments(self, arg2: SBTarget) -> bool:
        ...

    def SetIncludeRecognizedArguments(self, arg2: bool) -> None:
        ...

    def GetIncludeLocals(self) -> bool:
        ...

    def SetIncludeLocals(self, arg2: bool) -> None:
        ...

    def GetIncludeStatics(self) -> bool:
        ...

    def SetIncludeStatics(self, arg2: bool) -> None:
        ...

    def GetInScopeOnly(self) -> bool:
        ...

    def SetInScopeOnly(self, arg2: bool) -> None:
        ...

    def GetIncludeRuntimeSupportValues(self) -> bool:
        ...

    def SetIncludeRuntimeSupportValues(self, arg2: bool) -> None:
        ...

    def GetUseDynamic(self) -> DynamicValueType:
        ...

    def SetUseDynamic(self, arg2: DynamicValueType) -> None:
        ...



class SBWatchpoint:
    r"""
    Represents an instance of watchpoint for a specific target program.

    A watchpoint is determined by the address and the byte size that resulted in
    this particular instantiation.  Each watchpoint has its settable options.

    See also :py:class:`SBTarget.watchpoint_iter()` for example usage of iterating through the
    watchpoints of the target.
    """

    
    
    @overload
    def __init__(self):
        ...

    @overload
    def __init__(self , rhs: SBWatchpoint):
        ...
    

    def IsValid(self) -> bool:
        ...




    def __eq__(self, rhs: SBWatchpoint) -> bool:
        ...

    def __ne__(self, rhs: SBWatchpoint) -> bool:
        ...

    def GetError(self) -> SBError:
        ...

    def GetID(self) -> WatchIdT:
        ...

    def GetHardwareIndex(self) -> Int32T:
        r"""
            With -1 representing an invalid hardware index.
        """
        ...

    def GetWatchAddress(self) -> AddrT:
        ...

    def GetWatchSize(self) -> SizeT:
        ...

    def SetEnabled(self, enabled: bool) -> None:
        ...

    def IsEnabled(self) -> bool:
        ...

    def GetHitCount(self) -> UInt32T:
        ...

    def GetIgnoreCount(self) -> UInt32T:
        ...

    def SetIgnoreCount(self, n: UInt32T) -> None:
        ...

    def GetCondition(self) -> CharConstStar:
        r"""
            Get the condition expression for the watchpoint.
        """
        ...

    def SetCondition(self, condition: CharConstStar) -> None:
        r"""
            The watchpoint stops only if the condition expression evaluates to true.
        """
        ...

    def GetDescription(self, description: SBStream, level: DescriptionLevel) -> bool:
        ...

    @staticmethod
    def EventIsWatchpointEvent(event: SBEvent) -> bool:
        ...

    @staticmethod
    def GetWatchpointEventTypeFromEvent(event: SBEvent) -> WatchpointEventType:
        ...

    @staticmethod
    def GetWatchpointFromEvent(event: SBEvent) -> SBWatchpoint:
        ...

    def __str__(self) -> str:
        ...




class declaration:
    '''A class that represents a source declaration location with file, line and column.'''
    def __init__(self, file, line, col):
        self.file = file
        self.line = line
        self.col = col

class value_iter:
    '''Allows iterating over the children of an :py:class:`SBValue`.'''
    def __iter__(self):
        return self

    def __next__(self):
        if self.index >= self.length:
            raise StopIteration()
        child_sbvalue = self.sbvalue.GetChildAtIndex(self.index)
        self.index += 1
        return value(child_sbvalue)

    def next(self):
        return self.__next__()

    def __init__(self,value):
        self.index = 0
        self.sbvalue = value
        if type(self.sbvalue) is value:
            self.sbvalue = self.sbvalue.sbvalue
        self.length = self.sbvalue.GetNumChildren()

class value:
    '''Wraps :py:class:`SBValue` objects so the resulting object can be used as a variable would be in code.

    So if you have a Point structure variable in your code in the current frame named "pt",
    you can initialize an instance of this class with it: ::

        pt = lldb.value(lldb.frame.FindVariable("pt"))
        print pt
        print pt.x
        print pt.y

        pt = lldb.value(lldb.frame.FindVariable("rectangle_array"))
        print rectangle_array[12]
        print rectangle_array[5].origin.x'''
    def __init__(self, sbvalue):
        self.sbvalue = sbvalue

    def __nonzero__(self):
        return self.sbvalue.__nonzero__()

    def __bool__(self):
        return self.sbvalue.__bool__()

    def __str__(self):
        return self.sbvalue.__str__()

    def __getitem__(self, key):
# Allow array access if this value has children...
        if type(key) is value:
            key = int(key)
        if type(key) is int:
            child_sbvalue = (self.sbvalue.GetValueForExpressionPath("[%i]" % key))
            if child_sbvalue and child_sbvalue.IsValid():
                return value(child_sbvalue)
            raise IndexError("Index '%d' is out of range" % key)
        raise TypeError("No array item of type %s" % str(type(key)))

    def __iter__(self):
        return value_iter(self.sbvalue)

    def __getattr__(self, name):
        child_sbvalue = self.sbvalue.GetChildMemberWithName (name)
        if child_sbvalue and child_sbvalue.IsValid():
            return value(child_sbvalue)
        raise AttributeError("Attribute '%s' is not defined" % name)

    def __add__(self, other):
        return int(self) + int(other)

    def __sub__(self, other):
        return int(self) - int(other)

    def __mul__(self, other):
        return int(self) * int(other)

    def __floordiv__(self, other):
        return int(self) // int(other)

    def __mod__(self, other):
        return int(self) % int(other)

    def __divmod__(self, other):
        return int(self) % int(other)

    def __pow__(self, other):
        return int(self) ** int(other)

    def __lshift__(self, other):
        return int(self) << int(other)

    def __rshift__(self, other):
        return int(self) >> int(other)

    def __and__(self, other):
        return int(self) & int(other)

    def __xor__(self, other):
        return int(self) ^ int(other)

    def __or__(self, other):
        return int(self) | int(other)

    def __div__(self, other):
        return int(self) / int(other)

    def __truediv__(self, other):
        return int(self) / int(other)

    def __iadd__(self, other):
        result = self.__add__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __isub__(self, other):
        result = self.__sub__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __imul__(self, other):
        result = self.__mul__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __idiv__(self, other):
        result = self.__div__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __itruediv__(self, other):
        result = self.__truediv__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ifloordiv__(self, other):
        result =  self.__floordiv__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __imod__(self, other):
        result =  self.__and__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ipow__(self, other):
        result = self.__pow__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ipow__(self, other, modulo):
        result = self.__pow__(self, other, modulo)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ilshift__(self, other):
        result = self.__lshift__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __irshift__(self, other):
        result =  self.__rshift__(other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __iand__(self, other):
        result =  self.__and__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ixor__(self, other):
        result =  self.__xor__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __ior__(self, other):
        result =  self.__ior__(self, other)
        self.sbvalue.SetValueFromCString (str(result))
        return result

    def __neg__(self):
        return -int(self)

    def __pos__(self):
        return +int(self)

    def __abs__(self):
        return abs(int(self))

    def __invert__(self):
        return ~int(self)

    def __complex__(self):
        return complex (int(self))

    def __int__(self):
        ...

    def __long__(self):
        return self.__int__()

    def __float__(self):
        return float (self.sbvalue.GetValueAsSigned())

    def __oct__(self):
        return '0%o' % self.sbvalue.GetValueAsUnsigned()

    def __hex__(self):
        return '0x%x' % self.sbvalue.GetValueAsUnsigned()

    def __len__(self):
        return self.sbvalue.GetNumChildren()

    def __eq__(self, other):
        if type(other) is int:
                return int(self) == other
        elif type(other) is str:
                return str(self) == other
        elif type(other) is value:
                self_err = SBError()
                other_err = SBError()
                self_val = self.sbvalue.GetValueAsUnsigned(self_err)
                if self_err.fail:
                        raise ValueError("unable to extract value of self")
                other_val = other.sbvalue.GetValueAsUnsigned(other_err)
                if other_err.fail:
                        raise ValueError("unable to extract value of other")
                return self_val == other_val
        raise TypeError("Unknown type %s, No equality operation defined." % str(type(other)))

    def __ne__(self, other):
        return not self.__eq__(other)



class SBSyntheticValueProvider:
    def __init__(self,valobj):
        pass

    def num_children(self):
        return 0

    def get_child_index(self,name):
        return None

    def get_child_at_index(self,idx):
        return None

    def update(self):
        pass

    def has_children(self):
        return False


#####
#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

#########################

set_type_cache = {}
set_pattern: re.Pattern = re.compile("tySet_(.+)")


def return_exception( func ):

    def wrapper(*args, **kwargs):

        try:

            s = func(*args, **kwargs)    
            return str( s )
        
        except  Exception as e:
            return str( repr(e) ) 

    return wrapper


@return_exception
def set_type_name( valobj: SBValue ) -> str: 
    name = valobj.GetType().GetName()

    if name in set_type_cache:
        return set_type_cache[name]
    
    match = set_pattern.match(name)
    if match:
        set_type_cache[name] = match.group(1)
        return match.group(1)
    else:
        return "???"

@return_exception
def format_ni8(valobj : SBValue):
    s = '?'
    val = valobj.GetValueAsSigned();
    s = str(val)  + " (" + hex(val & 0xFF) + ")"

@return_exception
def format_ni16(valobj : SBValue ):

    val = valobj.GetValueAsSigned();
    s = str(val)  + " (" + hex(val & 0xFFFF) + ")"

    return s

@return_exception
def format_ni32(valobj : SBValue):
    val = valobj.GetValueAsSigned();
    s = str(val)  + " (" + hex(val & 0xFFFFFF) + ")"
    return s

@return_exception
def format_ni64(valobj : SBValue):
    return format_ni32(valobj)

@return_exception
def format_nu(valobj : SBValue):
    s = '?'
    val = valobj.GetValueAsUnsigned();
    s = str(val)
    return s

@return_exception
def format_unsigned( value: NimValue):
    valobj = value.valobj
    name = valobj.GetType().GetName()

    if name == "NI8":
        return format_ni8(valobj)
    elif name == "NI16":
        return format_ni16(valobj)
    elif name == "NI32":
        return format_ni32(valobj)
    elif name == "NI64":
        return format_ni64(valobj)

@return_exception
def format_signed( value: NimValue):
    valobj = value.valobj
    return format_nu( valobj) + " ±"


def enum_from_name_value_target( name: str, value: int, target: SBTarget ) -> str:
    symbol_context_list = target.FindSymbols(name)    

    for symbol_context in symbol_context_list:
        symbol = symbol_context.GetSymbol()
        
        start_addr = symbol.GetStartAddress()

        file_addr = start_addr.GetFileAddress()
        t_nim_type_ptr = hex(file_addr)
        expr = 'reprEnum(' + str(value) + ", (TNimType *)" + t_nim_type_ptr + ')'
        options = lldb.SBExpressionOptions()
        
        expr_value = target.EvaluateExpression(expr) 
        if (expr_value.error.fail):
            return "error"
        else:


            nonSynth = expr_value.GetNonSyntheticValue()
            data = nonSynth.GetChildMemberWithName('data').AddressOf()
            data.SetFormat(lldb.eFormatCString)
            s = data.GetValue()
            s = str(s).replace("\"" , "")
            return f"⚐ {s}"
            
    
@return_exception
def _format_dict( d : dict ) -> str:
    
    result = ", ".join( map( _format_dict_items, d.items() ) )     
    return "{ " + result + " }"
    
    
@return_exception
def _format_dict_items( t ):
    name , value = t
    return name + ":" + str(value)

@return_exception
def all_symbols( value: SBTarget):
    
    a = []
    for module in value.module_iter():
        for symbol in module:
            a.append( str( symbol))
    return a

def enum_name( klass : NimClass ) -> str:
    id = klass.qualifier + "_"
    return "NTI" +  klass.name.lower() + "__" + id 
    
@return_exception
def format_enum(value : NimValue):
    
    valobj = value.valobj
    
    
    target = valobj.GetTarget()
    t_nim_type = enum_name( value.klass )
    val = valobj.GetValueAsUnsigned()
    
    # each enum is a type so get the type name
    
    # get the address of the TNimType data for this type_name 
    # if the type name is tyEnum_MyEnum__OGi9bloluW89a39aqCEVLC1Xw
    # then TNimType will be NTI__OGi9bloluW89a39aqCEVLC1Xw_

    return enum_from_name_value_target( t_nim_type, val, target )
    

@return_exception
def format_set( value: NimValue):
        
    
    valobj = value.valobj    
    element_type = set_type_name( valobj)
    kind = make_class( element_type )
    
    if len( valobj ) == 0:
        return "Set["  + str( kind ) + "]{}"
    

    values = []

    if kind.kind == SummarizableKinds.ENUM:
        for son in valobj:
            
            var = son.GetValueAsUnsigned()
            name = enum_name( kind )
            target = son.GetTarget()
            values.append( enum_from_name_value_target( name, var, target ) )
        
        return "Set["  + str( kind ) + "]{" + ", ".join( values ) + "}"

    for son in valobj:
        
        
        nim_value = make_value( son )
        
        if nim_value in NOT_OBJ_KINDS:
            values.append( str(nim_value) )
        else:
            values.append( str(nim_value.klass.name) )

    
    return "Set["  + str( kind ) + "]{" + ", ".join( values ) + "}"
    

@return_exception        
def format_string(value : NimValue):
    
    valobj = value.valobj
    s = '<error>'
    nonSynth = valobj.GetNonSyntheticValue()
    data = nonSynth.GetChildMemberWithName('data').AddressOf()
    data.SetFormat(lldb.eFormatCString)
    s = data.GetValue()
    return str( s ) 

@return_exception
def format_sequence(value : NimValue):
    valobj = value.valobj
    s = '<error>'
        
    nonSynth = valobj.GetNonSyntheticValue()
    sup = nonSynth.GetChildMemberWithName('Sup')
    len = sup.GetChildMemberWithName('len')
    reserved = sup.GetChildMemberWithName('reserved')
    s = "Seq { len=" + str(len.GetValueAsSigned()) + ", reserved=" + str(reserved.GetValueAsSigned()) + " }"

    return s

@return_exception
def format_tuple( value: NimValue):


    values = []
    for son in value.valobj:
        v = son.GetValue()
        values.append( str( v ))
    return "(" + ", ".join( values ) + ")"
        

@return_exception
def format_array(value : NimValue):
    
    valobj = value.valobj
    s = '<error>'
    arrayType = valobj.GetType()
    elemType = arrayType.GetArrayElementType()
    class_name = str( make_class( elemType.GetName() )  )
    size = int(arrayType.GetByteSize() / elemType.GetByteSize())
    s = f"Array[{class_name}] " + "{ " + str(size) +  " }"

    return s

@return_exception
def format_table(value : NimValue):
    valobj = value.valobj
    s = '<error>'
    nonSynth = valobj.GetNonSyntheticValue()
    counter = nonSynth.GetChildMemberWithName('counter')
    s = "{ counter=" + str(counter.GetValueAsSigned()) + " }"
    return s


@return_exception
def format_obj(value: NimValue):
    valobj = value.valobj
        

    accum = {}
    for son in valobj:
    
        name = son.GetName()

        if name == 'dummy':
            continue

        nim_value = make_value( son )

        if nim_value.klass.kind == SummarizableKinds.PROC:
            continue
        elif nim_value.klass.kind == SummarizableKinds.BOOL:
            accum[name] = str(nim_value)

        elif nim_value.klass.kind in NOT_OBJ_KINDS:
            
            accum[name] = value_formatters[nim_value.klass.kind](nim_value)
        

        else:
            
            if name == 'suggestMode':
                accum[name] = son.GetType().GetName()
            else:
                accum[name] = str(nim_value.klass)

    
    result = _format_dict( accum )
    s = str(value.klass.name) + ":" + result
    return  s

@return_exception
def format_bool(value: NimValue):
    valobj  = value.valobj
    v = "False" if valobj.GetValueAsSigned() == 0 else "True"
    return str( v )

@return_exception
def format_proc( value: NimValue):
    return "proc"

class SummarizableKinds:
    
    STRING = "String"
    TUPLE = "Tuple"

    BOOL = "Bool"
    PROC = "Proc"
    SEQ = "Sequence"
    SET = "Set"
    ARRAY = "Array"
    ENUM = "Enum"
    SIGNED = "Signed"
    UNSIGNED = "Unsigned"
    TABLE = "Table"
    REF_OBJ = "RefObj"
    VAL_OBJ = "ValObj"

class NimClass:
        
    __slots__ = ["name" , "qualifier" , "kind"]

    def __init__(self , name : str , qualifier: str , kind: SummarizableKinds ) -> None:
        self.name = name
        self.qualifier = qualifier
        self.kind = kind
            
    def __str__(self) -> str:

        formatter = kind_formatter.get(self.kind , lambda x: "?")
        return self.name
                    
class NimValue:
    
    __slots__ = ["klass" , "valobj"]

    def __init__(self , valobj : SBValue , klass : NimClass ) -> None:

        self.klass : NimClass = klass
        self.valobj : SBValue = valobj
    
    def __str__(self) -> str:
        kind = self.klass.kind

        formatter = value_formatters.get(kind , lambda x : str( self.klass ) )
        
        return formatter( self )
        
NOT_OBJ_KINDS = ( 
            SummarizableKinds.STRING ,  SummarizableKinds.ENUM,
            SummarizableKinds.SIGNED , SummarizableKinds.UNSIGNED , 
            SummarizableKinds.ARRAY , SummarizableKinds.BOOL)

nim_name_pattern : re.Pattern = re.compile("(?P<prefix>[a-zA-Z]+)_*(?P<name>[a-zA-Z0-9]*)_*(?P<qualifier>[a-zA-Z0-9]*)")
nim_enum_pattern: re.Pattern =  re.compile(r'^tyEnum_(.+?)_([A-Za-z0-9]+)$')

value_formatters = {
SummarizableKinds.STRING : format_string,
SummarizableKinds.SEQ: format_sequence,
SummarizableKinds.ARRAY: format_array,
SummarizableKinds.SIGNED: format_signed,
SummarizableKinds.UNSIGNED: format_unsigned,
SummarizableKinds.TABLE: format_table,
SummarizableKinds.ENUM: format_enum,
SummarizableKinds.REF_OBJ : format_obj,
SummarizableKinds.VAL_OBJ : format_obj,
SummarizableKinds.SET : format_set,
SummarizableKinds.BOOL : format_bool,
SummarizableKinds.PROC : format_proc,
SummarizableKinds.TUPLE : format_tuple,
}            

kind_formatter = {
SummarizableKinds.STRING : lambda x: "[a]",
SummarizableKinds.SEQ : lambda x: "[)",
SummarizableKinds.ARRAY: lambda x: "[]",
SummarizableKinds.ENUM: lambda x: "⚐",
SummarizableKinds.SIGNED: lambda x: "±",
SummarizableKinds.UNSIGNED: lambda x: "U",
SummarizableKinds.TABLE: lambda x: r"{A:B}",
SummarizableKinds.REF_OBJ: lambda x: "{ * }",
SummarizableKinds.VAL_OBJ: lambda x: "{...}",
}

enum_cache: Mapping[str , str] = {}
class_cache : Mapping[str, NimClass] = {}
kind_cache : Mapping[str, SummarizableKinds] = {}

def _make_class_qualifier( type_name : str ) -> str:

    splits = type_name.split('_')

    if len( splits ) <= 2:
        return ""
    
    return splits[-1]
    
def _make_class_name( type_name : str ) -> str:

    if type_name.startswith("NimStringDesc"):
        return "String"

    elif type_name.startswith("NI") or type_name.startswith("NU"):
        return "Integer"
    
    
    match : Optional[re.Match] = nim_name_pattern.search( type_name)

    if match is None:
        return "?" + type_name

    else:
        groups = match.groupdict()
        name = groups.get("name","")

        if "colon" in name:
            name , _ = name.split("colon")

        return name

def make_kind( type_name: str) -> SummarizableKinds:

    if type_name in kind_cache:
        return kind_cache[type_name]

    result : SummarizableKinds
    if type_name.startswith("NimStringDesc"):
        result = SummarizableKinds.STRING
    elif type_name.startswith("tySequence"):
        result = SummarizableKinds.SEQ
    elif type_name.startswith("tyArray"):
        result = SummarizableKinds.ARRAY
    elif type_name.startswith("tySet"):
        result = SummarizableKinds.SET        
    elif type_name.startswith("tyEnum"):
        result = SummarizableKinds.ENUM
    elif type_name.startswith("NI"):
        result = SummarizableKinds.SIGNED
    elif type_name.startswith("NU"):
        result = SummarizableKinds.UNSIGNED
    elif type_name.startswith("tyObject_Table"):
        result = SummarizableKinds.TABLE
    elif type_name.startswith("tyProc"):
        result = SummarizableKinds.PROC
    elif type_name.startswith("tyTuple"):
        result = SummarizableKinds.TUPLE
    elif type_name == "bool":
        result = SummarizableKinds.BOOL
    elif type_name.endswith("*"):
        result = SummarizableKinds.REF_OBJ
    else:
        result = SummarizableKinds.VAL_OBJ
    
    kind_cache[type_name] = result
    return result

def make_class( type_name : str) -> NimClass:

    if type_name in class_cache:
        return class_cache[type_name]

    name = _make_class_name( type_name)
    qualifier = _make_class_qualifier( type_name)
    kind = make_kind( type_name)

    result = NimClass( name = name , 
    qualifier = qualifier , 
    kind = kind )

    class_cache[type_name] = result
    return result

def make_value( valobj: SBValue) -> NimValue:

    type_name  = valobj.GetType().GetName()
    nim_class : NimClass = make_class( type_name )
    nim_value = NimValue( valobj = valobj , klass = nim_class )
    return nim_value


def nim_value_formatter( valobj : SBValue , internal_dict):
        
    return make_value( valobj)
    

class NimSeqProvider:

    def __init__(self, valobj, dict):
        self.valobj = valobj

    def num_children(self):
        try:
            self.count = self.len.GetValueAsUnsigned(0)
            return self.count
        except:
            return None

    def get_child_at_index(self, index):
        try:
            offset = index * self.data_size
            return self.start.CreateChildAtOffset(
                '[' + str(index) + ']', offset, self.data_type)
        except:
            return None

    def get_child_index(self, name):
        try:
            return int(name.lstrip('[').rstrip(']'))
        except:
            return None

    def update(self):
        try:
            data = self.valobj.GetChildMemberWithName('data')
            self.start = data
            self.sup = self.valobj.GetChildMemberWithName('Sup')
            self.len = self.sup.GetChildMemberWithName('len')
            self.reserved = self.sup.GetChildMemberWithName('reserved')
            self.data_type = data.GetType().GetArrayElementType()
            self.data_size = self.data_type.GetByteSize()
        except:
            pass
    
    def has_children(self):
        return True

class NimTableProvider:

    def __init__(self, valobj, dict):
        self.valobj = valobj

    def num_children(self):
        try:
            return len(self.ary)
        except:
            return None

    def get_child_at_index(self, index):
        try:
            return self.ary[index]
        except:
            return None

    def get_child_index(self, name):
        try:
            key = int(name.lstrip('[').rstrip(']'))
            return self.table.get(key)
        except:
            return None

    def num_elem_children(self):
        try:
            self.seqCount = self.len.GetValueAsUnsigned(0)
            if self.seqCount < 0 or self.seqCount > 1000000:
                self.seqCount = 0
            return self.seqCount
        except:
            return None

    def get_elem_at_index(self, index):
        try:
            offset = index * self.data_size
            return self.start.CreateChildAtOffset(
                '[' + str(index) + ']', offset, self.data_type)
        except:
            return None

    def update(self):
        try:
            self.seq = self.valobj.GetChildMemberWithName('data').GetNonSyntheticValue()
            data = self.seq.GetChildMemberWithName('data')
            self.start = data
            self.sup = self.seq.GetChildMemberWithName('Sup')
            self.len = self.sup.GetChildMemberWithName('len')
            self.reserved = self.sup.GetChildMemberWithName('reserved')
            self.data_type = data.GetType().GetArrayElementType()
            self.data_size = self.data_type.GetByteSize()
            self.table = {}
            self.ary = []
            count = self.num_elem_children()
            if count > 1000:
                count = 1000
            for i in range(self.num_elem_children()):
                elem = self.get_elem_at_index(i)
                field0 = elem.GetChildMemberWithName('Field0')
                hash = field0.GetValueAsUnsigned()
                if hash != 0:
                    field1 = elem.GetChildMemberWithName('Field1')
                    key = field1.GetSummary()
                    if key is None:
                        key = str(field1.GetValue())
                    field2 = elem.GetChildMemberWithName('Field2').GetNonSyntheticValue()
                    value = field2.CreateValueFromAddress(key, field2.GetLoadAddress(), field2.GetType())
                    self.table[key] = len(self.ary)
                    self.ary.append(value)
        except:
            pass
    
    def has_children(self):
        return True

class NimObjTableProvider:

    def __init__(self, valobj, dict):
        self.valobj = valobj

    def num_children(self):
        return 0

    def get_child_at_index(self, index):
        return None

    def get_child_index(self, name):
        return None

    def num_elem_children(self):
        return 0

    def get_elem_at_index(self, index):
        return None


    def update(self):
        return None
        try:
            self.seq = self.valobj.GetChildMemberWithName('data').GetNonSyntheticValue()
            data = self.seq.GetChildMemberWithName('data')
            self.start = data
            self.sup = self.seq.GetChildMemberWithName('Sup')
            self.len = self.sup.GetChildMemberWithName('len')
            self.reserved = self.sup.GetChildMemberWithName('reserved')
            self.data_type = data.GetType().GetArrayElementType()
            self.data_size = self.data_type.GetByteSize()
            self.table = {}
            self.ary = []
            count = self.num_elem_children()
            if count > 1000:
                count = 1000
            for i in range(self.num_elem_children()):
                elem = self.get_elem_at_index(i)
                field0 = elem.GetChildMemberWithName('Field0')
                hash = field0.GetValueAsUnsigned()
                if hash != 0:
                    field1 = elem.GetChildMemberWithName('Field1')
                    key = field1.GetSummary()
                    if key is None:
                        key = str(field1.GetValue())
                    field2 = elem.GetChildMemberWithName('Field2').GetNonSyntheticValue()
                    value = field2.CreateValueFromAddress(key, field2.GetLoadAddress(), field2.GetType())
                    self.table[key] = len(self.ary)
                    self.ary.append(value)
        except:
            pass
    
    def has_children(self):
        return False


def __lldb_init_module(debugger : SBDebugger, internal_dict):
    
# to enable logging:
# define lldb.formatters.Logger._lldb_formatters_debug_level to any number greater than 0
# if you define it to any value greater than 1, the log will be automatically flushed after each write (slower but should make sure most of the stuff makes it to the log even if we crash)
# if you define it to any value greater than 2, the calling function's details will automatically be logged (even slower, but provides additional details)
# if you need the log to go to a file instead of on screen, define
# lldb.formatters.Logger._lldb_formatters_debug_filename to a valid
# filename

    lldb.formatters.Logger._lldb_formatters_debug_level = 2
    lldb.formatters.Logger._lldb_formatters_debug_filename = r"C:\Users\Mateus\Desktop\workspace\nimskull\.vscode\log.txt"
    category = debugger.GetDefaultCategory()
    category.SetEnabled(True)

    category.AddTypeSummary(
        lldb.SBTypeNameSpecifier(".+", True), 
        lldb.SBTypeSummary.CreateWithFunctionName("lldbnim.nim_value_formatter"))

    # category.AddTypeSummary(lldb.SBTypeNameSpecifier("NI8"), 
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimNI8_SummaryFormatter'))



    # category.AddTypeSummary(lldb.SBTypeNameSpecifier("^tyEnum_.*$", True), \
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimEnum_SummaryFormatter'))


    # category.AddTypeSummary(lldb.SBTypeNameSpecifier('NimStringDesc'), 
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimStringDesc_SummaryFormatter'))


    # category.AddTypeSummary(
    #     lldb.SBTypeNameSpecifier("^tySequence__.*$", True),
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimSeq_SummaryFormatter'))

    # category.AddTypeSummary(
    #     lldb.SBTypeNameSpecifier("^tyArray__.*$", True),
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimArray_SummaryFormatter'))

    category.AddTypeSynthetic(
        lldb.SBTypeNameSpecifier("^tySequence__.*$", True),
        lldb.SBTypeSynthetic.CreateWithClassName("lldbnim.NimSeqProvider",
                                                 lldb.eTypeOptionCascade))

    category.AddTypeSynthetic(
        lldb.SBTypeNameSpecifier("^tyObject_Table__.*$", True),
        lldb.SBTypeSynthetic.CreateWithClassName("lldbnim.NimTableProvider",
                                                 lldb.eTypeOptionCascade))
    category.AddTypeSynthetic(
        lldb.SBTypeNameSpecifier("^tyObject_StringTable.*$", True),
        lldb.SBTypeSynthetic.CreateWithClassName("lldbnim.NimObjTableProvider",
                                                 lldb.eTypeOptionCascade))

    # category.AddTypeSummary(
    #     lldb.SBTypeNameSpecifier("^tyObject_Table__.*$", True),
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimTable_SummaryFormatter'))
    
    # category.AddTypeSummary(
    #     lldb.SBTypeNameSpecifier("^tyObject_.*$", True),
    #     lldb.SBTypeSummary.CreateWithFunctionName('lldbnim.NimObject_SummaryFormatter') )
        