/// File containing debug switches for parts of the interpreter.
//
/// Debugging of lookups on values.
pub const LOOKUP_DEBUG = false;
/// Debugging of lookups on slots objects (noisier than LOOKUP_DEBUG).
pub const SLOTS_LOOKUP_DEBUG = false;
/// Debugging of garbage collections.
pub const GC_DEBUG = false;
/// Debugging of which addresses called track and then didn't untrack the object.
pub const GC_TRACK_SOURCE_DEBUG = false;
/// Debugging of functions related to older generation objects pointing to newer
/// generations.
pub const REMEMBERED_SET_DEBUG = false;
/// Debugging of activation exits (regular and non-local returns).
pub const ACTIVATION_EXIT_DEBUG = false;
/// Dump every executable after codegen.
pub const EXECUTABLE_DUMP_DEBUG = false;
/// Print every instruction that's executed.
pub const EXECUTION_DEBUG = false;
