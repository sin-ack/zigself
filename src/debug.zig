/// File containing debug switches for parts of the interpreter.
//
/// Debugging of lookups on values.
pub const LOOKUP_DEBUG = false;
/// Debugging of lookups on slots objects (noisier than LOOKUP_DEBUG).
pub const SLOTS_LOOKUP_DEBUG = false;
/// Debugging of garbage collections.
pub const GC_DEBUG = false;
/// Debugging of functions related to older generation objects pointing to newer
/// generations.
pub const REMEMBERED_SET_DEBUG = false;
