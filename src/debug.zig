/// File containing debug switches for parts of the interpreter.
//
/// Debugging of lookups on values.
pub const LOOKUP_DEBUG = false;
/// Debugging of lookups on slots objects (noisier than LOOKUP_DEBUG).
pub const SLOTS_LOOKUP_DEBUG = false;
/// Debugging of garbage collections.
pub const GC_DEBUG = false;
/// Extra debug messages that are very spammy.
pub const GC_SPAMMY_DEBUG = false;
/// Debugging of allocation token requests.
pub const GC_TOKEN_DEBUG = false;
/// Debugging of every single allocation using an AllocationToken.
pub const GC_TOKEN_ALLOCATION_DEBUG = false;
/// Debugging of which addresses called track and then didn't untrack the object.
pub const GC_TRACK_SOURCE_DEBUG = false;
/// Debugging of functions related to older generation objects pointing to newer
/// generations.
pub const REMEMBERED_SET_DEBUG = false;
/// Debugging of activation exits (regular and non-local returns).
pub const ACTIVATION_EXIT_DEBUG = false;
/// Dump every ASTcode executable after astgen.
pub const AST_EXECUTABLE_DUMP_DEBUG = false;
/// Dump every lowcode executable after astgen.
pub const LOW_EXECUTABLE_DUMP_DEBUG = false;
/// Print every instruction that's executed.
pub const EXECUTION_DEBUG = false;
/// Print pushes and pops to VM stacks.
pub const STACK_DEBUG = false;
/// Report when a an empty heap handle slot was found non-optimally.
pub const HEAP_HANDLE_MISS_DEBUG = false;
/// Crash the VM if an out-of-order heap handle untrack happens.
/// i.e. track(A) track(B), then untrack(A) untrack(B).
/// This causes slowdowns in the VM because Heap.allocateHandle has to do more
/// work to find an empty handle slot, so you can use this to track down those
/// cases.
pub const CRASH_ON_OUT_OF_ORDER_HANDLE_FREES = false;
