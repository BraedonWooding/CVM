pub mod instructions;

/// == CVM Instruction Set ==
/// Loosely based off a mixture of Mips and x86
/// Important characteristics
/// - Human readable to a degree (when in printable format)

/// == Registers in CVM ==
/// To support bizarre registers in languages like Mips or x86
/// CVM has 'RegisterConfigs' which just consist of the following properties
/// - Register id (i.e. $31),
/// - Register name (i.e. $ra),
/// - Register size (in bytes)4
/// - Register type
///     Can have one of the following specialisations
///     - 'static' (holds a specific value i.e. $0 in MIPS holds only 0)
///         - readonly, no atomic reads guarantees (since immutable)
///     - 'read' holds a mutable value that is read only, updates can only
///       occur inbetween cycles (thus you get an atomic guarantee in some way)
///       for example a timer would be a 'read' variable.
///       - You don't get a full atomic guarantee because multiple threads may
///         get different values even if they make the call at the same time
///         the guarantee is just simply that it will only update prior to any
///         reads and not during one.
///       - Typically this means the operation is atomic but not necessarily
///         i.e. you could have a threadlocal like read variable that updates
///         periodically after operations this would be non atomic but still
///         guarantees that reads are atomic in retrospect to the value
///     - 'write' holds a write only value, typically only used for things such
///       as a fake screen display.  Writes are guaranteed to be atomic
///       - Proper atomic too (i.e. atomic in terms of threads as well)
///       - The ordering is defined using a property (see below)
///     - 'threadlocal' represents a threadlocal register (i.e. each thread
///       has a unique copy that isn't maintained between them)
///     - 'atomic' represents a shared atomic register between threads
///       guarantees that reads and writes are consistent.
///       - The ordering is defined below by using a property
///     They can also have the following properties (bitset)
///     - memory_order:
///         - 'relaxed', 'consume', 'acquire', 'release', 'acq_rel', 'seq_cst'
///         - default is 'relaxed' (i.e. no constraints)
///     - size:
///         - is an integer
/// Note: for things such as $0 in MIPS or most x86 registers id == name

/// Represents all the instruction opcodes
///
/// Examples
/// - If you want to do *a = *a + *b (where they both are int) it has to be
///   ADD IntFast | PointerDeref a(Deref) b(Deref) # *a = *a + *b
#[allow(non_camel_case_types)]
pub enum InstructionOpcodes {
    /// Nothing; just a skip / placeholder
    NOP = 0,
    /// $1 = $1 + $2
    ADD = 1,
    /// $1 = $1 - $2
    SUB = 2,
    /// $1 = $1 * $2
    MUL = 3,
    /// $1 = $1 / $2
    DIV = 4,
    /// $1 = $1 % $2
    MOD = 5,
    /// $1 = $2
    MOV = 6,
    /// Set a register with a value
    SET = 7,
    /// Branch $1 = $2
    BEQ = 10,
    /// Branch $1 != $2
    BNE = 12,
    /// Branch $1 > $2
    BGT = 13,
    /// Branch $1 >= $2
    BGE = 14,
    /// Branch $1 < $2
    BLT = 15,
    /// Branch $1 <= $2
    BLE = 16,
    /// Unconditional Jump, effectively identical to BEQ 3 0 0
    JMP = 17,
    /// Call a subroutine (i.e. JAL)
    RUN = 18,
    /// Call a foreign function
    FFI = 19,
    /// Returns from a function
    RET = 20,
}

#[allow(non_camel_case_types)]
pub enum RegisterSpecial {
    /// Whatever is in the register is the value
    Constant = 0,
    /// Follow the location inside the register
    Deref = 1,
}

/// Represents a typical $0 argument
/// Effectively indicates what kind of operation we are performing
/// Should be multiples of 4 since the first two bits are for if the value
/// is an immediate (constant) or a pointerderef (i.e. dereference before use)
/// value.
#[allow(non_camel_case_types)]
pub enum ArithmeticKind {
    /// Error technically
    None = 0,
    /// Int32 constant but uses the fastest instruction to compare
    /// them; for example Int32 on x86_32, and Int64 on x86_64
    IntFast = 1,
    /// Represents an Int8 operation
    Int8 = 2,
    /// Represents an Int16 operation
    Int16 = 3,
    /// Represents an Int32 operation
    Int32 = 4,
    /// Represents an Int64 operation
    Int64 = 5,
    /// UInt32 constant but uses the fastest instruction to compare
    /// them; for example UInt32 on x86_32, and UInt64 on x86_64
    UIntFast = 6,
    /// Represents an UInt8 operation
    UInt8 = 7,
    /// Represents an UInt16 operation
    UInt16 = 8,
    /// Represents an UInt32 operation
    UInt32 = 9,
    /// Represents an UInt64 operation
    UInt64 = 10,
    /// Represents a F32 operation
    Float = 11,
    /// Represents a F64 operation
    Double = 12,
    /// Represents a pointer action
    Pointer = 13,
}
