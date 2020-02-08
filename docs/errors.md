# Errors in Compiler

## User Errors (i.e. syntax/lexical/...)

If any syntax errors occurs it will stop at the syntax error.  This is different to the way most compilers do it and I am open for it to change.

Lexical errors don't stop compilation but do stop any output.

## Internal Errors

I'm very pro internal errors mainly due to this being just me right now so I'm very unsure about most of the code being executed.

However I do have a strict rule in terms of what should produce an error (stopping compilation) and what should produce a warning (and not stop compilation);

If the problem is at all recoverable than a warning should be raised (more importantly an `log::internal_warn`), only if the problem is completely non recoverable should we raise an `log::internal_error` because that fully stops execution.
